# Load required libraries
library(sRQA)
library(ggplot2)
library(grid)
library(gridExtra)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(dplyr)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(paradox)
library(mlr3misc)
library(mlr3mbo)
library(mlr3hyperband)
library(pROC)
library(caret)
library(readr)

options(scipen = 999)
set.seed(123)

###############################################################################
# PART 1: Load windowed RR interval files
###############################################################################
get_file_data <- function() {
  setwd("D:/Projects/Physionet/mit-bih-atrial-fibrillation-database-1.0.0/windowed")
  
  all_files <- list.files(".", pattern = "\\.csv$", full.names = FALSE)
  
  ecg_list <- lapply(all_files, function(filename) {
    list(
      filename = filename,
      id = sub("^([^_]+)_.*", "\\1", filename),
      window = sub(".*_window([0-9]+)_.*", "\\1", filename),
      type = sub(".*_(AF|N|O)\\.csv$", "\\1", filename),
      data = read.csv(filename)
    )
  })
  
  return(ecg_list)
}

ecg_list <- get_file_data()

###############################################################################
# PART 2: Symbolic recurrence analysis
###############################################################################
symbolic_recurrence_analysis_error <- function(file_item, column_name, 
                                               SYMBOLIZATION_METHOD, NUM_SYMBOLS, 
                                               WINDOW, DELAY, EMBEDDING_DIMENSION, 
                                               OUTPUT) {
  
  tryCatch({
    print(file_item$filename)
    
    # Make symbols
    sym = symbolize_ts(file_item$data[[column_name]], 
                       method = SYMBOLIZATION_METHOD,
                       num_symbols = NUM_SYMBOLS,
                       window_size = WINDOW,
                       visualize = OUTPUT)
    
    # Recurrence matrix from symbolic ts
    smat = symbolic_recurrence_matrix(sym, 
                                      time_delay = DELAY, 
                                      embedding_dimension = EMBEDDING_DIMENSION)
    
    # RQA on symbolic matrix
    quant_out = data.frame(quant_rqa(smat))
    
    # Add identification variables to merge later
    quant_out$filename = file_item$filename
    quant_out$id = file_item$id
    quant_out$window = file_item$window
    quant_out$type = file_item$type
    
    return(list(quant_out = quant_out))
    
  }, error = function(e) {
    # Log the error
    # Note: Fails when window has too few unique RR values to form distinct quantile bins for symbolization
    message(paste("ERROR in file:", file_item$filename, "-", e$message))
    # Return NULL so we can filter it out later
    return(NULL)
  })
}

# Process all files
results_list <- lapply(ecg_list, function(file_item) {
  symbolic_recurrence_analysis_error(
    file_item = file_item,
    column_name = "rr_interval",
    SYMBOLIZATION_METHOD = "quantiles",
    NUM_SYMBOLS = 5,
    WINDOW = 5,
    DELAY = 1,
    EMBEDDING_DIMENSION = 1,
    OUTPUT = FALSE
  )
})

# Remove NULL results (failed files)
results_list <- results_list[!sapply(results_list, is.null)]

# Extract all quant_out dataframes and combine
quant_out_list <- lapply(results_list, function(x) x$quant_out)
final_results_df <- do.call(rbind, quant_out_list)

# Reset row names
rownames(final_results_df) <- NULL
final_results_df$id <- paste0("ID_", final_results_df$id)

# Prepare data (skip Other "O")
dat <- final_results_df[final_results_df$type != "O", ]
dat[,1:14] <- scale(dat[,1:14]) # Z-score
# Drop RR
dat <- dat[,c(2:18)]

###############################################################################
# PART 3: Mixed effects models
###############################################################################

# Factor type for mixed models
dat$type <- factor(ifelse(dat$type == "AF", 1, 0), levels = c(0, 1))

run_lmer_analysis <- function(outcome, data, predictor = "type", random_effect = "(1 | id)") {
  formula_str <- paste0(outcome, " ~ ", predictor, " + ", random_effect)
  formula_obj <- as.formula(formula_str)
  
  model <- lmer(formula_obj, data = data, REML = TRUE)
  
  fixed_effects <- broom.mixed::tidy(model, conf.int = TRUE, effects = "fixed")
  fixed_effects <- fixed_effects[fixed_effects$term != "(Intercept)", ]
  fixed_effects$outcome <- outcome
  
  return(fixed_effects)
}

# Batch model outcomes for sRQA
outcomes_srqa <- c("DET","L","Lmax","DIV","ENTR","LAM","TT","Vmax","VENTR","MRT","RTE","NMPRT","TREND")

resultsM <- lapply(outcomes_srqa, function(x) {
  run_lmer_analysis(outcome = x, data = dat)
})

resultsM <- do.call(rbind, resultsM)
resultsM$p.fdr <- p.adjust(resultsM$p.value, method = "fdr")

# Forest plot of sRQA effect estimates
fig_forest <- resultsM %>%
  mutate(
    significant = sign(conf.low) == sign(conf.high),
    outcome = reorder(outcome, estimate)
  ) %>%
  ggplot(aes(x = estimate, y = outcome, color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.5) +
  scale_color_manual(values = c("gray60", "steelblue"), 
                     labels = c("Non-significant", "Significant"),
                     breaks = c(FALSE, TRUE)) +
  labs(
    x = "AFib Effect Estimate (95% CI)",
    y = "sRQA Measure",
    color = "",
    title = "Arrhythmyia Patterns in R-R Intervals"
  ) +
  theme_bw(base_size = 18) + theme(legend.position="none") 

print(fig_forest)

###############################################################################
# PART 4: XGBoost classification
# This section shows how the model was originally trained and tuned.
# To reproduce our results, skip to PART 5 and load the saved model and data splits.
###############################################################################

# # Keep only sRQA predictors, drop metadata columns
# dat_xgb <- dat[,-c(14:16)]
# 
# # Encode outcome as factor
# dat_xgb$out <- dat_xgb$type
# dat_xgb <- dat_xgb[,-14] # Drop original type column
# dat_xgb <- data.frame(dat_xgb)
# dat_xgb$out <- factor(dat_xgb$out, levels = c(0, 1))
# 
# # 80/20 train-test split
# train_idx <- sample(c(TRUE, FALSE), nrow(dat_xgb), replace = TRUE, prob = c(0.8, 0.2))
# trainset <- dat_xgb[train_idx,]
# testset <- dat_xgb[!train_idx,]
# 
# # Define binary classification task
# AFtask <- TaskClassif$new(id = "AF_prediction", 
#                        backend = trainset, 
#                        target = "out")
# 
# # Create XGBoost learner
# AFlearner <- lrn("classif.xgboost", 
#                  predict_type = "prob",
#                  booster = "gbtree")
# 
# # Define hyperparameter search space
# search_space <- ps(
#   nrounds = p_int(lower = 100, upper = 800, tags="budget"),
#   eta = p_dbl(lower = 0.1, upper = 0.3),
#   max_depth = p_int(lower = 3, upper = 12),
#   gamma = p_dbl(lower = 0, upper = 15),
#   lambda = p_dbl(lower = 0, upper = 15),
#   alpha = p_dbl(lower = 0, upper = 15)
# )
# 
# # 5-fold cross-validation
# af_cv <- rsmp("cv", folds = 5)
# 
# # Hyperband tuning
# af_tuner <- tnr("hyperband", eta=2)
# 
# # Create tuning instance with AUC as optimization metric
# afib_tune_instance <- TuningInstanceSingleCrit$new(
#   task = AFtask,
#   learner = AFlearner,
#   resampling = af_cv,
#   measure = msr("classif.auc"),  
#   search_space = search_space,
#   terminator = trm("evals", n_evals = 1000)
# )
# 
# # Run hyperparameter tuning
# af_tuner$optimize(afib_tune_instance)
# 
# # Best performance and hyperparameters
# afib_tune_instance$result_y
# afib_tune_instance$result_x_domain
# 
# # Train final model with tuned hyperparameters
# finmod <- lrn("classif.xgboost", predict_type = "prob")
# 
# final_params <- list(booster = "gbtree", nthread=1)
# final_params <- append(final_params, afib_tune_instance$result_x_domain)
# 
# finmod$param_set$values <- final_params
# finmod$train(AFtask)
# 
# # Predict on held-out test set
# predictions <- finmod$predict_newdata(newdata=testset)
# preds <- data.frame(truth=predictions$data$truth, prob=predictions$data$prob)
# 
# # ROC curve
# mroc <- roc(preds$truth, preds$prob.1, print.auc=T)
# fig_roc <- ggroc(mroc, legacy.axes = F, color = "red", size = 1.15) +
#   geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "gray") +
#   annotate("text", x = 0.4, y = 0.1, 
#            label = paste("AUC =", round(auc(mroc), 3))) +
#   labs(title = "Discrimination of Arrhythmia in R-R Intervals",
#        x = "Specificity",
#        y = "Sensitivity") +
#   coord_fixed(ratio = 1, xlim = c(1, 0), ylim = c(0, 1)) +
#   theme_bw(base_size=18)
# 
# print(fig_roc)
# 
# # Confusion matrix
# foracc <- data.frame(predicted=predictions$data$response, truth=predictions$data$truth)
# foracc$predicted <- factor(foracc$predicted, levels=c(0,1))
# foracc$truth <- factor(foracc$truth, levels=c(0,1))
# 
# caret::confusionMatrix(foracc$predicted, foracc$truth, positive="1")
# table(foracc$predicted, foracc$truth)

###############################################################################
# Save model and data splits for reproducibility
###############################################################################
# saveRDS(finmod, "D:/Projects/Physionet/learners/final_model_010826.rds")
# write.csv(testset, "D:/Projects/Physionet/learners/testset_final_model_010826.csv",row.names=F)
# write.csv(trainset, "D:/Projects/Physionet/learners/trainset_final_model_010826.csv",row.names=F)

###############################################################################
# PART 5: Reproduce results from saved model and provided data splits
# Load the trained model and the exact train/test splits we used.
###############################################################################
testset <- read_csv("D:/Projects/Physionet/learners/testset_final_model_010826.csv")
trainset <- read_csv("D:/Projects/Physionet/learners/trainset_final_model_010826.csv")

provemod <- readRDS("D:/Projects/Physionet/learners/final_model_010826.rds")
newpredictions <- provemod$predict_newdata(newdata=testset)

# Extract predictions from saved model
newpreds <- data.frame(truth=newpredictions$data$truth, prob=newpredictions$data$prob)

# ROC curve from saved model
newmroc <- roc(newpreds$truth, newpreds$prob.1, print.auc=T)
fig_roc_saved <- ggroc(newmroc, legacy.axes = F, color = "red", size = 1.15) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "gray") +
  annotate("text", x = 0.4, y = 0.1, 
           label = paste("AUC =", round(auc(newmroc), 3))) +
  labs(title = "Discrimination of Arrhythmia in R-R Intervals",
       x = "Specificity",
       y = "Sensitivity") +
  coord_fixed(ratio = 1, xlim = c(1, 0), ylim = c(0, 1)) +
  theme_bw(base_size=18)

print(fig_roc_saved)

# Confusion matrix from saved model
foracc <- data.frame(predicted=newpredictions$data$response, truth=newpredictions$data$truth)
foracc$predicted <- factor(foracc$predicted, levels=c(0,1))
foracc$truth <- factor(foracc$truth, levels=c(0,1))

caret::confusionMatrix(foracc$predicted, foracc$truth, positive="1")
table(foracc$predicted, foracc$truth)
