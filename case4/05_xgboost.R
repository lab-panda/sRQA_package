# ===============================
# 05_xgboost.R
# restructures data, trains xgboost with DART booster using LOOCV, hyperparameter tunining  using hyperband
# Output: xgboost results, ROC curve 
# ===============================

library(readr)
library(dplyr)
library(tidyr)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(paradox)
library(mlr3misc)
library(pROC)
library(imputeTS)
library(caret)
library(mlr3hyperband)

# ================================
# SET PATHS
# ================================

data_dir <- "."  # UPDATE to your local data directory
results_dir <- file.path(data_dir, "results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

srqa_data <- read_csv("C:/Users/etm2132/Desktop/projects/mu3d_sequential/deception_results_srqa/with_pause/deception_srqa_with_pause_1120_final.csv")

# create labels for valence (neg/pos)
srqa_data <- srqa_data %>%
  mutate(valence_label = ifelse(Valence == 0, "neg", "pos")) %>%
  filter(!is.na(Valence))  # Remove rows with missing Valence (but actually none) 

# pivot wider - separate columns for each valence. instead of 4 rows per person, each person has 2 rows 
# 1 row with +, 1 with negative. the rows differ in truth vs lie 
srqa_wide <- srqa_data %>%
  pivot_wider(
    id_cols = c(participant_ID, Veracity, Sex, Race),
    names_from = valence_label,
    values_from = c(n_words, prop_pauses, pause_RR, pause_DET, pause_L, 
                    pause_Lmax, pause_DIV, pause_ENTR, pause_LAM, pause_TT,
                    pause_Vmax, pause_VENTR, pause_MRT, pause_RTE, pause_NMPRT, pause_TREND),
    names_sep = "_"
  )

# create multiplicative features
srqa_wide <- srqa_wide %>%
  mutate(
    n_words_mult = n_words_pos * n_words_neg,
    prop_pauses_mult = prop_pauses_pos * prop_pauses_neg,
    pause_RR_mult = pause_RR_pos * pause_RR_neg,
    pause_DET_mult = pause_DET_pos * pause_DET_neg,
    pause_L_mult = pause_L_pos * pause_L_neg,
    pause_Lmax_mult = pause_Lmax_pos * pause_Lmax_neg,
    pause_DIV_mult = pause_DIV_pos * pause_DIV_neg,
    pause_ENTR_mult = pause_ENTR_pos * pause_ENTR_neg,
    pause_LAM_mult = pause_LAM_pos * pause_LAM_neg,
    pause_TT_mult = pause_TT_pos * pause_TT_neg,
    pause_Vmax_mult = pause_Vmax_pos * pause_Vmax_neg,
    pause_VENTR_mult = pause_VENTR_pos * pause_VENTR_neg,
    pause_MRT_mult = pause_MRT_pos * pause_MRT_neg,
    pause_RTE_mult = pause_RTE_pos * pause_RTE_neg,
    pause_NMPRT_mult = pause_NMPRT_pos * pause_NMPRT_neg,
    pause_TREND_mult = pause_TREND_pos * pause_TREND_neg
  )

# Define features
feature_names <- c(
  "pause_RR_neg", "pause_DET_neg", "pause_L_neg", "pause_Lmax_neg",
  "pause_DIV_neg", "pause_ENTR_neg", "pause_LAM_neg", "pause_TT_neg",
  "pause_Vmax_neg", "pause_VENTR_neg", "pause_MRT_neg", "pause_RTE_neg",
  "pause_NMPRT_neg", "pause_TREND_neg",
  "pause_RR_pos", "pause_DET_pos", "pause_L_pos", "pause_Lmax_pos",
  "pause_DIV_pos", "pause_ENTR_pos", "pause_LAM_pos", "pause_TT_pos",
  "pause_Vmax_pos", "pause_VENTR_pos", "pause_MRT_pos", "pause_RTE_pos",
  "pause_NMPRT_pos", "pause_TREND_pos",
  "pause_RR_mult", "pause_DET_mult", "pause_L_mult", "pause_Lmax_mult",
  "pause_DIV_mult", "pause_ENTR_mult", "pause_LAM_mult", "pause_TT_mult",
  "pause_Vmax_mult", "pause_VENTR_mult", "pause_MRT_mult", "pause_RTE_mult",
  "pause_NMPRT_mult", "pause_TREND_mult"
)

# Extract features
feature_data <- srqa_wide[, feature_names]

# Extract outcome variable from the dataset
y <- srqa_wide$Veracity
valid_cases <- !is.na(y)

# Filter to valid cases (should be same tho, just safeguard)
feature_data_clean <- feature_data[valid_cases, ]
y_clean <- y[valid_cases]

use <- feature_data_clean %>%
  mutate(Veracity = factor(y[valid_cases], 
                           levels = c(0,1), 
                           labels = c("lie", "truth")))

# ================================
# MLR3 CLASSIFICATION PIPELINE
# ================================

# Define a task - classification task to predict Veracity (before: regression task to predict GE)
veracity_task <- TaskClassif$new(
  id = "veracity_prediction", 
  backend = use, 
  target = "Veracity",
  positive = "truth"  # clarify positive class for metrics
)

# Create a learner - XGBoost for classification (not regression)
veracity_learner <- lrn("classif.xgboost", 
                        predict_type = "prob",  # probabilities for AUC??
                        objective = "binary:logistic",  # Binary classification (not reg) 
                        booster = "dart",
                        nthread = 1)
#lets try the gbtree and gblinear also, but dart best

# Define hyperparameter search space
search_space <- ps(
  nrounds = p_int(lower = 10, upper = 150, tags = "budget"), # Expanded range more than before per email
  eta = p_dbl(lower = 0.001, upper = 0.3), # Learning rate
  alpha = p_dbl(lower=0, upper = 1),
  max_depth = p_int(lower = 2, upper = 10),    # Tree depth
  gamma = p_dbl(lower = 0, upper = 8),
  lambda = p_dbl(lower = 0, upper = 3)
)


# Set up resampling strategy - Leave One Out CV
veracity_loo <- rsmp("loo")

# Define tuning strategy - Random search
# Paul suggests 1000+ iterations in real life: starting with 10 for now
veracity_tuner <- tnr("hyperband", eta =2) #could change to 1 later w higher nevals



# Create tuning instance
veracity_tune_instance <- TuningInstanceSingleCrit$new(
  task = veracity_task, #defined above
  learner = veracity_learner, #defined above 
  resampling = veracity_loo, #defined above
  measure = msr("classif.acc"),  #accuracy optimization 
  search_space = search_space,
  terminator = trm("evals", n_evals = 1000)  # 1000 evaluations
)


# run tuning
veracity_tuner$optimize(veracity_tune_instance)

# view tuning results
cat("Best AUC:", veracity_tune_instance$result_y, "\n")
#best hyperparameters
print(veracity_tune_instance$result_x_domain)

# ================================
# FINAL MODEL WITH TUNED PARAMETERS
# ================================

# create final model with tuned hyperparameters
finmod <- lrn("classif.xgboost", predict_type = "prob")

# set fixed parameters
final_params <- list(
  objective = "binary:logistic",
  booster = "dart")
final_params=append(final_params, veracity_tune_instance$result_x_domain)

# set all parameters at once
finmod$param_set$values <- final_params

# Resample the final model using LOO
rr <- resample(
  task = veracity_task, 
  learner = finmod, 
  resampling = veracity_loo, 
  store_models = TRUE
)

# Extract predictions
predictions <- as.data.table(rr$prediction())

# ================================
# CALCULATE PERFORMANCE METRICS
# ================================

# Get predicted probabilities for "truth" class
predictions$truth <- factor(predictions$truth, levels = c("lie", "truth"))

pred_probs <- predictions$prob.truth #12/8 : changed to 1- to fix the reference label problem 
true_labels <- ifelse(predictions$truth == "truth", 1, 0)
# try convert as factor

# Calculate ROC and AUC
roc_obj <- roc(
  response = true_labels,
  predictor = pred_probs,
  levels = c(0, 1), # first level is lie, second is truth 
  quiet = TRUE)

auc_final <- auc(roc_obj)


#now lets find p values nd spec, sens, ppv, npv
optimal_coords <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"), 
                         best.method = "closest.topleft")

optimal_threshold <- optimal_coords$threshold

# Get predicted classes (using threshold)
pred_classes <- ifelse(pred_probs > optimal_threshold, "truth", "lie")
pred_classes <- factor(pred_classes, levels = c("lie", "truth"))

# create confusion matrix with noinforate test
cm <- confusionMatrix(
  data = pred_classes,
  reference = predictions$truth,
  positive = "truth"
)

print(cm)
auc_final

# ================================
# PANEL B: ROC CURVE VISUALIZATION
# ================================

library(pROC)
library(ggplot2)

# Create ROC plot with ggplot2 (prettier than base plot)
roc_plot <- ggroc(roc_obj, legacy.axes = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +#adds diagonal line: guessing
  annotate("text", x = 0.75, y = 0.25, #add auc to the plot 
           label = paste0("AUC = ", round(auc_final, 3)), 
           size = 5, fontface = "bold") +
  labs(
    title = "ROC Curve: Truth vs. Lie Classification",
    subtitle = "XGBoost with sRQA Pause Features",
    x = "Specificity",
    y = "Sensitivity"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
roc_plot