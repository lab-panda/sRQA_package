library(sRQA)
library(readr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(lme4)
library(lmerTest)
library(broom.mixed) 

options(scipen = 999)

setwd("D:/Projects/hbn/movie_vs_rest")

##############################################################################
# PART 1: IMPORT AND CLEAN
##############################################################################
# Load data
movies <- read_csv("Files/movie_vs_rest_scans_111725.csv")

# Clean up task variable
movies$task <- ifelse(movies$task == "movieDM","movie","rest")

# Make average dorsal attention variables
DorsAttnA_cols <- grep("DorsAttnA", names(movies), value = TRUE)
movies$DorsAttnA_mean <- rowMeans(movies[DorsAttnA_cols], na.rm = TRUE)

DorsAttnB_cols <- grep("DorsAttnB", names(movies), value = TRUE)
movies$DorsAttnB_mean <- rowMeans(movies[DorsAttnB_cols], na.rm = TRUE)

# Match length of movie scan to rest scans
movies_filtered <- movies %>%
  group_by(participant_id, task) %>%
  slice_head(n = 373) %>%
  ungroup()

# Group by participant ID and task
movie_groups <- split(movies_filtered, list(movies_filtered$participant_id, movies_filtered$task))

##############################################################################
# PART 2: SYMBOLIZE
##############################################################################
# Function for symbolic RQA
symbolic_recurrence_analysis <- function(data, column_name, SYMBOLIZATION_METHOD, NUM_SYMBOLS, WINDOW, 
                                         DELAY, EMBEDDING_DIMENSION, OUTPUT) {
  
  # Make symbols
  sym = symbolize_ts(data[[column_name]], 
                     method = SYMBOLIZATION_METHOD,
                     num_symbols = NUM_SYMBOLS,
                     window_size = WINDOW,
                     visualize = OUTPUT)
  
  # Recurrence matrix from symbolic ts
  smat = symbolic_recurrence_matrix(sym, time_delay=DELAY, embedding_dimension=EMBEDDING_DIMENSION)
  
  # RQA on symbolic matrix
  quant_out = data.frame(quant_rqa(smat))
  
  # Add identification to merge later
  quant_out$participant_id = data$participant_id[1]
  quant_out$task= data$task[1]
  
  return(quant_out)
}

# Apply symbolic_recurrence_function to DORSAL B
resultsB <- lapply(movie_groups, function(x) {
  symbolic_recurrence_analysis(x, "DorsAttnB_mean", "quantiles", 5, 5, 1, 1, FALSE)
})

# Get results
DorsAttnB <- do.call(rbind, resultsB)
DorsAttnB$ROI <- "B"

# Z-score 
DorsAttnB[, 1:14] <- scale(DorsAttnB[, 1:14])

############################################################################
# Apply symbolic_recurrence_function to DORSAL A
resultsA <- lapply(movie_groups, function(x) {
  symbolic_recurrence_analysis(x, "DorsAttnA_mean", "quantiles", 5, 5, 1, 1, FALSE)
})

# Get results
DorsAttnA <- do.call(rbind, resultsA)
DorsAttnA$ROI <- "A"

# Z-score
DorsAttnA[, 1:14] <- scale(DorsAttnA[, 1:14])

##############################################################################
# Function for symbolic cross-RQA and symbolic RQA plot
symbolic_cross_recurrence_analysis <- function(data, column_name1, column_name2, SYMBOLIZATION_METHOD, NUM_SYMBOLS, WINDOW, 
                                               DELAY, EMBEDDING_DIMENSION, OUTPUT) {
  
  # Make symbols for the time series
  symbols = symbolize_ts_pair(data[[column_name1]], data[[column_name2]],
                              method = SYMBOLIZATION_METHOD,
                              num_symbols = NUM_SYMBOLS,
                              window_size = WINDOW,
                              visualize = OUTPUT)
  
  # Cross-Recurrence matrix from symbolic ts
  smat = cross_symbolic_recurrence_matrix(symbols$symbols1, symbols$symbols2, 
                                          embedding_dimension = EMBEDDING_DIMENSION, 
                                          time_delay = DELAY)
  
  # CRQA on symbolic matrix
  quant_out = data.frame(quant_crqa(smat))
  
  # Add identification to merge later
  quant_out$participant_id = data$participant_id[1]
  quant_out$task = data$task[1]
  
  return(quant_out)
}

# Apply symbolic_recurrence_function to DORSAL A & B
resultsAB <- lapply(movie_groups, function(x) {
  symbolic_cross_recurrence_analysis(x, "DorsAttnA_mean", "DorsAttnB_mean", "quantiles", 5, 5, 1, 1, FALSE)
})

# Get results
DorsAttnAB <- do.call(rbind, resultsAB)
DorsAttnAB$ROI <- "AB"

# Z-score
DorsAttnAB[, 1:20] <- scale(DorsAttnAB[, 1:20])

##############################################################################
# PART 3: MODEL
##############################################################################
# Factor
# Model A
DorsAttnA$task <- factor(ifelse(DorsAttnA$task == "movie", 1, 0), levels = c(0, 1))
levels(DorsAttnA$task) # Check levels

# Model B
DorsAttnB$task <- factor(ifelse(DorsAttnB$task == "movie", 1, 0), levels = c(0, 1))
levels(DorsAttnB$task) # Check levels

# Model AB
DorsAttnAB$task <- factor(ifelse(DorsAttnAB$task == "movie", 1, 0), levels = c(0, 1))
levels(DorsAttnAB$task) # Check levels

# Function to batch model
run_lmer_analysis <- function(outcome, data, predictor = "task", random_effect = "(1 | participant_id)") {
  # Create the formula
  formula_str <- paste0(outcome, " ~ ", predictor, " + ", random_effect)
  formula_obj <- as.formula(formula_str)
  
  # Fit the model
  model <- lmer(formula_obj, data = data, REML = TRUE)
  
  # Extract fixed effects with confidence intervals
  fixed_effects <- broom.mixed::tidy(model, conf.int = TRUE, effects = "fixed")
  
  # Filter out the intercept
  fixed_effects <- fixed_effects[fixed_effects$term != "(Intercept)", ]
  
  # Add outcome name as a column
  fixed_effects$outcome <- outcome
  
  return(fixed_effects)
}

# Batch model outcomes for sRQA
outcomes_srqa <- c("DET","L","Lmax","DIV","ENTR","LAM","TT","Vmax","VENTR","MRT","RTE","NMPRT","TREND")

###############################################################################
# Model A
modelA <- lapply(outcomes_srqa, function(x) {
  run_lmer_analysis(outcome = x, data = DorsAttnA)
})

# Combine all results into one dataframe
modelA <- do.call(rbind, modelA)
modelA$p.fdr <- p.adjust(modelA$p.value, method = "fdr")
###############################################################################
# Model B
modelB <- lapply(outcomes_srqa, function(x) {
  run_lmer_analysis(outcome = x, data = DorsAttnB)
})

# Combine all results into one dataframe
modelB <- do.call(rbind, modelB)
modelB$p.fdr <- p.adjust(modelB$p.value, method = "fdr")
###############################################################################
# Model AB
# Batch model outcomes for sCRQA
outcomes_scrqa <- c("DET","L","Lmax","DIV","ENTR","LAM","TT","Vmax","VENTR",
                    "MRT1","MRT2","RTE1","RTE2","NMPRT1","NMPRT2",
                    "TREND")
# Model
modelAB <- lapply(outcomes_scrqa, function(x) {
  run_lmer_analysis(outcome = x, data = DorsAttnAB)
})

# Combine all results into one dataframe
modelAB <- do.call(rbind, modelAB)
modelAB$p.fdr <- p.adjust(modelAB$p.value, method = "fdr")

##############################################################################
# PART 4: PLOT
##############################################################################
# Create the ordered differences plot for model A
figA=modelA %>%
  mutate(
    significant = sign(conf.low) == sign(conf.high),
    outcome = reorder(outcome, estimate)
  ) %>%
  
  ggplot(aes(x = estimate, y = outcome, color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.5) +
  scale_color_manual(values = c("gray60", "steelblue"), 
                     labels = c("Non-significant", "Significant")) +
  labs(
    x = "Movie Effect Estimate (95% CI)",
    y = "sRQA Measure",
    color = "",
    title = "Dorsal Attention Network A"
  ) +
  theme_bw() + theme_bw(base_size = 18) + theme(legend.position="none")
print(figA)

# Create the ordered differences plot for model B
figB=modelB %>%
  mutate(
    significant = sign(conf.low) == sign(conf.high),
    outcome = reorder(outcome, estimate)
  ) %>%
  
  ggplot(aes(x = estimate, y = outcome, color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.5) +
  scale_color_manual(values = c("gray60", "steelblue"), 
                     labels = c("Non-significant", "Significant")) +
  labs(
    x = "Movie Effect Estimate (95% CI)",
    y = "sRQA Measure",
    color = "",
    title = "Dorsal Attention Network B"
  ) +
  theme_bw() + theme_bw(base_size = 18) + theme(legend.position="none")
print(figB)

# Create the ordered differences plot for model AB
figAB=modelAB %>%
  mutate(
    significant = sign(conf.low) == sign(conf.high),
    outcome = reorder(outcome, estimate)
  ) %>%
  
  ggplot(aes(x = estimate, y = outcome, color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.5) +
  scale_color_manual(values = c("gray60", "steelblue"), 
                     labels = c("Non-significant", "Significant")) +
  labs(
    x = "Movie Effect Estimate (95% CI)",
    y = "sCRQA Measure",
    color = "",
    title = "Dorsal Attention Network AB"
  ) +
  theme_bw() + theme_bw(base_size = 18) + theme(legend.position="none")
print(figAB)

# ggsave("figs/orderedA.pdf",plot=figA,width=6,height=6,dpi=300,device=cairo_pdf)
# ggsave("figs/orderedB.pdf",plot=figB,width=6,height=6,dpi=300,device=cairo_pdf)
# ggsave("figs/orderedAB.pdf",plot=figAB,width=6,height=6,dpi=300,device=cairo_pdf)
