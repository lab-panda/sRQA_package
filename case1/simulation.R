library(sRQA)
library(ggplot2)
library(reshape2)
library(gridExtra)

setwd("D:/Projects/sRQA_paper")

###############################################################################
# PART 1: SIMULATION STUDY (100 RUNS)
###############################################################################

n_simulations <- 100
n <- 200  # Length of time series
frequency <- 0.1  # For periodic signal
regime_shift_point <- 100  # Where the shift occurs

# Parameters for symbolization
sim_method <- "rle"
sim_num_symbols <- 6
sim_window_size <- 3
sim_embedding_dimension <- 3
sim_min_line_length <- 2

# Initialize storage for results
sim_all_results <- data.frame(
  sim_id = integer(),
  System = character(),
  RR = numeric(),
  DET = numeric(),
  ENTR = numeric(),
  stringsAsFactors = FALSE
)

cat("Running", n_simulations, "simulations with regime shift analysis...\n")

for(sim in 1:n_simulations) {
  
  set.seed(12345 + sim)
  
  # 1a. Stochastic (White Noise)
  sim_ts_noise <- rnorm(n, mean = 0, sd = 1)
  
  # 1b. Periodic (Sinusoidal with known frequency)
  sim_ts_periodic <- sin(2 * pi * frequency * (1:n)) + rnorm(n, 0, 0.1)
  
  # 1c. Chaotic with regime shift
  # First half: Highly chaotic (r = 3.9)
  sim_ts_chaotic_first <- numeric(regime_shift_point)
  sim_ts_chaotic_first[1] <- runif(1, min = 0.01, max = 0.99)
  for(i in 2:regime_shift_point) {
    sim_ts_chaotic_first[i] <- 3.9 * sim_ts_chaotic_first[i-1] * (1 - sim_ts_chaotic_first[i-1])
  }
  sim_ts_chaotic_first <- scale(sim_ts_chaotic_first, center = TRUE, scale = TRUE)
  
  # Second half: Periodic sine wave
  sim_ts_chaotic_second <- sin(2 * pi * frequency * (1:regime_shift_point)) + rnorm(regime_shift_point, 0, 0.1)
  sim_ts_chaotic_second <- scale(sim_ts_chaotic_second)
  
  # Combine both halves
  sim_ts_chaotic <- as.numeric(rbind(sim_ts_chaotic_first, sim_ts_chaotic_second))
  
  # Symbolize and compute RQA -----------------------------------------------
  
  # Stochastic
  sim_sym_noise <- symbolize_ts(sim_ts_noise, method = sim_method,
                                num_symbols = sim_num_symbols,
                                window_size = sim_window_size, visualize = FALSE)
  sim_rm_noise <- symbolic_recurrence_matrix(sim_sym_noise,
                                             embedding_dimension = sim_embedding_dimension)
  sim_rqa_noise <- quant_rqa(sim_rm_noise, min_line_length = sim_min_line_length)
  
  # Periodic
  sim_sym_periodic <- symbolize_ts(sim_ts_periodic, method = sim_method,
                                   num_symbols = sim_num_symbols,
                                   window_size = sim_window_size, visualize = FALSE)
  sim_rm_periodic <- symbolic_recurrence_matrix(sim_sym_periodic,
                                                embedding_dimension = sim_embedding_dimension)
  sim_rqa_periodic <- quant_rqa(sim_rm_periodic, min_line_length = sim_min_line_length)
  
  # Chaotic (full)
  sim_sym_chaotic <- symbolize_ts(sim_ts_chaotic, method = sim_method,
                                  num_symbols = sim_num_symbols,
                                  window_size = sim_window_size, visualize = FALSE)
  sim_rm_chaotic <- symbolic_recurrence_matrix(sim_sym_chaotic,
                                               embedding_dimension = sim_embedding_dimension)
  sim_rqa_chaotic <- quant_rqa(sim_rm_chaotic, min_line_length = sim_min_line_length)
  
  # Pre-shift and post-shift separately -------------------------------------
  
  sim_ts_pre_shift <- sim_ts_chaotic[1:regime_shift_point]
  sim_ts_post_shift <- sim_ts_chaotic[(regime_shift_point + 1):n]
  
  # Pre-shift (chaotic regime)
  sim_sym_pre <- symbolize_ts(sim_ts_pre_shift, method = sim_method,
                              num_symbols = sim_num_symbols,
                              window_size = sim_window_size, visualize = FALSE)
  sim_rm_pre <- symbolic_recurrence_matrix(sim_sym_pre,
                                           embedding_dimension = sim_embedding_dimension)
  sim_rqa_pre <- quant_rqa(sim_rm_pre, min_line_length = sim_min_line_length)
  
  # Post-shift (periodic regime)
  sim_sym_post <- symbolize_ts(sim_ts_post_shift, method = sim_method,
                               num_symbols = sim_num_symbols,
                               window_size = sim_window_size, visualize = FALSE)
  sim_rm_post <- symbolic_recurrence_matrix(sim_sym_post,
                                            embedding_dimension = sim_embedding_dimension)
  sim_rqa_post <- quant_rqa(sim_rm_post, min_line_length = sim_min_line_length)
  
  # Store results (all 5 cases) ---------------------------------------------
  
  sim_results <- data.frame(
    sim_id = rep(sim, 5),
    System = c("Stochastic", "Periodic", "Chaotic", "Pre-Shift", "Post-Shift"),
    RR   = c(sim_rqa_noise$RR,   sim_rqa_periodic$RR,   sim_rqa_chaotic$RR,   sim_rqa_pre$RR,   sim_rqa_post$RR),
    DET  = c(sim_rqa_noise$DET,  sim_rqa_periodic$DET,  sim_rqa_chaotic$DET,  sim_rqa_pre$DET,  sim_rqa_post$DET),
    ENTR = c(sim_rqa_noise$ENTR, sim_rqa_periodic$ENTR, sim_rqa_chaotic$ENTR, sim_rqa_pre$ENTR, sim_rqa_post$ENTR),
    stringsAsFactors = FALSE
  )
  
  sim_all_results <- rbind(sim_all_results, sim_results)
  
  if(sim %% 10 == 0) cat("Completed", sim, "of", n_simulations, "simulations\n")
}

cat("\nSimulation complete!\n\n")

# Simulation summary stats
sim_mean_values <- aggregate(. ~ System, data = sim_all_results[, -1], FUN = mean)
sim_sd_values   <- aggregate(. ~ System, data = sim_all_results[, -1], FUN = sd)

sim_summary_stats <- merge(sim_mean_values, sim_sd_values, by = "System", suffixes = c("_Mean", "_SD"))
print(sim_summary_stats)

###############################################################################
# PART 2: Visualizations
###############################################################################
# Note values in visualizations are standardized
# Visualize Three Main Systems (Stochastic, Periodic, Chaotic) (fig2, panel a)
sim_main_systems <- sim_all_results[sim_all_results$System %in% c("Stochastic", "Periodic", "Chaotic"), ]
sim_main_long <- melt(sim_main_systems[, c("sim_id", "System", "RR", "DET", "ENTR")],
                      id.vars = c("sim_id", "System"))

sim_main_long$System <- factor(sim_main_long$System,
                               levels = c("Stochastic", "Periodic", "Chaotic"))

# Z-score each measure
sim_main_long <- do.call(rbind, lapply(split(sim_main_long, sim_main_long$variable), function(df) {
  df$value <- scale(df$value)
  df
}))

sim_main_long$variable <- factor(sim_main_long$variable,
                                 levels = c("RR", "DET", "ENTR"),
                                 labels = c("Recurrence Rate", "Determinism", "Entropy"))

p_sim_main <- ggplot(sim_main_long, aes(x = System, y = value, fill = System, color = System)) +
  geom_jitter(alpha = 0.2, width = 0.1) +
  geom_boxplot(fill = NA, color = "black", alpha = 0.2, linewidth = 0.5, outlier.shape = NA, width = 0.4) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("Stochastic" = "#E41A1C", "Periodic" = "#377EB8", "Chaotic" = "#4DAF4A")) +
  scale_fill_manual(values  = c("Stochastic" = "#E41A1C", "Periodic" = "#377EB8", "Chaotic" = "#4DAF4A")) +
  labs(x = NULL, y = "Value") +
  theme_bw() +
  theme(text = element_text(size = 18),
        legend.position = "none",
        strip.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))


# Visualize Regime Shift (Pre-Shift vs Post-Shift) (fig2, panel c)
sim_regime_data <- sim_all_results[sim_all_results$System %in% c("Pre-Shift", "Post-Shift"), ]
sim_regime_long <- melt(sim_regime_data[, c("sim_id", "System", "RR", "DET", "ENTR")],
                        id.vars = c("sim_id", "System"))

# Z-score each measure
sim_regime_long <- do.call(rbind, lapply(split(sim_regime_long, sim_regime_long$variable), function(df) {
  df$value <- scale(df$value)
  df
}))

sim_regime_long$variable <- factor(sim_regime_long$variable,
                                   levels = c("RR", "DET", "ENTR"),
                                   labels = c("Recurrence Rate", "Determinism", "Entropy"))

sim_regime_long$System <- factor(sim_regime_long$System, levels = c("Pre-Shift", "Post-Shift"))

p_sim_regime <- ggplot(sim_regime_long, aes(x = System, y = value, fill = System, color = System)) +
  geom_jitter(alpha = 0.2, width = 0.1) +
  geom_boxplot(fill = NA, color = "black", alpha = 0.2, linewidth = 0.5, outlier.shape = NA, width = 0.4) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("Pre-Shift" = "#FF7F00", "Post-Shift" = "#984EA3")) +
  scale_fill_manual(values  = c("Pre-Shift" = "#FF7F00", "Post-Shift" = "#984EA3")) +
  labs(x = NULL, y = "Value") +
  theme_bw() +
  theme(text = element_text(size = 18),
        legend.position = "none",
        strip.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Combined plot
sim_combined_plot <- grid.arrange(p_sim_main, p_sim_regime, nrow = 2, ncol = 1)
print(sim_combined_plot)

###############################################################################
# PART3: Statistical Analysis 
###############################################################################

# Three main systems
sim_measures <- c("RR", "DET", "ENTR")

cohens_d <- function(x, y) {
  mean_diff <- mean(x) - mean(y)
  pooled_sd <- sqrt((var(x) + var(y)) / 2)
  return(mean_diff / pooled_sd)
}

sim_main_stats_list <- lapply(sim_measures, function(measure) {
  
  stochastic_vals <- sim_main_systems[sim_main_systems$System == "Stochastic", measure]
  periodic_vals   <- sim_main_systems[sim_main_systems$System == "Periodic",   measure]
  chaotic_vals    <- sim_main_systems[sim_main_systems$System == "Chaotic",    measure]
  
  kw_p <- kruskal.test(list(stochastic_vals, periodic_vals, chaotic_vals))$p.value
  
  p_raw <- c(
    wilcox.test(stochastic_vals, periodic_vals)$p.value,
    wilcox.test(stochastic_vals, chaotic_vals)$p.value,
    wilcox.test(periodic_vals,   chaotic_vals)$p.value
  )
  p_adj <- p.adjust(p_raw, method = "fdr")
  
  data.frame(
    Measure    = measure,
    Comparison = c("Stochastic vs Periodic", "Stochastic vs Chaotic", "Periodic vs Chaotic"),
    KW_p       = kw_p,
    p_value    = p_raw,
    p_adjusted = p_adj,
    Cohens_d   = c(cohens_d(stochastic_vals, periodic_vals),
                   cohens_d(stochastic_vals, chaotic_vals),
                   cohens_d(periodic_vals,   chaotic_vals))
  )
})

sim_main_stats <- do.call(rbind, sim_main_stats_list)
rownames(sim_main_stats) <- NULL
print(sim_main_stats)


# Regime Shift Analysis (summary)
sim_pre_shift_data  <- sim_all_results[sim_all_results$System == "Pre-Shift",  ]
sim_post_shift_data <- sim_all_results[sim_all_results$System == "Post-Shift", ]

sim_pre_means       <- colMeans(sim_pre_shift_data[,  c("RR", "DET", "ENTR")])
sim_post_means      <- colMeans(sim_post_shift_data[, c("RR", "DET", "ENTR")])
sim_changes         <- sim_post_means - sim_pre_means
sim_percent_changes <- (sim_changes / sim_pre_means) * 100

sim_regime_shift_summary <- data.frame(
  Measure         = c("RR", "DET", "ENTR"),
  Pre_Shift_Mean  = sim_pre_means,
  Post_Shift_Mean = sim_post_means,
  Change          = sim_changes,
  Percent_Change  = sim_percent_changes
)

print(sim_regime_shift_summary)

# Regime Shift Analysis (tests)
sim_regime_stats_list <- lapply(sim_measures, function(measure) {
  
  pre_vals  <- sim_pre_shift_data[[measure]]
  post_vals <- sim_post_shift_data[[measure]]
  
  mean_diff <- mean(post_vals - pre_vals)
  
  data.frame(
    Measure     = measure,
    Pre_Mean    = round(mean(pre_vals), 4),
    Post_Mean   = round(mean(post_vals), 4),
    Mean_Change = round(mean_diff, 4),
    t_test_p    = t.test(post_vals, pre_vals, paired = TRUE)$p.value,
    Wilcoxon_p  = wilcox.test(post_vals, pre_vals, paired = TRUE)$p.value,
    Cohens_d    = mean_diff / sd(post_vals - pre_vals)
  )
})

sim_regime_stats <- do.call(rbind, sim_regime_stats_list)
rownames(sim_regime_stats) <- NULL
print(sim_regime_stats)