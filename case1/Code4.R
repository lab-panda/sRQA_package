library(sRQA)
library(ggplot2)
library(reshape2)

# This code generates Table 1, Supplemental Figure 1, Supplemental Figure 2, &
# Supplemental Table 2

###############################################################################
# SETUP: Simulation parameters (needed to regenerate the underlying series)
###############################################################################

n_simulations <- 100
n <- 200
frequency <- 0.1
regime_shift_point <- 100

sim_methods <- c("rle", "ordinal_pattern", "quantiles")
sim_num_symbols <- 6
sim_window_size <- 3
sim_embedding_dimension <- 3
sim_min_line_length <- 2

symbolize_args <- function(method) {
  list(num_symbols = sim_num_symbols,
       window_size = sim_window_size)
}

run_symbolic_rqa <- function(ts_vec, method) {
  extra_args <- symbolize_args(method)
  sym <- do.call(symbolize_ts,
                 c(list(ts_vec, method = method, visualize = FALSE), extra_args))
  rm  <- symbolic_recurrence_matrix(sym, embedding_dimension = sim_embedding_dimension)
  quant_rqa(rm, min_line_length = sim_min_line_length)
}

rqa_all_metrics <- c("RR", "DET", "L", "Lmax", "DIV", "ENTR", "LAM", "TT",
                     "Vmax", "VENTR", "MRT", "RTE", "NMPRT", "TREND")

###############################################################################
# PART 1: Run the simulations
###############################################################################

sim_all_results <- data.frame(
  sim_id = integer(),
  Method = character(),
  System = character(),
  stringsAsFactors = FALSE
)
for (m in rqa_all_metrics) sim_all_results[[m]] <- numeric()

for (sim in 1:n_simulations) {
  
  set.seed(12345 + sim)
  
  sim_ts_noise <- rnorm(n, mean = 0, sd = 1)
  
  sim_ts_periodic <- sin(2 * pi * frequency * (1:n)) + rnorm(n, 0, 0.1)
  
  sim_ts_chaotic_first <- numeric(regime_shift_point)
  sim_ts_chaotic_first[1] <- runif(1, min = 0.01, max = 0.99)
  for (i in 2:regime_shift_point) {
    sim_ts_chaotic_first[i] <- 3.9 * sim_ts_chaotic_first[i - 1] * (1 - sim_ts_chaotic_first[i - 1])
  }
  sim_ts_chaotic_first <- scale(sim_ts_chaotic_first, center = TRUE, scale = TRUE)
  
  sim_ts_chaotic_second <- sin(2 * pi * frequency * (1:regime_shift_point)) + rnorm(regime_shift_point, 0, 0.1)
  sim_ts_chaotic_second <- scale(sim_ts_chaotic_second)
  
  sim_ts_chaotic <- as.numeric(rbind(sim_ts_chaotic_first, sim_ts_chaotic_second))
  
  for (method in sim_methods) {
    
    sim_rqa_noise    <- run_symbolic_rqa(sim_ts_noise,      method)
    sim_rqa_periodic <- run_symbolic_rqa(sim_ts_periodic,   method)
    sim_rqa_chaotic  <- run_symbolic_rqa(sim_ts_chaotic,    method)
    
    rqa_list <- list(Stochastic = sim_rqa_noise, Periodic = sim_rqa_periodic, Chaotic = sim_rqa_chaotic)
    
    sim_results <- data.frame(
      sim_id = rep(sim, 3),
      Method = rep(method, 3),
      System = names(rqa_list),
      stringsAsFactors = FALSE
    )
    for (met in rqa_all_metrics) {
      sim_results[[met]] <- sapply(rqa_list, function(x) x[[met]])
    }
    
    sim_all_results <- rbind(sim_all_results, sim_results)
  }
}

sim_rle_results <- sim_all_results[sim_all_results$Method == "rle", ]

###############################################################################
# PART 2: Supplemental Figures S1 & S2
# Method comparison within each system
###############################################################################

sim_zdist <- sim_all_results[sim_all_results$System %in% c("Stochastic", "Periodic", "Chaotic"),
                             c("sim_id", "Method", "System", "RR", "DET", "ENTR")]

sim_zdist_long <- melt(sim_zdist, id.vars = c("sim_id", "Method", "System"),
                       variable.name = "Measure", value.name = "value")

# Exclude RR under quantiles (near-uniform marginal distribution by construction)
sim_zdist_long <- sim_zdist_long[!(sim_zdist_long$Method == "quantiles" & sim_zdist_long$Measure == "RR"), ]

# Z-score within (Method, Measure)
sim_zdist_long <- do.call(rbind, lapply(
  split(sim_zdist_long, list(sim_zdist_long$Method, sim_zdist_long$Measure)),
  function(df) { df$z <- as.numeric(scale(df$value)); df }
))

sim_zdist_long$Method  <- factor(sim_zdist_long$Method, levels = sim_methods)
sim_zdist_long$Measure <- factor(sim_zdist_long$Measure, levels = c("RR", "DET", "ENTR"),
                                 labels = c("Recurrence Rate", "Determinism", "Entropy"))
sim_zdist_long$System  <- factor(sim_zdist_long$System, levels = c("Stochastic", "Periodic", "Chaotic"))


levels(factor(sim_zdist_long$Method))
p_consistency_by_system <- ggplot(sim_zdist_long, aes(x = Method, y = z, fill = Method, color = Method)) +
  geom_jitter(alpha = 0.2, position = position_jitter(width = 0.1, height = 0, seed = 42)) +
  geom_boxplot(fill = NA, color = "black", alpha = 0.2, linewidth = 0.5, outlier.shape = NA, width = 0.4) +
  facet_grid(Measure ~ System, scales = "free_y") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(labels = c("RLE", "Ordinal", "Quantiles")) +
  labs(x = NULL, y = "Z-Score") +
  theme_bw() +
  theme(text = element_text(size = 18),
        legend.position = "none",
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(p_consistency_by_system)


p_consistency <- ggplot(sim_zdist_long, aes(x = System, y = z, fill = System, color = System)) +
  geom_jitter(alpha = 0.2, position = position_jitter(width = 0.1, height = 0, seed = 42)) +
  geom_boxplot(fill = NA, color = "black", alpha = 0.2, linewidth = 0.5, outlier.shape = NA, width = 0.4) +
  facet_grid(Measure ~ Method, scales = "free_y",
             labeller = labeller(Method = c(rle = "RLE",
                                            ordinal_pattern = "Ordinal",
                                            quantiles = "Quantiles"))) +
  scale_color_manual(values = c("Stochastic" = "#E41A1C", "Periodic" = "#377EB8", "Chaotic" = "#4DAF4A")) +
  scale_fill_manual(values  = c("Stochastic" = "#E41A1C", "Periodic" = "#377EB8", "Chaotic" = "#4DAF4A")) +
  labs(x = NULL, y = "Z-Score") +
  theme_bw() +
  theme(text = element_text(size = 18),
        legend.position = "none",
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(p_consistency)

###############################################################################
# PART 3: Supplemental Figure S3
# Original vs shuffled surrogates
###############################################################################

sim_surrogate_results <- data.frame(
  sim_id = integer(),
  Method = character(),
  System = character(),
  RR = numeric(),
  DET = numeric(),
  ENTR = numeric(),
  stringsAsFactors = FALSE
)

for (sim in 1:n_simulations) {
  
  set.seed(12345 + sim)  # same seed as Part 1 -> identical base series
  
  sim_ts_noise <- rnorm(n, mean = 0, sd = 1)
  sim_ts_periodic <- sin(2 * pi * frequency * (1:n)) + rnorm(n, 0, 0.1)
  
  sim_ts_chaotic_first <- numeric(regime_shift_point)
  sim_ts_chaotic_first[1] <- runif(1, min = 0.01, max = 0.99)
  for (i in 2:regime_shift_point) {
    sim_ts_chaotic_first[i] <- 3.9 * sim_ts_chaotic_first[i - 1] * (1 - sim_ts_chaotic_first[i - 1])
  }
  sim_ts_chaotic_first <- scale(sim_ts_chaotic_first, center = TRUE, scale = TRUE)
  
  sim_ts_chaotic_second <- sin(2 * pi * frequency * (1:regime_shift_point)) + rnorm(regime_shift_point, 0, 0.1)
  sim_ts_chaotic_second <- scale(sim_ts_chaotic_second)
  
  sim_ts_chaotic <- as.numeric(rbind(sim_ts_chaotic_first, sim_ts_chaotic_second))
  
  # Shuffle - preserves marginal distribution, destroys temporal order
  sim_ts_noise_surr    <- sample(sim_ts_noise)
  sim_ts_periodic_surr <- sample(sim_ts_periodic)
  sim_ts_chaotic_surr  <- sample(sim_ts_chaotic)
  
  for (method in sim_methods) {
    
    surr_rqa_noise    <- run_symbolic_rqa(sim_ts_noise_surr,    method)
    surr_rqa_periodic <- run_symbolic_rqa(sim_ts_periodic_surr, method)
    surr_rqa_chaotic  <- run_symbolic_rqa(sim_ts_chaotic_surr,  method)
    
    surr_results <- data.frame(
      sim_id = rep(sim, 3),
      Method = rep(method, 3),
      System = c("Stochastic", "Periodic", "Chaotic"),
      RR   = c(surr_rqa_noise$RR,   surr_rqa_periodic$RR,   surr_rqa_chaotic$RR),
      DET  = c(surr_rqa_noise$DET,  surr_rqa_periodic$DET,  surr_rqa_chaotic$DET),
      ENTR = c(surr_rqa_noise$ENTR, surr_rqa_periodic$ENTR, surr_rqa_chaotic$ENTR),
      stringsAsFactors = FALSE
    )
    
    sim_surrogate_results <- rbind(sim_surrogate_results, surr_results)
  }
}

comparison_cols <- c("sim_id", "Method", "System", "RR", "DET", "ENTR")
sim_original_for_comparison <- sim_all_results[
  sim_all_results$System %in% c("Stochastic", "Periodic", "Chaotic"), comparison_cols]
sim_original_for_comparison$Type <- "Original"
sim_surrogate_results$Type <- "Surrogate"

sim_comparison_data <- rbind(sim_original_for_comparison, sim_surrogate_results)

sim_surrogate_long <- melt(sim_comparison_data[, c("sim_id", "Method", "System", "Type", "RR", "DET", "ENTR")],
                           id.vars = c("sim_id", "Method", "System", "Type"),
                           variable.name = "Measure", value.name = "value")

sim_surrogate_long <- do.call(rbind, lapply(
  split(sim_surrogate_long, list(sim_surrogate_long$Method, sim_surrogate_long$Measure)),
  function(df) { df$z <- as.numeric(scale(df$value)); df }
))

sim_surrogate_long$Type <- factor(sim_surrogate_long$Type, levels = c("Original", "Surrogate"))
sim_surrogate_long$Method <- factor(sim_surrogate_long$Method, levels = sim_methods)
sim_surrogate_long$Measure <- factor(sim_surrogate_long$Measure, levels = c("RR", "DET", "ENTR"),
                                     labels = c("Recurrence Rate", "Determinism", "Entropy"))
sim_surrogate_long$System <- factor(sim_surrogate_long$System,
                                    levels = c("Stochastic", "Periodic", "Chaotic"))

# One plot per symbolization method (object p_surrogate is reassigned/printed each time)
p_surrogate_list <- list()  # to hold all plots, keyed by method name

for (m in sim_methods) {
  
  p_surrogate <- ggplot(sim_surrogate_long[sim_surrogate_long$Method == m, ],
                        aes(x = Type, y = z, fill = Type, color = Type)) +
    geom_jitter(alpha = 0.2, position = position_jitter(width = 0.1, height = 0, seed = 42)) +
    geom_boxplot(fill = NA, color = "black", alpha = 0.2, linewidth = 0.5, outlier.shape = NA, width = 0.4) +
    facet_grid(Measure ~ System, scales = "free_y") +
    scale_color_manual(values = c("Original" = "#1B9E77", "Surrogate" = "#D95F02")) +
    scale_fill_manual(values  = c("Original" = "#1B9E77", "Surrogate" = "#D95F02")) +
    labs(x = NULL, y = "Z-Score") +
    theme_bw() +
    theme(text = element_text(size = 18),
          legend.position = "none",
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save into the list under its method name
  p_surrogate_list[[m]] <- p_surrogate
}

# Visualize quantiles
print(p_surrogate_list[["quantiles"]])

###############################################################################
# PART 4: Table 1: Descriptive Statistics, Mean (SD)
###############################################################################

sim_rle_main <- sim_rle_results[sim_rle_results$System %in% c("Stochastic", "Periodic", "Chaotic"), ]

desc_table <- do.call(rbind, lapply(rqa_all_metrics, function(measure) {
  do.call(rbind, lapply(c("Stochastic", "Periodic", "Chaotic"), function(system) {
    vals <- sim_rle_main[sim_rle_main$System == system, measure]
    data.frame(Measure = measure, System = system,
               Mean = round(mean(vals), 3), SD = round(sd(vals), 3))
  }))
}))

desc_table$Cell <- sprintf("%.3f (%.3f)", desc_table$Mean, desc_table$SD)
desc_table_wide <- reshape(desc_table[, c("Measure", "System", "Cell")],
                           idvar = "Measure", timevar = "System", direction = "wide")
colnames(desc_table_wide) <- gsub("^Cell\\.", "", colnames(desc_table_wide))
desc_table_wide <- desc_table_wide[, c("Measure", "Stochastic", "Periodic", "Chaotic")]
desc_table_wide$Measure <- factor(desc_table_wide$Measure, levels = rqa_all_metrics)
desc_table_wide <- desc_table_wide[order(desc_table_wide$Measure), ]

print(desc_table_wide, row.names = FALSE)
###############################################################################
# PART 5: Table S1: Cohen's d (FDR-adjusted p)
###############################################################################

cohens_d <- function(x, y) {
  mean_diff <- mean(x) - mean(y)
  pooled_sd <- sqrt((var(x) + var(y)) / 2)
  return(mean_diff / pooled_sd)
}

cohens_d_ci <- function(x, y, conf_level = 0.95) {
  n1 <- length(x); n2 <- length(y)
  d <- cohens_d(x, y)
  se_d <- sqrt((n1 + n2) / (n1 * n2) + (d^2) / (2 * (n1 + n2)))
  z <- qnorm(1 - (1 - conf_level) / 2)
  data.frame(Cohens_d = d, CI_Lower = d - z * se_d, CI_Upper = d + z * se_d)
}

safe_t_test_p <- function(x, y, tol = 1e-8, label = "") {
  if (any(!is.finite(x)) || any(!is.finite(y))) {
    warning("Non-finite values (NA/NaN/Inf) present for ", label, " - t-test skipped (NA returned)")
    return(NA_real_)
  }
  if (sd(x) < tol || sd(y) < tol) {
    warning("Near-constant data for ", label, " - t-test skipped (NA returned)")
    return(NA_real_)
  }
  tryCatch(t.test(x, y)$p.value, error = function(e) {
    warning("t.test failed for ", label, ": ", conditionMessage(e))
    NA_real_
  })
}

safe_cohens_d_ci <- function(x, y, conf_level = 0.95, label = "") {
  if (any(!is.finite(x)) || any(!is.finite(y))) {
    warning("Non-finite values (NA/NaN/Inf) present for ", label, " - Cohen's d not computed (NA returned)")
    return(data.frame(Cohens_d = NA_real_, CI_Lower = NA_real_, CI_Upper = NA_real_))
  }
  if (sd(x) < 1e-8 || sd(y) < 1e-8) {
    warning("Near-constant data for ", label, " - Cohen's d not computed (NA returned)")
    return(data.frame(Cohens_d = NA_real_, CI_Lower = NA_real_, CI_Upper = NA_real_))
  }
  cohens_d_ci(x, y, conf_level)
}

full_metric_stats_list <- list()

for (measure in rqa_all_metrics) {
  
  stochastic_vals <- sim_rle_main[sim_rle_main$System == "Stochastic", measure]
  periodic_vals   <- sim_rle_main[sim_rle_main$System == "Periodic",   measure]
  chaotic_vals    <- sim_rle_main[sim_rle_main$System == "Chaotic",    measure]
  
  lbl <- paste("rle", measure)
  
  p_raw <- c(
    safe_t_test_p(stochastic_vals, periodic_vals, label = paste(lbl, "Stochastic vs Periodic")),
    safe_t_test_p(stochastic_vals, chaotic_vals,   label = paste(lbl, "Stochastic vs Chaotic")),
    safe_t_test_p(periodic_vals,   chaotic_vals,   label = paste(lbl, "Periodic vs Chaotic"))
  )
  p_adj <- p.adjust(p_raw, method = "fdr")
  
  d_ci_sp <- safe_cohens_d_ci(stochastic_vals, periodic_vals, label = paste(lbl, "Stochastic vs Periodic"))
  d_ci_sc <- safe_cohens_d_ci(stochastic_vals, chaotic_vals,   label = paste(lbl, "Stochastic vs Chaotic"))
  d_ci_pc <- safe_cohens_d_ci(periodic_vals,   chaotic_vals,   label = paste(lbl, "Periodic vs Chaotic"))
  d_ci_all <- rbind(d_ci_sp, d_ci_sc, d_ci_pc)
  
  full_metric_stats_list[[length(full_metric_stats_list) + 1]] <- data.frame(
    Measure    = measure,
    Comparison = c("Stochastic vs Periodic", "Stochastic vs Chaotic", "Periodic vs Chaotic"),
    Cohens_d   = d_ci_all$Cohens_d,
    p_value    = p_raw,
    p_adjusted = p_adj
  )
}

sim_full_metric_stats <- do.call(rbind, full_metric_stats_list)
rownames(sim_full_metric_stats) <- NULL

d_table <- sim_full_metric_stats[, c("Measure", "Comparison", "Cohens_d", "p_adjusted")]

d_table$Cell <- sprintf(
  "%.2f (%s)", d_table$Cohens_d,
  ifelse(is.na(d_table$p_adjusted), "n/a",
         ifelse(d_table$p_adjusted < .001, "p < .001",
                paste0("p = ", formatC(d_table$p_adjusted, format = "f", digits = 3))))
)

d_table_wide <- reshape(d_table[, c("Measure", "Comparison", "Cell")],
                        idvar = "Measure", timevar = "Comparison", direction = "wide")
colnames(d_table_wide) <- gsub("^Cell\\.", "", colnames(d_table_wide))

comp_order <- c("Stochastic vs Periodic", "Stochastic vs Chaotic", "Periodic vs Chaotic")
d_table_wide <- d_table_wide[, c("Measure", comp_order)]
d_table_wide$Measure <- factor(d_table_wide$Measure, levels = rqa_all_metrics)
d_table_wide <- d_table_wide[order(d_table_wide$Measure), ]

print(d_table_wide, row.names = FALSE)