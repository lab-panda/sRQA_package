
library(sRQA)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(reshape2)

setwd("D:/Projects/sRQA_paper")

###############################################################################
# PART 1: Create single example time series
###############################################################################

# Example Time series 
set.seed(12345)

# Parameters
n <- 200  # Length of time series
frequency <- 0.1  # 10 time units per cycle

# 1a. Stochastic (White Noise)
ts_noise <- rnorm(n, mean = 0, sd = 1)

# 1b. Periodic (Sinusoidal with known frequency)
ts_periodic <- sin(2 * pi * frequency * (1:n)) + rnorm(n, 0, 0.1)

# 1c. Chaotic
# First half: Highly chaotic (r = 3.9)
ts_chaotic <- numeric(100)
ts_chaotic[1] <- 0.1

for(i in 2:100) {
  ts_chaotic[i] <- 3.9 * ts_chaotic[i-1] * (1 - ts_chaotic[i-1])
}

ts_chaotic <- scale(ts_chaotic, center = TRUE, scale = TRUE)

# Second half: Periodic sine wave
ts_chaotic2 <- sin(2 * pi * frequency * (1:100)) + rnorm(100, 0, 0.1)
ts_chaotic2 <- scale(ts_chaotic2)

fin_ts_chaotic <- rbind(ts_chaotic, ts_chaotic2)

###############################################################################
# Part 2: Symbolize and visualize examples 
###############################################################################

s1_method <- "rle"
s1_num_symbols <- 6
s1_window_size <- 3
s1_embedding_dimension <- 3
s1_min_line_length <- 2

# 3a. Stochastic system
symbols_noise <- symbolize_ts(ts_noise, method = s1_method,
                              num_symbols = s1_num_symbols,
                              window_size = s1_window_size, visualize = F)

# Make nice for plot
a <- visualize_symbols(ts_noise, symbols_noise, point_size = 1.5, line_size = 0.8,
                       alpha = 0.8)
a_fin <- a + ggtitle("Stochastic Time Series") + theme_minimal(base_size = 18) +
  labs(x = "Time", y = "Value") +
  theme(legend.position = "none", axis.text = element_text(size = 14)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))

rm_noise <- symbolic_recurrence_matrix(symbols_noise,
                                       embedding_dimension = s1_embedding_dimension)
rqa_noise <- quant_rqa(rm_noise, min_line_length = s1_min_line_length)
rqa_noise <- as.data.frame(rqa_noise)
rqa_noise$system <- "noise"

# 3b. Periodic system
symbols_periodic <- symbolize_ts(ts_periodic, method = s1_method,
                                 num_symbols = s1_num_symbols,
                                 window_size = s1_window_size, visualize = F)

b <- visualize_symbols(ts_periodic, symbols_periodic, point_size = 1.5, line_size = 0.8,
                       alpha = 0.8)
b_fin <- b + ggtitle("Periodic Time Series") + theme_minimal(base_size = 18) +
  labs(x = "Time", y = "Value") +
  theme(legend.position = "none", axis.text = element_text(size = 14))

rm_periodic <- symbolic_recurrence_matrix(symbols_periodic,
                                          embedding_dimension = s1_embedding_dimension)
rqa_periodic <- quant_rqa(rm_periodic, min_line_length = s1_min_line_length)
rqa_periodic <- as.data.frame(rqa_periodic)
rqa_periodic$system <- "periodic"

# 3c. Chaotic system (full)
symbols_chaotic <- symbolize_ts(fin_ts_chaotic, method = s1_method,
                                num_symbols = s1_num_symbols,
                                window_size = s1_window_size, visualize = F)

c_plot <- visualize_symbols(fin_ts_chaotic, symbols_chaotic, point_size = 1.5, line_size = 0.8,
                            alpha = 0.8)
c_fin <- c_plot + ggtitle("Chaotic Time Series") + theme_minimal(base_size = 18) +
  labs(x = "Time", y = "Value") +
  theme(legend.position = "none", axis.text = element_text(size = 14))

rm_chaotic <- symbolic_recurrence_matrix(symbols_chaotic,
                                         embedding_dimension = s1_embedding_dimension)
rqa_chaotic <- quant_rqa(rm_chaotic, min_line_length = s1_min_line_length)
rqa_chaotic <- as.data.frame(rqa_chaotic)
rqa_chaotic$system <- "chaotic"

# Combined plot and results table
panel1 <- grid.arrange(a_fin, b_fin, c_fin, nrow = 3, ncol = 1)
print(panel1)

# Combine results for Table 1
s1_comb_results <- rbind(rqa_noise, rqa_periodic, rqa_chaotic)
print(s1_comb_results)

###############################################################################
# Part3: Visualize RPs (symbols) 
###############################################################################

p_srp_noise <- symbol_recurrence_plot(
  symbols_noise,
  embedding_dimension = s1_embedding_dimension,
  window_size = s1_window_size,
  title = "Stochastic (White Noise)",
  theme = "light",
  point_size = 0.5,
  alpha = 0.5
)

p_srp_noise <- p_srp_noise + theme_bw(base_size = 18) + ggtitle("Stochastic") +
  theme(legend.position = "none") + labs(subtitle = NULL) +
  scale_x_continuous(breaks = seq(0, 200, 50)) +
  scale_y_continuous(breaks = seq(0, 200, 50)) + coord_fixed(ratio = 1)

p_srp_periodic <- symbol_recurrence_plot(
  symbols_periodic,
  embedding_dimension = s1_embedding_dimension,
  window_size = s1_window_size,
  title = "Periodic (f=0.1)",
  theme = "light",
  point_size = 0.5,
  alpha = 0.5
)

p_srp_periodic <- p_srp_periodic + theme_bw(base_size = 18) + ggtitle("Periodic") +
  theme(legend.position = "none") + labs(subtitle = NULL) +
  scale_x_continuous(breaks = seq(0, 200, 50)) +
  scale_y_continuous(breaks = seq(0, 200, 50)) + coord_fixed(ratio = 1)

p_srp_chaotic <- symbol_recurrence_plot(
  symbols_chaotic,
  embedding_dimension = s1_embedding_dimension,
  window_size = s1_window_size,
  title = "Chaotic with Regime Shift",
  theme = "light",
  point_size = 0.5,
  alpha = 0.5
)

p_srp_chaotic <- p_srp_chaotic + theme_bw(base_size = 18) + ggtitle("Chaotic") +
  theme(legend.position = "none") + labs(subtitle = NULL) +
  scale_x_continuous(breaks = seq(0, 200, 50)) +
  scale_y_continuous(breaks = seq(0, 200, 50)) + coord_fixed(ratio = 1)

panel2 <- grid.arrange(p_srp_noise, p_srp_periodic, p_srp_chaotic, ncol = 3)
print(panel2)

###############################################################################
# Part 4: Example detecting regime shift
###############################################################################

s1_window_width <- 40
s1_step_size <- 5
s1_n_windows <- floor((n - s1_window_width) / s1_step_size) + 1

s1_windowed_num_symbols <- 8
s1_windowed_embedding_dim <- 3

windowed_rqa <- data.frame(
  window_center = numeric(s1_n_windows),
  RR = numeric(s1_n_windows),
  DET = numeric(s1_n_windows),
  L = numeric(s1_n_windows),
  ENTR = numeric(s1_n_windows),
  LAM = numeric(s1_n_windows)
)

for(i in 1:s1_n_windows) {
  start_idx <- (i - 1) * s1_step_size + 1
  end_idx <- start_idx + s1_window_width - 1
  
  if(end_idx > n) break
  
  window_ts <- fin_ts_chaotic[start_idx:end_idx]
  window_center <- (start_idx + end_idx) / 2
  
  window_symbols <- symbolize_ts(window_ts, method = "rle",
                                 num_symbols = s1_windowed_num_symbols,
                                 window_size = s1_window_size)
  window_rm <- symbolic_recurrence_matrix(window_symbols,
                                          embedding_dimension = s1_windowed_embedding_dim)
  window_rqa <- quant_rqa(window_rm, min_line_length = s1_min_line_length)
  
  windowed_rqa$window_center[i] <- window_center
  windowed_rqa$RR[i] <- window_rqa$RR
  windowed_rqa$DET[i] <- window_rqa$DET
  windowed_rqa$L[i] <- window_rqa$L
  windowed_rqa$ENTR[i] <- window_rqa$ENTR
  windowed_rqa$LAM[i] <- window_rqa$LAM
  
  if(i %% 5 == 0) cat("Processed window", i, "of", s1_n_windows, "\n")
}

windowed_rqa <- windowed_rqa[windowed_rqa$window_center > 0, ]

# Visualize windowed results (fig 2, panel b)
windowed_long <- melt(windowed_rqa, id.vars = "window_center")
windowed_long <- windowed_long %>%
  group_by(variable) %>%
  mutate(value_z = as.numeric(scale(value))) %>%
  ungroup()

s1_select <- windowed_long[windowed_long$variable %in% c("RR", "DET", "ENTR"), ]

facet_labels <- c("RR" = "Recurrence Rate", "DET" = "Determinism", "ENTR" = "Entropy")
p_regime_shift <- ggplot(s1_select, aes(x = window_center, y = value_z)) +
  annotate("rect",
           xmin = 85, xmax = 115,
           ymin = -Inf, ymax = Inf,
           fill = "#2D6A4F", alpha = 0.08) +
  geom_line(color = "#1B3A6B", linewidth = 0.9, alpha = 0.85) +
  geom_point(color = "#1B3A6B", size = 1.8, alpha = 0.7) +
  geom_vline(xintercept = 100,
             color = "#2D6A4F", linetype = "dashed", linewidth = 0.9, alpha = 0.7) +
  annotate("text",
           x = 100, y = Inf,
           label = "Regime Shift",
           color = "#333333",
           vjust = 1.8,
           size = 3.2,
           fontface = "bold") +
  facet_wrap(~ variable, scales = "free_y", ncol = 3,
             labeller = labeller(variable = facet_labels)) +
  labs(
    title = "Windowed sRQA: Detecting Regime Shift",
    x = "Time (Window Center)",
    y = "RQA Measure"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20,
                              margin = margin(b = 12)),
    strip.text = element_text(face = "bold", size = 14, color = "#333333"),
    strip.background = element_rect(fill = "#eeeeee", color = NA),
    axis.title = element_text(size = 14, color = "#444444"),
    axis.text = element_text(size = 11, color = "#666666"),
    axis.ticks = element_line(color = "#bbbbbb"),
    panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "#cccccc", fill = NA, linewidth = 0.5),
    panel.spacing = unit(1.2, "lines"),
    plot.margin = margin(16, 16, 12, 12),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
print(p_regime_shift)

