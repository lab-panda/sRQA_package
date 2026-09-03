library(sRQA)
library(ggplot2)
library(patchwork)

# FIGURE 1
# Generate a periodic time series 
t <- seq(0, 4*pi, length.out = 200)
ts_periodic <- sin(t) 
ts_periodic <- scale(ts_periodic)

# Apply symbolize_ts() function to symbolize the periodic signal 
quant_symbols = sRQA::symbolize_ts(ts_periodic, method="quantiles", num_symbols=5, visualize=T)
plotQ <- visualize_symbols(ts_periodic,quant_symbols,method = "quantiles",title = "Quantile Symbolization") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0, face = "plain"), legend.position = "none") + ylab("Z-score")

ord_symbols = sRQA::symbolize_ts(ts_periodic, method="ordinal_pattern", window_size=3, num_symbols=6, visualize=T)
plotO <- visualize_symbols(ts_periodic,ord_symbols,method = "ordinal_pattern",title = "Ordinal Pattern Symbolization") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0, face = "plain"),legend.position = "none") + ylab("Z-score")

rle_symbols = sRQA::symbolize_ts(ts_periodic, method="rle", window_size=10, num_symbols=8, visualize=T)
plotR <- visualize_symbols(ts_periodic,rle_symbols,method = "rle",title = "RLE Symbolization") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0, face = "plain"),legend.position = "none") + ylab("Z-score")

# Recurrence plots
srp_quant <- symbol_recurrence_plot(quant_symbols, embedding_dimension = 1, theme = "light",
  point_size = 0.3, alpha = 0.8) +
  ggtitle("Quantile Symbolization") +
  labs(subtitle = NULL) +
  scale_x_continuous(breaks = seq(0, 200, 50)) +
  scale_y_continuous(breaks = seq(0, 200, 50)) +
  coord_fixed(ratio = 1) +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1))) +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0),legend.title = element_text(hjust = 0),
  legend.text = element_text(hjust = 0),legend.justification = "left") + ylab("Time") + xlab("Time")

srp_ord <- symbol_recurrence_plot(ord_symbols, embedding_dimension = 1, window_size = 3, theme = "light",
  point_size = 0.3, alpha = 0.8) +
  ggtitle("Ordinal Pattern Symbolization") +
  labs(subtitle = NULL) +
  scale_x_continuous(breaks = seq(0, 200, 50)) +
  scale_y_continuous(breaks = seq(0, 200, 50)) +
  coord_fixed(ratio = 1) +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1))) +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0), legend.title = element_text(hjust = 0),
  legend.text = element_text(hjust = 0),legend.justification = "left") + ylab("Time") + xlab("Time")

srp_rle <- symbol_recurrence_plot(rle_symbols, embedding_dimension = 1, window_size = 10, theme = "light",
  point_size = 0.3, alpha = 0.8) +
  ggtitle("RLE Symbolization") +
  labs(subtitle = NULL) +
  scale_x_continuous(breaks = seq(0, 200, 50)) +
  scale_y_continuous(breaks = seq(0, 200, 50)) +
  coord_fixed(ratio = 1) +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1))) +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0),legend.title = element_text(hjust = 0),
  legend.text = element_text(hjust = 0),legend.justification = "left") + ylab("Time") + xlab("Time")

# Build figure
figure <- (plotQ | srp_quant) / (plotO | srp_ord) / (plotR | srp_rle) +
  plot_annotation(tag_levels = "a", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 24, face = "plain"))
print(figure)
