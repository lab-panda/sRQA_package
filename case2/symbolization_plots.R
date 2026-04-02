library(readr)
library(sRQA)
library(ggplot2)
library(gridExtra)
library(viridis)  

setwd("D:/Projects/Physionet")
normal <- read_csv("mit-bih-atrial-fibrillation-database-1.0.0/windowed/00735_window0_N.csv")
afib <- read_csv("mit-bih-atrial-fibrillation-database-1.0.0/windowed/00735_window15_AF.csv")

n_symbols=symbolize_ts(normal$rr_interval, method="quantiles", num_symbols=5, window_size=5, visualize=F) 
a_symbols=symbolize_ts(afib$rr_interval, method="quantiles", num_symbols=5, window_size=5, visualize=F) 

# Time series plots with Viridis
save_n=visualize_symbols(normal$rr_interval,n_symbols,method="quantiles")
save_n=save_n + ylab("R-R Interval (ms)") + xlab("Time (index)") + ggtitle("Normal Sinus Rhythm") +
  theme_bw(base_size = 18) +
  theme(plot.title = element_text(face = "plain"), legend.position = "none") +
  scale_color_viridis_d(option = "viridis")

sym_legend=save_n + ylab("R-R Interval (ms)") + xlab("Time (index)") + ggtitle("Normal Sinus Rhythm") +
  theme_bw(base_size = 18) +
  theme(plot.title = element_text(face = "plain"), legend.position = "bottom") +
  scale_color_viridis_d(option = "viridis")

save_a=visualize_symbols(afib$rr_interval,a_symbols,method="quantiles") 
save_a=save_a + ylab("R-R Interval (ms)") + xlab("Time (index)") + ggtitle("Atrial Fibrillation") +
  theme_bw(base_size = 18) +
  theme(plot.title = element_text(face = "plain"), legend.position = "none") +
  scale_color_viridis_d(option = "viridis")

a=grid.arrange(save_n,save_a,nrow=2)
print(a)

# Recurrence plots with Viridis
save_n2=symbol_recurrence_plot(n_symbols,
                               point_size = 1,
                               point_shape = 16,
                               alpha = 0.4)
save_n2=save_n2 + ggtitle("Normal Sinus Rhythm") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 200, 50)) +
  scale_y_continuous(breaks = seq(0, 200, 50)) +
  scale_color_viridis_d(option = "viridis") +
  coord_fixed(ratio = 1)

save_a2=symbol_recurrence_plot(a_symbols,
                               point_size = 1,
                               point_shape = 16,
                               alpha = 0.4)
save_a2=save_a2 + ggtitle("Atrial Fibrillation") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 200, 50)) +
  scale_y_continuous(breaks = seq(0, 200, 50)) +
  scale_color_viridis_d(option = "viridis") +
  coord_fixed(ratio = 1)

b=grid.arrange(save_n2,save_a2,nrow=1)
print(b)