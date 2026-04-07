# ===============================
# 04_statistical_analysis_part2.R
# Mixed-effects models, ANOVA tables, post-hoc comparisons, ordered differences plots, interaction plots for valence x sex
# Output: model and post-hoc results for ValencexSex, 2 ordered differences plot, 4 interaction plots
# ===============================
library(lme4) 
library(lmerTest) 
library(car)
library(emmeans)# for post-hoc comparisons
library(tidyverse)
library(kableExtra)
library(broom.mixed)
library(gridExtra)
library(cowplot) # for legend 
# ===============================
# SET PATHS
# ===============================

data_dir <- "."  # UPDATE to your local data directory
results_dir <- file.path(data_dir, "results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

# ===============================
# LOAD AND PREPARE DATA
# ===============================

final_data_with_pause <- read.csv(file.path(data_dir, "deception_srqa_with_pause_1120_final.csv"))

# factor variables
final_data_with_pause <- final_data_with_pause %>%
  mutate(
    Veracity = factor(Veracity, levels = c(0, 1), labels = c("Lie", "Truth")),
    Valence = factor(Valence, levels = c(0, 1), labels = c("Negative", "Positive")),
    Sex = factor(Sex, levels = c(0, 1), labels = c("Female", "Male")),
    Race = factor(Race, levels = c(0, 1), labels = c("Black", "White"))
  )

# ===============================
# METRICS TO ANALYZE: 14
# ===============================

metrics_to_analyze <- c("RR", "DET", "L", "Lmax", "DIV", "ENTR", "LAM", "TT", 
                        "Vmax", "VENTR", "MRT", "RTE", "NMPRT", "TREND")
metric_labels <- c(
  "RR" = "Recurrence Rate",
  "DET" = "Determinism",
  "L" = "Mean Diagonal Length",
  "Lmax" = "Max Diagonal Length",
  "DIV" = "Divergence",
  "ENTR" = "Entropy",
  "LAM" = "Laminarity",
  "TT" = "Trapping Time",
  "Vmax" = "Max Vertical Line Length",
  "VENTR" = "Vertical Entropy",
  "MRT" = "Mean Recurrence Time",
  "RTE" = "Recurrence Time Entropy",
  "NMPRT" = "Most Probable Recurrence Times",
  "TREND" = "Trend of Recurrence Rate"
)


# ===============================
# STEP 1: Z-SCORE ALL RQA METRICS since outcomes are on different scales
# ===============================

final_data_scaled <- final_data_with_pause %>%
  mutate(across(starts_with("pause_"), 
                ~scale(., center = TRUE, scale = TRUE)[,1], #scale fun z scores each col
                .names = "{.col}_scaled")) #creates new cols

# ===============================
# STEP 2: RUN MODELS FUn
# ===============================

# Function to run model and extract Veracity beta (effect) and CI
extract_interaction_beta_vs  <- function(metric_name, data) {
  metric_col <- paste0("pause_", metric_name, "_scaled")
  
  # Run model: scaled_metric ~ Veracity*Valence + (1|participant)
  formula <- as.formula(paste0(metric_col, " ~ Veracity * Valence * Sex + Race + (1|participant_ID)"))
  model <- lmer(formula, data = data) #runs mixed effects model
  
  # Use broom.mixed::tidy to get coefficients with CI
  model_tidy <- tidy(model, conf.int = TRUE, conf.level = 0.95)
  
  # Extract Veracity main effect (how much does being truthful change the metric on average?)
  interaction_row <- model_tidy %>%
    filter(term == "ValencePositive:SexMale") #lie is the reference, so VeracityTruth is the coeff
  #plot the interaction instead now: VeracityTruth:ValencePositive as the effect, or ValencePositive:SexMale
  
  if(nrow(interaction_row) == 0) return(NULL) # sanity check - if there's no veracity, return nothing
  
  data.frame(
    Metric = metric_name,
    Beta = interaction_row$estimate,
    CI_lower = interaction_row$conf.low,
    CI_upper = interaction_row$conf.high,
    SE = interaction_row$std.error,
    p_value = interaction_row$p.value
  )
}

# Run for all metrics
beta_results_2 <- lapply(metrics_to_analyze, extract_interaction_beta_vs, data = final_data_scaled) #applies fun to all 14 metrics
beta_df_2 <- bind_rows(beta_results_2)

# ===============================
# STEP 3: ORDER BY BETA MAGNITUDE - this is good order for graph too 
# ===============================

beta_df_2 <- beta_df_2 %>%
  arrange(Beta) %>%  # Sort by beta (smallest to largest)
  mutate(Metric_ordered = factor(Metric, levels = Metric))  # make ordered factor (for plotting later) 
#neg beta = truth tellers have lower srqa metric than liars 
#pos beta: truth tellers have higher srqa metric than liars 


# ==============================
# INTERMEDIARY: run fdr on this for the results section of the paper (q) 
beta_df_2$fdr_p <- p.adjust(beta_df_2$p_value, method = "fdr")

# ===============================
# STEP 4: CREATE S-PLOT (ORDERED DIFFERENCES GRAPH)
# ===============================

s_plot_vs <- beta_df_2 %>%
  mutate(
    significant = sign(CI_lower) == sign(CI_upper),
    Metric_ordered = reorder(Metric_ordered, Beta)
  ) %>%
  ggplot(aes(x = Beta, y = Metric_ordered, color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") + 
  geom_pointrange(aes(xmin = CI_lower, xmax = CI_upper), size = 1) +
  scale_color_manual(values = c("gray60", "steelblue"),
                     labels = c("Non-significant", "Significant")) +
  
  xlab("Veracity × Sex Effect (95% CI)") +
  ylab("sRQA Measure") +
  labs(title = "Pause Patterns in Deceptive Speech") +
  
  theme_bw() + 
  theme(legend.position = "none") + 
  theme(text=element_text(size=22))

print(s_plot_vs)

ggsave(file.path(results_dir, "figure2_ordered_differences_vs.pdf"),
       plot = s_plot_vs, width = 10, height = 8, dpi = 300, device = cairo_pdf)

write.csv(beta_df_2, file.path(results_dir, "figure2_ordered_differences_table.csv"), row.names = FALSE)

# Print table
print(beta_df_2)
# =====================================================================
# PART 3: INTERACTION PLOTS (emmeans)
# adopted from pauls code structure
# =====================================================================

# new: let's z score these 
final_data_z <- final_data_with_pause %>%
  mutate(
    pause_Vmax = scale(pause_Vmax)[,1],
    pause_ENTR = scale(pause_ENTR)[,1],
    pause_DIV  = scale(pause_DIV)[,1],
    pause_RR   = scale(pause_RR)[,1],
    pause_Lmax = scale(pause_Lmax)[,1]
  )

#  ======================
# INTERACTION PLOTS 

# Significant metrics
significant_metrics <- c("Vmax", "DIV", "Lmax")

# Max Vertical Line Length
model_vmax_vs <- lmer(pause_Vmax ~ Veracity * Valence * Sex + Race + (1|participant_ID), 
                      data = final_data_with_pause)
emm_vmax_vs <- emmeans(model_vmax_vs, ~ Valence * Sex)
emm_vmax_vs_df <- as.data.frame(emm_vmax_vs)
pairs(emmeans(model_vmax_vs, ~ Sex | Valence), adjust = "tukey")

# Divergence
model_div_vs <- lmer(pause_DIV ~ Veracity * Valence * Sex + Race + (1|participant_ID), 
                     data = final_data_with_pause)
emm_div_vs <- emmeans(model_div_vs, ~ Valence * Sex)
emm_div_vs_df <- as.data.frame(emm_div_vs)
pairs(emmeans(model_div_vs, ~ Sex | Valence), adjust = "tukey")

# Max Diagonal Length
model_lmax_vs <- lmer(pause_Lmax ~ Veracity * Valence * Sex + Race + (1|participant_ID), 
                      data = final_data_with_pause)
emm_lmax_vs <- emmeans(model_lmax_vs, ~ Valence * Sex)
emm_lmax_vs_df <- as.data.frame(emm_lmax_vs)
pairs(emmeans(model_lmax_vs, ~ Sex | Valence), adjust = "tukey")


# Create interaction plots
p_vmax_vs <- ggplot(emm_vmax_vs_df, aes(x = Valence, y = emmean, 
                                        color = Sex, group = Sex, shape = Sex)) +
  geom_line(linewidth = 1.5, position = position_dodge(width = 0.2)) +
  geom_point(size = 5, alpha = 0.8, position = position_dodge(width = 0.2)) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), 
                 linewidth = 1.5, alpha = 0.6,
                 position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("Female" = "maroon", "Male" = "green4")) +
  scale_shape_manual(values = c("Female" = 16, "Male" = 17)) +
  theme_bw(base_size = 22) +
  labs(
    title = "Max Vertical Length",
    x = "Valence",
    y = "Adjusted Mean (95% CI)"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    legend.position = "right"
  )

p_div_vs <- ggplot(emm_div_vs_df, aes(x = Valence, y = emmean, 
                                      color = Sex, group = Sex, shape = Sex)) +
  geom_line(linewidth = 1.5, position = position_dodge(width = 0.2)) +
  geom_point(size = 5, alpha = 0.8, position = position_dodge(width = 0.2)) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), 
                 linewidth = 1.5, alpha = 0.6,
                 position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("Female" = "maroon", "Male" = "green4")) +
  scale_shape_manual(values = c("Female" = 16, "Male" = 17)) +
  theme_bw(base_size = 22) +
  labs(
    title = "Divergence",
    x = "Valence",
    y = "Adjusted Mean (95% CI)"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    legend.position = "none"
  )

p_lmax_vs <- ggplot(emm_lmax_vs_df, aes(x = Valence, y = emmean, 
                                        color = Sex, group = Sex, shape = Sex)) +
  geom_line(linewidth = 1.5, position = position_dodge(width = 0.2)) +
  geom_point(size = 5, alpha = 0.8, position = position_dodge(width = 0.2)) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), 
                 linewidth = 1.5, alpha = 0.6,
                 position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("Female" = "maroon", "Male" = "green4")) +
  scale_shape_manual(values = c("Female" = 16, "Male" = 17)) +
  theme_bw(base_size = 22) +
  labs(
    title = "Max Diagonal Length",
    x = "Topic Valence",
    y = "Adjusted Mean (95% CI)"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    legend.position = "none"
  )


# 
# ggsave(file.path(results_dir, "figure4_interactionplot_03052026.pdf"),
#        plot = combined_b, width = 8, height = 14, dpi = 300, device = cairo_pdf)

