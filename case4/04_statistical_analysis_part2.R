# ===============================
# 04_statistical_analysis_part2.R
# Mixed-effects models, ANOVA tables, post-hoc comparisons, ordered differences plots, interaction plots for valence x sex
# Output: model and post-hoc results for ValencexSex, 2 ordered differences plot, 4 interaction plots
# ===============================

# ===============================
# STEP 1: Z-SCORE ALL RQA METRICS since outcomes are on different scales
# (already have final_data_scaled from Part 1)

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

# --- Figure 3: Veracity x Valence interaction (4 significant metrics) ---

# these were sig (figure A)
significant_interaction_metrics <- c("Vmax", "ENTR", "DIV", "RR")

# Emmeans for each: copied from the mu3d state ouput script
# linear model ME model for each 

# max vertical line length
model_vmax <- lmer(pause_Vmax ~ Veracity * Valence + Sex + Race + (1|participant_ID), data = final_data_z)
emm_vmax <- emmeans(model_vmax, ~ Veracity * Valence)
emm_vmax_df <- as.data.frame(emm_vmax)
pairs(emmeans(model_vmax, ~ Veracity | Valence), adjust = "tukey")

# entropy
model_entr <- lmer(pause_ENTR ~ Veracity * Valence + Sex + Race + (1|participant_ID), data = final_data_z)
emm_entr <- emmeans(model_entr, ~ Veracity * Valence)
emm_entr_df <- as.data.frame(emm_entr)
pairs(emmeans(model_entr, ~ Veracity | Valence), adjust = "tukey")

# div
model_div <- lmer(pause_DIV ~ Veracity * Valence + Sex + Race + (1|participant_ID), data = final_data_z)
emm_div <- emmeans(model_div, ~ Veracity * Valence)
emm_div_df <- as.data.frame(emm_div)
pairs(emmeans(model_div, ~ Veracity | Valence), adjust = "tukey")

# RR
model_rr <- lmer(pause_RR ~ Veracity * Valence + Sex + Race + (1|participant_ID), data = final_data_z)
emm_rr <- emmeans(model_rr, ~ Veracity * Valence)
emm_rr_df <- as.data.frame(emm_rr)
pairs(emmeans(model_rr, ~ Veracity | Valence), adjust = "tukey")

# each of these is the predicted value (adjusted for covs + indiv participant differences)

# PLOTS: adopted from pauls code emailed

# Plot 1: Max Vertical Line Length
p_vmax <- ggplot(emm_vmax_df, aes(x = Veracity, y = emmean, 
                                  color = Valence, group = Valence, shape = Valence)) +
  geom_line(linewidth = 1.2, position = position_dodge(width = 0.2)) +  
  geom_point(size = 4, position = position_dodge(width = 0.2), alpha = 0.8) + 
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL),  
                 linewidth = 1.5, alpha = 0.6, 
                 position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("Negative" = "black", "Positive" = "#E64B35")) +
  scale_shape_manual(values = c("Negative" = 16, "Positive" = 17)) +
  theme_bw(base_size = 22) +
  labs(
    title = "Max V. Length",
    x = "Veracity",
    y = "Adjusted Mean (95% CI)"
  ) +
  theme(
    panel.grid.major = element_blank(),  # remove y grid lines
    panel.border = element_blank(),      # remove box around plot
    axis.line = element_line(color = "black", linewidth = 0.5),  # Add back axis lines
    legend.position = "none"
  )
print(p_vmax) 

# entropy
p_entr <- ggplot(emm_entr_df, aes(x = Veracity, y = emmean, 
                                  color = Valence, group = Valence, shape = Valence)) +
  geom_line(linewidth = 1.2, position = position_dodge(width = 0.2)) +
  geom_point(size = 4, position = position_dodge(width = 0.2), alpha = 0.8) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), 
                 linewidth = 1.5, alpha = 0.6,
                 position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("Negative" = "black", "Positive" = "#E64B35")) +
  scale_shape_manual(values = c("Negative" = 16, "Positive" = 17)) +
  theme_bw(base_size = 22) +
  labs(
    title = "Entropy",
    x = "Veracity",
    y = "Adjusted Mean (95% CI)"
  ) +
  theme(
    panel.grid.major = element_blank(),  # remove y grid lines
    panel.border = element_blank(),      # remove box around plot
    axis.line = element_line(color = "black", linewidth = 0.5)  # Add back axis lines
  )

print(p_entr)

# divergence
p_div <- ggplot(emm_div_df, aes(x = Veracity, y = emmean, 
                                color = Valence, group = Valence, shape = Valence)) +
  geom_line(linewidth = 1.2, position = position_dodge(width = 0.2)) +
  geom_point(size = 4, position = position_dodge(width = 0.2), alpha = 0.8) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), 
                 linewidth = 1.5, alpha = 0.6,
                 position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("Negative" = "black", "Positive" = "#E64B35")) +
  scale_shape_manual(values = c("Negative" = 16, "Positive" = 17)) +
  theme_bw(base_size = 22) +
  labs(
    title = "Divergence",
    x = "Veracity",
    y = "Adjusted Mean (95% CI)"
  ) +
  theme(
    panel.grid.major = element_blank(),  # remove y grid lines
    panel.border = element_blank(),      # remove box around plot
    axis.line = element_line(color = "black", linewidth = 0.5) , # Add back axis line 
    legend.position = "none"
  )

print(p_div)

# recurrence rate 
p_rr <- ggplot(emm_rr_df, aes(x = Veracity, y = emmean, 
                              color = Valence, group = Valence, shape = Valence)) +
  geom_line(linewidth = 1.2, position = position_dodge(width = 0.2)) +
  geom_point(size = 4, position = position_dodge(width = 0.2), alpha = 0.8) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), 
                 linewidth = 1.5, alpha = 0.6,
                 position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("Negative" = "black", "Positive" = "#E64B35")) +
  scale_shape_manual(values = c("Negative" = 16, "Positive" = 17)) +
  theme_bw(base_size = 22) +
  labs(
    title = "Recur. Rate",
    x = "Veracity",
    y = "Adjusted Mean (95% CI)"
  ) +
  theme(
    panel.grid.major = element_blank(),  # remove y grid lines
    panel.border = element_blank(),      # remove box around plot
    axis.line = element_line(color = "black", linewidth = 0.5),  # Add back axis lines
    legend.position = "none"
  )

print(p_rr) 
