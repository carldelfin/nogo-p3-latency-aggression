# ==================================================================================================
# Figure 1
# ==================================================================================================

f1_1 <- box_plotter("lha_agg") +
  scale_y_continuous(name = "LHA Aggression",
                     expand = c(0.02, 0),
                     limits = c(0, 25),
                     breaks = seq(0, 25, 5),
                     oob = scales::squish)

f1_2 <- box_plotter("lha_anti") +
  scale_y_continuous(name = "LHA Antisocial",
                     expand = c(0.02, 0),
                     limits = c(0, 20),
                     breaks = seq(0, 20, 5),
                     oob = scales::squish)

fig_1 <- f1_1 + f1_2 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "A")

ggsave(here("output/figures/fig_1_one_and_half_column_width_140mm_height_60mm_300dpi.pdf"),
       fig_1, width = 140, height = 60, units = "mm", dpi = 300)

# ==================================================================================================
# Figure 2
# ==================================================================================================

f2_1 <- line_plotter(reg_results_hdi,
                     "lha_agg",
                     "LHA Aggression")

f2_2 <- line_plotter(reg_results_hdi,
                     "lha_anti",
                     "LHA Antisocial")

fig_2 <- f2_1 + f2_2 +
  plot_layout(ncol = 2, nrow = 1) +
  plot_annotation(tag_levels = "A")

ggsave(here("output/figures/fig_2_double_column_width_190mm_height_70mm_300dpi.pdf"),
       fig_2, width = 190, height = 70, units = "mm", dpi = 300)

# ==================================================================================================
# Figure 3
# ==================================================================================================

f3_1 <- data_scaled %>%
  data_grid(lha_agg = seq_range(lha_agg, n = 100), age = 0) %>%
  add_fitted_draws(reg_results[[3]][[3]]) %>%
  # back-transform standardized values to original scale
  mutate(.value = .value * sd(data$p3_latency) + mean(data$p3_latency) -
                  (fixef(reg_results[[3]][[3]])[2, 1] * sd(data$p3_latency) * mean(data$age) / sd(data$age)) +
                  (fixef(reg_results[[3]][[3]])[3, 1] * sd(data$p3_latency) * mean(data$lha_agg) / sd(data$lha_agg)),
        lha_agg = lha_agg * attr(data_scaled$lha_agg, 'scaled:scale') + attr(data_scaled$lha_agg, 'scaled:center')) %>%
  ggplot(aes(x = lha_agg, y = p3_latency)) +
  geom_point(data = data, color = "#5b5b5b") +
  stat_lineribbon(aes(y = .value),
                  .width = c(.90),
                   point_interval = median_hdi,
                  color = "#004d00",
                  alpha = 0.8) +
  labs(x = "LHA Aggression",
       y = "NoGo P3 latency (ms)") +
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  scale_y_continuous(limits = c(400, 540), breaks = seq(400, 525, 25)) +
  scale_color_manual(values = colors_prob) +
  scale_fill_manual(values = colors_prob) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(size = title_text_size, color = "black", hjust = 0.5),
        axis.text.x = element_text(size = axis_text_size, color = "black"),
        axis.text.y = element_text(size = axis_text_size, color = "black", vjust = 0.5),
        axis.title.x = element_text(size = axis_title_size, color = "black", hjust = 0.5),
        axis.title.y = element_text(size = axis_title_size, color = "black", hjust = 0.5))

data_tmp <- data %>% select(p3_latency, lha_anti) %>% na.omit()
f3_2 <- data_scaled %>%
    select(lha_anti, p3_latency) %>%
    na.omit() %>%
  data_grid(lha_anti = seq_range(lha_anti, n = 100), age = 0) %>%
  add_fitted_draws(reg_results[[4]][[3]]) %>%
  # back-transform standardized values to original scale
  mutate(.value = .value * sd(data$p3_latency) + mean(data$p3_latency) -
                  (fixef(reg_results[[4]][[3]])[2, 1] * sd(data$p3_latency) * mean(data$age) / sd(data$age)) +
                  (fixef(reg_results[[4]][[3]])[3, 1] * sd(data_tmp$p3_latency) * mean(data_tmp$lha_anti) / sd(data_tmp$lha_anti)),
        lha_anti = lha_anti * attr(data_scaled$lha_anti, 'scaled:scale') + attr(data_scaled$lha_anti, 'scaled:center')) %>%
  # proceed with plot
  ggplot(aes(x = lha_anti, y = p3_latency)) +
  geom_point(data = data, color = "#5b5b5b") +
  stat_lineribbon(aes(y = .value),
                  .width = c(.90),
                   point_interval = median_hdi,
                  color = "#004d00",
                  alpha = 0.8) +
  labs(x = "LHA Antisocial",
       y = "NoGo P3 latency (ms)") +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) +
  scale_y_continuous(limits = c(400, 540), breaks = seq(400, 525, 25)) +
  scale_color_manual(values = colors_prob) +
  scale_fill_manual(values = colors_prob) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(size = title_text_size, color = "black", hjust = 0.5),
        axis.text.x = element_text(size = axis_text_size, color = "black"),
        axis.text.y = element_text(size = axis_text_size, color = "black", vjust = 0.5),
        axis.title.x = element_text(size = axis_title_size, color = "black", hjust = 0.5),
        axis.title.y = element_text(size = axis_title_size, color = "black", hjust = 0.5))

fig_3 <- f3_1 + f3_2 +
  plot_layout(ncol = 2, nrow = 1) +
  plot_annotation(tag_levels = "A")

ggsave(here("output/figures/fig_3_one_and_a_half_column_width_140mm_height_70mm_300dpi.pdf"),
       fig_3, width = 140, height = 70, units = "mm", dpi = 300)
