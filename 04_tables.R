# ==================================================================================================
# Table 1
# ==================================================================================================

desc_a <- data %>%
  dplyr::select(lha_agg, lha_anti) %>%
  describe() %>%
  select(mean, sd, min, max) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(variable = factor(c(
    "LHA Aggression",
    "LHA Antisocial")),
    mean = paste0(mean, " ± ", sd),
    range = paste0(min, " - ", max)) %>%
  select(variable, mean, range)

desc_c <- data %>%
  filter(group == "control") %>%
  dplyr::select(lha_agg, lha_anti) %>%
  describe() %>%
  select(mean, sd, min, max) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(variable = factor(c(
    "LHA Aggression",
    "LHA Antisocial")),
    mean = paste0(mean, " ± ", sd),
    range = paste0(min, " - ", max)) %>%
  select(variable, mean, range)

desc_p <- data %>%
  filter(group == "patient") %>%
  dplyr::select(lha_agg, lha_anti) %>%
  describe() %>%
  select(mean, sd, min, max) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(variable = factor(c(
    "LHA Aggression",
    "LHA Antisocial")),
    mean = paste0(mean, " ± ", sd),
    range = paste0(min, " - ", max)) %>%
  select(variable, mean, range)

diff <- c(paste0(round(as.numeric(grp_results_hdi[[1]]["estimate"]), 2), " [",
                 round(as.numeric(grp_results_hdi[[1]]["lower90"]), 2), ", ",
                 round(as.numeric(grp_results_hdi[[1]]["upper90"]), 2), "]"),

          paste0(round(as.numeric(grp_results_hdi[[2]]["estimate"]), 2), " [",
                 round(as.numeric(grp_results_hdi[[2]]["lower90"]), 2), ", ",
                 round(as.numeric(grp_results_hdi[[2]]["upper90"]), 2), "]"))

descriptives <- cbind(desc_a, desc_c, desc_p, diff)
rownames(descriptives) <- NULL

table_1 <- descriptives %>%
  clean_names() %>%
  dplyr::select(variable, mean, range, mean_2, range_2, mean_3, range_3, diff) %>%
  flextable() %>%
  set_header_labels(variable = "",
                    mean = "Mean ± SD",
                    range = "Range",
                    mean_2 = "Mean ± SD",
                    range_2 = "Range",
                    mean_3 = "Mean ± SD",
                    range_3 = "Range",
                    diff = "Est. diff. [90% HDI]") %>%
  add_header_row(values = c("", "Full sample (N = 46)", "CVs (N = 20)", "MDOs (N = 26)", "Estimated difference"),
                 colwidths = c(1, 2, 2, 2, 1)) %>%
  align(i = 1, part = "header", align = "center") %>%
  align(j = 2:8, part = "body", align = "center") %>%
  set_caption("Table 1. Descriptive overview and estimated group difference in aggressive and antisocial behavior.") %>%
  autofit()

table_1 <- add_footer(table_1, variable = "CVs, community volunteers; MDOs, mentally disordered offenders; SD, standard deviation; HDI, highest density interval." )
table_1 <- merge_at(table_1, j = 1:8, part = "footer")

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
    width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar())

save_as_docx(table_1, path = here("output/tables/table_1.docx"), pr_section = sect_properties)

# ==================================================================================================
# Table 2
# ==================================================================================================

order <- c("nogo_accuracy",
           "n2_latency", "n2_windowed_amplitude", "n2_moving_amplitude",
           "p3_latency", "p3_windowed_amplitude", "p3_moving_amplitude")

reg_results_agg <- reg_results_hdi %>%
    filter(predictor == "lha_agg") %>%
    select(outcome, estimate, lower90, upper90) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(estimate = paste0(estimate, " [", lower90, ", ", upper90, "]"),
           outcome = factor(outcome, levels = order, label = order)) %>%
    arrange(outcome)

rownames(reg_results_agg) <- c("NoGo accuracy",
                               "NoGo N2 latency",
                               "NoGo N2\\textsubscript{WIN}",
                               "NoGo N2\\textsubscript{MOV}",
                               "NoGo P3 latency",
                               "NoGo P3\\textsubscript{WIN}",
                               "NoGo P3\\textsubscript{MOV}")

reg_results_agg <- reg_results_agg %>% select(estimate)

reg_results_anti <- reg_results_hdi %>%
    filter(predictor == "lha_anti") %>%
    select(outcome, estimate, lower90, upper90) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(estimate = paste0(estimate, " [", lower90, ", ", upper90, "]"),
           outcome = factor(outcome, levels = order, label = order)) %>%
    arrange(outcome)

rownames(reg_results_anti) <- NULL

reg_results_anti <- reg_results_anti %>% select(estimate)

table_2 <- cbind(reg_results_agg, reg_results_anti) %>%
  clean_names() %>%
  mutate(outcome = row.names(reg_results_agg)) %>%
  select(outcome, estimate, estimate_2) %>%
  flextable() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  set_header_labels(outcome = "Outcome variable",
                    estimate = "Beta [90% HDI]",
                    estimate_2 = "Beta [90% HDI]") %>%
  add_header_row(values = c("", "LHA Aggression", "LHA Antisocial"),
                 colwidths = c(1, 1, 1)) %>%
  align(i = 1, part = "header", align = "center") %>%
  align(j = 2:3, part = "body", align = "center") %>%
  set_caption("Table 2. Standardized estimates (Beta) from robust Bayesian regression models, adjusted for age (N = 46).") %>%
  autofit()

table_2 <- add_footer(table_2, outcome = "LHA, Life History of Aggression; HDI, highest density interval. Estimates where the 90% HDI does not contain zero are shown in bold." )
table_2 <- merge_at(table_2, j = 1:3, part = "footer")

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait",
    width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar())

save_as_docx(table_2, path = here("output/tables/table_2.docx"), pr_section = sect_properties)
