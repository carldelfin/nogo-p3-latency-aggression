# ==================================================================================================
# Function to knit a Word document (.docx) from a Latex file (.tex)
# ==================================================================================================

latex2docx <- function(latex, docx, template = NULL, wd = getwd()) {

  assertthat::assert_that(assertthat::is.readable(latex))
  assertthat::assert_that(assertthat::is.dir(fs::path_dir(docx)))

  latex <- fs::path_abs(latex)
  docx <- fs::path_abs(docx)

  template <- if (!is.null(template)) {
    glue::glue("--reference-doc={fs::path_abs(template)}")
  } else {
    NULL
  }

  processx::run(
    command = "pandoc",
    args = c("-s", latex, template, "-o", docx),
    wd = wd
  )

}

# ==================================================================================================
# Get posterior distribution of group difference
# ==================================================================================================

get_post_grp <- function(outcome) {
    tmp <- data
    tmp$outcome <- data[, outcome]
    tmp$group <- data[, "group"]

    mod <- update(grp_mod, newdata = tmp, control = control, iter = iter,
                 warmup = warmup, chains = chains, cores = cores, seed = seed, refresh = refresh)
    post <- posterior_samples(mod)

    return(post)
}

# ==================================================================================================
# Get HDI from posterior distribution of group difference
# ==================================================================================================

get_hdi_grp <- function(outcome) {
    post <- get_post_grp(outcome) %>% dplyr::select(b_groupcontrol, b_grouppatient)
    diff <- post[, "b_grouppatient"] - post[, "b_groupcontrol"]
    post66 <- median_hdi(diff, .width  = .66)
    post90 <- median_hdi(diff, .width  = .90)

    out <- data.frame(outcome = outcome,
                      estimate = post66$y,
                      lower66 = post66$ymin,
                      upper66 = post66$ymax,
                      lower90 = post90$ymin,
                      upper90 = post90$ymax)

    return(out)
}

# ==================================================================================================
# Get posterior, HDI, and model from regression analyses
# ==================================================================================================

get_reg_results <- function(outcome, predictor) {

    formula_age <- bf(reformulate(termlabels = c("age"), response = outcome))

    mod_age <- update(reg_mod, formula. = formula_age, newdata = data_scaled, control = control,
    cores = cores, chains = chains, warmup = warmup, iter = iter, refresh = refresh, seed = seed)

    mod_age_loo <- loo(mod_age)
    mod_age_elpdloo <- mod_age_loo$estimates[1, 1] * -1
    mod_age_elpdloo_se <- mod_age_loo$estimates[1, 2]

    formula_full <- bf(reformulate(termlabels = c("age", predictor), response = outcome))

    mod_full <- update(reg_mod, formula. = formula_full,
                       newdata = data_scaled, control = control,
                       cores = cores, chains = chains,
                       warmup = warmup, iter = iter, 
                       refresh = refresh, seed = seed)

    mod_full_loo <- loo(mod_full)
    mod_full_elpdloo <- mod_full_loo$estimates[1, 1] * -1
    mod_full_elpdloo_se <- mod_full_loo$estimates[1, 2]

    mod_post <- posterior_samples(mod_full)

    post66 <- mean_qi(mod_post[, 3], .width  = .66)
    post90 <- mean_qi(mod_post[, 3], .width  = .90)

    mod_hdi <- data.frame(outcome = outcome,
                      predictor = predictor,
                      estimate = post66$y,
                      lower66 = post66$ymin,
                      upper66 = post66$ymax,
                      lower90 = post90$ymin,
                      upper90 = post90$ymax,
                      elpd_loo_age = mod_age_elpdloo,
                      elpd_loo_full = mod_full_elpdloo)

    out <- list(mod_hdi, mod_post, mod_full)
    return(out)
}

# ==================================================================================================
# Boxplots for each outcome variable
# ==================================================================================================

box_plotter <- function(variable) {
  ggplot(data, aes_string(x = "group",
                          y = variable,
                          color = "group",
                          fill = "group")) +
    geom_boxplot(alpha = 0.5, coef = 1.5, outlier.shape = NA) +
    geom_jitter(position = position_jitter(height = 0.1, width = 0.2), alpha = 0.8) +
    scale_x_discrete(labels = labels) +
    theme_classic() +
    scale_color_manual(values = colors, labels = labels) +
    scale_fill_manual(values = colors, labels = labels) +
    theme(legend.text = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(size = axis_text_size, color = "black"),
          axis.text.y = element_text(size = axis_text_size, color = "black", vjust = 0.5),
          plot.title = element_text(size = title_text_size, hjust = 0.5, color = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = axis_title_size, color = "black", hjust = 0.5))
}

# ==================================================================================================
# Line plot
# ==================================================================================================

order <- c("nogo_accuracy",
           "n2_latency", "n2_windowed_amplitude", "n2_moving_amplitude",
           "p3_latency", "p3_windowed_amplitude", "p3_moving_amplitude")

beta_label <- bquote(~italic(beta))

line_plotter <- function(data, pred, title) {
    data %>%
    mutate(probability = ifelse(probability == "very likely", "yes", "no")) %>%
    filter(predictor == pred) %>%
    ggplot(aes(y = factor(outcome, levels = rev(order)),
               x = estimate)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_pointrange(aes(xmin = lower90,
                        xmax = upper90,
                        color = probability), size = 1, fatten = 2) +
    scale_x_continuous(limits = c(-0.4, 0.6),
                       breaks = seq(-0.4, 0.6, 0.2),
                       labels = scales::number_format(accuracy = 0.1)) +
    scale_y_discrete(labels = rev(c("NoGo accuracy",
                                    "NoGo N2 latency",
                                    expression(paste("NoGo N2"[WIN])),
                                    expression(paste("NoGo N2"[MOV])),
                                    "NoGo P3 latency",
                                    expression(paste("NoGo P3"[WIN])),
                                    expression(paste("NoGo P3"[MOV]))))) +
    scale_color_manual(values = c("#5b5b5b", "#009A00")) +
    labs(x = beta_label,
         y = "",
         title = title) +
    theme_classic() +
    theme(legend.position = "none",
          plot.title = element_text(size = title_text_size, color = "black", hjust = 0.5),
          axis.text.x = element_text(size = axis_text_size, color = "black"),
          axis.text.y = element_text(size = axis_text_size, color = "black", vjust = 0.5),
          axis.title.x = element_text(size = axis_title_size, color = "black", hjust = 0.5),
          axis.title.y = element_text(size = axis_title_size, color = "black", hjust = 0.5))
}
