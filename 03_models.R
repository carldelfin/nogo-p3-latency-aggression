# ==================================================================================================
# McDonald's Omega
# ==================================================================================================

omega_control_lha_agg <-
    data %>%
    filter(group == "control") %>%
    select(lha1:lha5) %>%
    psych::omega(1, plot = FALSE)

omega_control_lha_anti <-
    data %>%
    filter(group == "control") %>%
    select(lha8:lha11) %>%
    psych::omega(1, plot = FALSE)

omega_patient_lha_agg <-
    data %>%
    filter(group == "patient") %>%
    select(lha21:lha25) %>%
    psych::omega(1, plot = FALSE)

omega_patient_lha_anti <-
    data %>%
    filter(group == "patient") %>%
    select(lha28:lha211) %>%
    psych::omega(1, plot = FALSE)

# ==================================================================================================
# Group differences
# ==================================================================================================

if (overwrite == TRUE) {
    vars <- c("lha_agg", "lha_anti")

    grp_results_post <- lapply(vars, get_post_grp)
    grp_results_hdi <- lapply(vars, get_hdi_grp)

    saveRDS(grp_results_post, here("tmp/grp_results_post.rds"))
    saveRDS(grp_results_hdi, here("tmp/grp_results_hdi.rds"))

    rm(vars)
}

grp_results_post <- readRDS(here("tmp/grp_results_post.rds"))
grp_results_hdi <- readRDS(here("tmp/grp_results_hdi.rds"))

# ==================================================================================================
# Association between LHA and behavioral/EEG measures
# ==================================================================================================

if (overwrite == TRUE) {
    pred_vec <- c("lha_agg", "lha_anti")
    outcome_vec <- c("nogo_accuracy",
                     "p3_latency", "p3_windowed_amplitude", "p3_moving_amplitude",
                     "n2_latency", "n2_windowed_amplitude", "n2_moving_amplitude")

    grid <- expand.grid(predictor = pred_vec, outcome = outcome_vec, stringsAsFactors = FALSE)

    reg_results <- mapply(FUN = get_reg_results,
                          outcome = grid$outcome,
                          predictor = grid$predictor,
                          SIMPLIFY = FALSE)

    reg_results_hdi <- do.call(rbind, reg_results)[, 1] %>%
        rbindlist() %>%
        as.data.frame() %>%
        mutate(probability = ifelse(estimate > 0 & lower90 > 0 | estimate < 0 & upper90 < 0,
                                    "very likely",
                             ifelse(estimate > 0 & lower66 > 0 | estimate < 0 & upper66 < 0,
                                    "likely", "not likely"))) %>%
        mutate(probability = factor(probability,
                             level = c("not likely", "likely", "very likely"),
                             label = c("not likely", "likely", "very likely")))

    saveRDS(reg_results, here("tmp/reg_results.rds"))
    saveRDS(reg_results_hdi, here("tmp/reg_results_hdi.rds"))
}

reg_results <- readRDS(here("tmp/reg_results.rds"))
reg_results_hdi <- readRDS(here("tmp/reg_results_hdi.rds"))

# ==================================================================================================
# Possible group interaction
# ==================================================================================================

if (overwrite == TRUE) {

    # P3 latency and aggression
    p3_agg_loo <- loo(reg_results[[3]][[3]])
    p3_agg_looic <- p3_agg_loo$estimates[3, 1]
    p3_agg_looic_se <- p3_agg_loo$estimates[3, 2]

    saveRDS(p3_agg_looic, here("tmp/p3_agg_looic.rds"))
    saveRDS(p3_agg_looic_se, here("tmp/p3_agg_looic_se.rds"))

    p3_agg_int <- update(reg_mod,
                    formula. = bf(p3_latency ~ age + lha_agg * group),
                    newdata = data_scaled,
                    control = control,
                    cores = cores,
                    chains = chains,
                    warmup = warmup,
                    iter = iter,
                    refresh = refresh,
                    seed = seed)

    p3_agg_int_loo <- loo(p3_agg_int)
    p3_agg_int_looic <- p3_agg_int_loo$estimates[3, 1]
    p3_agg_int_looic_se <- p3_agg_int_loo$estimates[3, 2]

    saveRDS(p3_agg_int_looic, here("tmp/p3_agg_int_looic.rds"))
    saveRDS(p3_agg_int_looic_se, here("tmp/p3_agg_int_looic_se.rds"))

    # P3 latency and antisocial
    p3_anti_loo <- loo(reg_results[[4]][[3]])
    p3_anti_looic <- p3_anti_loo$estimates[3, 1]
    p3_anti_looic_se <- p3_anti_loo$estimates[3, 2]

    saveRDS(p3_anti_looic, here("tmp/p3_anti_looic.rds"))
    saveRDS(p3_anti_looic_se, here("tmp/p3_anti_looic_se.rds"))

    p3_anti_int <- update(reg_mod,
                    formula. = bf(p3_latency ~ age + lha_anti * group),
                    newdata = data_scaled,
                    control = control,
                    cores = cores,
                    chains = chains,
                    warmup = warmup,
                    iter = iter,
                    refresh = refresh,
                    seed = seed)

    p3_anti_int_loo <- loo(p3_anti_int)
    p3_anti_int_looic <- p3_anti_int_loo$estimates[3, 1]
    p3_anti_int_looic_se <- p3_anti_int_loo$estimates[3, 2]

    saveRDS(p3_anti_int_looic, here("tmp/p3_anti_int_looic.rds"))
    saveRDS(p3_anti_int_looic_se, here("tmp/p3_anti_int_looic_se.rds"))

    # N2 latency and antisocial
    n2_anti_loo <- loo(reg_results[[10]][[3]])
    n2_anti_looic <- n2_anti_loo$estimates[3, 1]
    n2_anti_looic_se <- n2_anti_loo$estimates[3, 2]

    saveRDS(n2_anti_looic, here("tmp/n2_anti_looic.rds"))
    saveRDS(n2_anti_looic_se, here("tmp/n2_anti_looic_se.rds"))

    n2_anti_int <- update(reg_mod,
                    formula. = bf(n2_latency ~ age + lha_anti * group),
                    newdata = data_scaled,
                    control = control,
                    cores = cores,
                    chains = chains,
                    warmup = warmup,
                    iter = iter,
                    refresh = refresh,
                    seed = seed)

    n2_anti_int_loo <- loo(n2_anti_int)
    n2_anti_int_looic <- n2_anti_int_loo$estimates[3, 1]
    n2_anti_int_looic_se <- n2_anti_int_loo$estimates[3, 2]

    saveRDS(n2_anti_int_looic, here("tmp/n2_anti_int_looic.rds"))
    saveRDS(n2_anti_int_looic_se, here("tmp/n2_anti_int_looic_se.rds"))

}

p3_agg_looic <- readRDS(here("tmp/p3_agg_looic.rds")) %>% round(2)
p3_agg_looic_se <- readRDS(here("tmp/p3_agg_looic_se.rds")) %>% round(2)

p3_agg_int_looic <- readRDS(here("tmp/p3_agg_int_looic.rds")) %>% round(2)
p3_agg_int_looic_se <- readRDS(here("tmp/p3_agg_int_looic_se.rds")) %>% round(2)

p3_anti_looic <- readRDS(here("tmp/p3_anti_looic.rds")) %>% round(2)
p3_anti_looic_se <- readRDS(here("tmp/p3_anti_looic_se.rds")) %>% round(2)

p3_anti_int_looic <- readRDS(here("tmp/p3_anti_int_looic.rds")) %>% round(2)
p3_anti_int_looic_se <- readRDS(here("tmp/p3_anti_int_looic_se.rds")) %>% round(2)

n2_anti_looic <- readRDS(here("tmp/n2_anti_looic.rds")) %>% round(2)
n2_anti_looic_se <- readRDS(here("tmp/n2_anti_looic_se.rds")) %>% round(2)

n2_anti_int_looic <- readRDS(here("tmp/n2_anti_int_looic.rds")) %>% round(2)
n2_anti_int_looic_se <- readRDS(here("tmp/n2_anti_int_looic_se.rds")) %>% round(2)
