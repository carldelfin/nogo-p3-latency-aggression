# ==================================================================================================
# Various settings
# ==================================================================================================

# plots
axis_text_size <- 10
axis_title_text_size <- 10
axis_title_size <- 11
legend_text_size <- 9
title_text_size <- 12
labels <- c("HCs", "MDOs")
colors <- c("#7570B3", "#D95F02")
colors_prob <- c("#99d699", "#19a419")
colors_dx <- brewer.pal(n = 6, name = "Greys")

# brms
control <- list(max_treedepth = 15, adapt_delta = 0.9)
iter <- 4000
warmup <- 1000
cores <- 12
chains <- 12
seed <- 2021
refresh <- 0

# ==================================================================================================
# Read and process data
# ==================================================================================================

# read preprocessed data
data <- read_csv(here("data/data.csv"))

# remove participant with missing LHA data
data <-subset(data, ID != "RPK166")

# process LHA data
tmp <- data %>% filter(group == "control") %>% select(ID, starts_with("lha"))

con_lha_total <- rowSums(tmp[c("lha1", "lha2", "lha3", "lha4", "lha5",
                               "lha6", "lha7", "lha8", "lha9", "lha10", "lha11")])

con_lha_anti <- rowSums(tmp[c("lha8", "lha9", "lha10", "lha11")])
con_lha_agg <- rowSums(tmp[c("lha1", "lha2", "lha3", "lha4", "lha5")])
con_lha_self <- rowSums(tmp[c("lha6", "lha7")])

tmp <- data %>% filter(group == "patient") %>% select(ID, starts_with("lha"))

# note, this is clinical ratings
pat_lha_total <- rowSums(tmp[c("lha21", "lha22", "lha23", "lha24", "lha25", "lha26",
                               "lha27", "lha28", "lha29", "lha210", "lha211")])
pat_lha_anti <- rowSums(tmp[c("lha28", "lha29", "lha210", "lha211")])
pat_lha_agg <- rowSums(tmp[c("lha21", "lha22", "lha23", "lha24", "lha25")])
pat_lha_self <- rowSums(tmp[c("lha26", "lha27")])

data$lha_total <- c(con_lha_total, pat_lha_total)
data$lha_anti <- c(con_lha_anti, pat_lha_anti)
data$lha_agg <- c(con_lha_agg, pat_lha_agg)
data$lha_self <- c(con_lha_self, pat_lha_self)

rm(tmp, con_lha_total, con_lha_anti, con_lha_agg, con_lha_self,
   pat_lha_total, pat_lha_anti, pat_lha_agg, pat_lha_self)

data <- saveRDS(data, here("data/data.rds"))
data <- readRDS(here("data/data.rds")) %>% as.data.frame()

data_scaled <- data %>% mutate_if(is.numeric, scale)

# ==================================================================================================
# Read temporary models
# ==================================================================================================

if(overwrite == TRUE) {
    data_tmp_grp <- data.frame(outcome = rnorm(40, 0, 1),
                               group = rep(c("control", "patient"), each = 20))

    grp_mod <- brm(data = data_tmp_grp,
                   bf(outcome ~ 0 + group, sigma ~ group),
                   family = student,
                   prior = c(prior(gamma(2, 0.1), class = nu),
                             prior(normal(0, 100), class = b)),
                   control = control, iter = iter, warmup = warmup, chains = chains,
                   cores = cores, seed = seed, refresh = refresh)

    saveRDS(grp_mod, here("tmp/grp_mod.rds"))

    data_tmp_reg <- data.frame(outcome = rnorm(40, 0, 1),
                               predictor = rnorm(40, 0, 1),
                               age = sample(20:50, 20, replace = TRUE),
                               group = rep(c("control", "patient"), each = 20))

    reg_mod <- brm(outcome ~ age + predictor,
                   data = data_tmp_reg,
                   family = student,
                   prior = c(prior(normal(0, 100), class = Intercept),
                             prior(normal(0, 100), class = b),
                             prior(cauchy(0, 1), class = sigma),
                             prior(gamma(2, 0.1), class = nu)),
                   control = control, iter = iter, warmup = warmup, chains = chains,
                   cores = cores, seed = seed, refresh = refresh)

    saveRDS(reg_mod, here("tmp/reg_mod.rds"))
}

grp_mod <- readRDS(here("tmp/grp_mod.rds"))
reg_mod <- readRDS(here("tmp/reg_mod.rds"))
