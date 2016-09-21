rm(list = ls())
`%ni%` = Negate(`%in%`)

# If running the script from scratch
# load("ClimatePred3.RData")

# If script already run and resuming workspace
load(".RData")
source("~/Dropbox/R/theme_fte.R")
source("analysis_scripts/ggcorr.R")


# ---- plotting_helpers ---------------------------------------------------

var_map <- c(
  rainfall = "Rainfall",
  mean_temp = "Temperature",
  index_nino3.4 = "ENSO Index",
  spei_03 = "Drought Index"
)

term_map <- c(
  age_classjuvenile = "Juveniles vs. Adults",
  age_classnewborn = "Newborns vs. Adults",
  `value` = "Climate Variable",
  `value:age_classjuvenile` = "Juvelines:Climate",
  `value:age_classnewborn` = "Newborns:Climate"
)

site_map <- c(
  `rppn-fma` = "Muriqui",
  amboseli = "Baboon",
  kakamega = "Blue Monkey",
  gombe = "Chimpanzee",
  karisoke = "Gorilla",
  beza = "Sifaka",
  ssr = "Capuchin"
)

quarter_map <- c(
  annual = "Calendar Y",
  coldest_q = "Coldest Q",
  driest_q = "Driest Q",
  warmest_q = "Warmest Q",
  wettest_q = "Wettest Q"
)

age_map <- c(
  newborn = "Infant",
  juvenile = "Juvenile",
  adult = "Adult",
  combined = "Age Classes Combined"
)

global_labeller <- labeller(
  age_class = age_map,
  var = var_map,
  term = term_map,
  site = site_map,
  grp = site_map,
  quarter = quarter_map
)


# ---- combine_climate_survival -------------------------------------------

qua$annual <- rep(1, nrow(sites))

# Takes ~5 minutes
m <- list()
for (i in 2:ncol(qua)) {
  temp_m <- stage_specific_survival(lh, weaning_ages = weaning,
                                    census_start_month = qua[, i][[1]])
  temp_m$quarter <- paste0(names(qua[, i]), "_q")
  m[[i - 1]] <- temp_m
}

m <- bind_rows(m)
m <- m %>%
  mutate(failures = trials - successes) %>%
  dplyr::rename(site = Study.Id)

m <- complete(m, nesting(site, quarter, year_of), age_class,
                  fill = list(n_animals = 0, individual_years = 0, s = 0,
                              deaths = 0, trials = 0, successes = 0,
                              failures = 0))

m$quarter <- revalue(m$quarter, c("annual_q" = "annual"))
full_df <- inner_join(climate_predictors, m)
full_df <- filter(full_df, var %ni% c("spei_01", "spei_06", "spei_12"))

# Rescale variables (within site, var, and quarter)
full_df <- full_df %>%
  group_by(site, var, quarter) %>%
  mutate(value = scale(value)) %>%
  ungroup()

by_site_qua_var <- full_df %>%
  group_by(site, quarter, var) %>%
  arrange(site, quarter, var) %>%
  nest()

qua_df <- gather(qua, quarter, start_month, -site)
qua_df$quarter <- paste0(qua_df$quarter, "_q")


# ==== Q1
# ---- q1_models ----------------------------------------------------------

remove_years <- by_site_qua_var %>%
  unnest() %>%
  select(site, quarter, var, year_of, age_class, successes, failures, value) %>%
  complete(nesting(site, year_of, var, age_class), quarter) %>%
  filter(is.na(value)) %>%
  select(site, year_of) %>%
  distinct()

# by_site_qua_var_ac <- anti_join(full_df, remove_years) %>%
by_site_qua_var_ac <- full_df %>%
  arrange(site, quarter, var, year_of) %>%
  group_by(site, quarter, var, age_class) %>%
  nest()

get_trials <- function(df) {

  fates <- list()

  for (i in 1:nrow(df)) {
    fate <- c(rep(1L, df[i, ]$successes),
              rep(0L, df[i, ]$trials - df[i, ]$successes))

    f <- data.frame(fate = fate)

    if (nrow(f) > 0) {
      f$year_of <- df[i, ]$year_of
      f$value <- df[i, ]$value

      fates[[i]] <- f
    }

  }

  fates <- dplyr::bind_rows(fates)
  fates$year_of <- factor(fates$year_of)

  return(fates)
}

by_site_qua_var_ac <- by_site_qua_var_ac %>%
  mutate(trials = purrr::map(data, ~ get_trials(.)))

by_site_qua_var_ac <- by_site_qua_var_ac %>%
  mutate(model = purrr::map(trials, ~ glmer(fate ~ value + (1 | year_of),
                                        family = binomial, data = .)),
         null_model = purrr::map(trials, ~ glmer(fate ~ 1 + (1 | year_of),
                                             family = binomial, data = .)))

# Verify
filter(by_site_qua_var_ac, site == "ssr" & age_class == "newborn")

by_site_qua_var_ac <- by_site_qua_var_ac %>%
  mutate(AICc = purrr::map_dbl(model, MuMIn::AICc),
         null_AICc = purrr::map_dbl(null_model, MuMIn::AICc)) %>%
  arrange(site, quarter, age_class)

q1 <- by_site_qua_var_ac %>%
  select(-data, -model, -null_model) %>%
  group_by(site, quarter, age_class) %>%
  mutate(delta = AICc - null_AICc)

# q1_coef <- by_site_qua_var_ac %>%
#   unnest(model %>% purrr::map(., ~ broom::tidy(., effects = "fixed",
#                                                conf.int = TRUE))) %>%
#   filter(!str_detect(term, "Intercept"))
#
# q1_coef$p_cat <- cut(q1_coef$p, breaks = c(0, 0.05, 1), labels = c("p < 0.05", "N.S."))
# q1_coef$p_cat <- factor(q1_coef$p_cat, levels = rev(levels(q1_coef$p_cat)))

q1_coef <- by_site_qua_var_ac %>%
  filter(var != "spei_03") %>%
  unnest(model %>% purrr::map(., ~ broom::tidy(., effects = "fixed",
                                               conf.int = TRUE,
                                               conf.method = "boot",
                                               .progress="txt", PBargs=list(style = 3)))) %>%
  filter(!str_detect(term, "Intercept"))

q1_coef <- q1_coef %>%
  mutate(ci_sig = (0 < conf.low) | (0 > conf.high))

q1_coef$p_cat <- factor(q1_coef$ci_sig, levels = c("FALSE", "TRUE"),
                        labels = c("N.S.", "p < 0.05"))

# Completed


# ---- q1_plot_tile_AIC ---------------------------------------------------

temp <- filter(q1, var != "spei_03")
lim <- max(abs(temp$delta))
# temp$var <- revalue(temp$var, var_map)
temp$quarter <- revalue(temp$quarter, quarter_map)
temp$age_class <- revalue(temp$age_class, age_map)
temp$age_class <- factor(temp$age_class, levels = c("Adult", "Juvenile", "Infant"))
q1_coef$age_class <- revalue(q1_coef$age_class, age_map)
q1_coef$age_class <- factor(q1_coef$age_class, levels = c("Adult", "Juvenile", "Infant"))

ggplot(temp,
       aes(x = age_class, y = quarter, fill = delta)) +
  geom_tile(color = "black", size = 0.1) +
  facet_grid(var ~ site, switch = "y", labeller = global_labeller) +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       # trans = sqrt_sign_trans(),
                       limits = c(-lim, lim),
                       name = "AICc units from\nnull model") +
  coord_equal() +
  theme_minimal() +
  labs(x = "", y = "") +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.35, "cm"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.margin = unit(c(0.35, 0.35, 0.35, 0.35), "cm"))

ggsave("~/Desktop/Q1_AIC_Tile_Plot_All.pdf",
       width = 8, height = 8, units = "in")


# ---- q1_plot_AIC --------------------------------------------------------

lim <- max(abs(temp$delta))
fil <- "age_class == 'Infant' & var != 'spei_03'"

p1 <- ggplot(filter_(temp, fil),
       aes(x = delta, y = quarter, fill = delta)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ site, switch = "y", labeller = global_labeller) +
  scale_x_continuous(limits = c(-ceiling(lim), ceiling(lim))) +
  scale_fill_gradientn(limits = c(-lim, lim),
                       breaks = seq(-ceiling(lim), ceiling(lim), by = 2),
                       colours = brewer.pal(11, "RdGy"),
                       name = "") +
  theme_fc() +
  labs(x = "\nAICc units from the null model", y = "Climate Model\n")
       # title = stringi::stri_trans_totitle(filter_(temp, fil)$age_class[[1]]))


# ---- q1_plot_coefficients -----------------------------------------------

# lim <- max(c(abs(filter_(q1_coef, fil)$conf.low),
#              abs(filter_(q1_coef, fil)$conf.high)))

lim <- max(c(abs(q1_coef$conf.low),
             abs(q1_coef$conf.high)))

temp1 <- filter_(q1_coef, fil)
temp1$quarter <- revalue(temp1$quarter, quarter_map)

p2 <- ggplot(temp1,
       aes(x = estimate, y = quarter, fill = p_cat)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low),
                 height = 0.5, size = 0.25) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ site, switch = "y", labeller = global_labeller) +
  scale_x_continuous(limits = c(-lim, lim), trans = sqrt_sign_trans()) +
  scale_fill_manual(values = brewer.pal(3, "RdGy")[c(3, 1)],
                    name = "", drop = FALSE) +
  theme_fc() +
  theme(legend.key.width = unit(0.5, "cm")) +
  labs(x = "\nCoefficient Estimate", y = "Climate Model\n")
       # title = stringi::stri_trans_totitle(filter_(q1_coef, fil)$age_class[[1]]))

cowplot::plot_grid(p1, p2, nrow = 2, scale = 0.95, labels = c("a", "b"))

ggsave("~/Desktop/temp.pdf",
       width = 10, height = 8.5, units = "in")

# ---- site_specific_surv_plots -------------------------------------------

my_site <- "beza"
my_quarter <- "coldest_q"
my_var <- "rainfall"
my_age_class <- "newborn"

temp <- by_site_qua_var_ac %>%
  dplyr::filter(site == my_site & quarter == my_quarter & var == my_var &
                  age_class == my_age_class)
temp1 <- temp$model[[1]]
temp2 <- as.data.frame(temp$trials[[1]])
temp1@call$data <- as.name(deparse(substitute(temp2)))

t1 <- sjp.glmer(temp1, type = "fe.pc", prnt.plot = FALSE, show.ci = TRUE,
                axis.lim = c(0, 1), facet.grid = FALSE)$plot.list[[1]]

t2 <- t1$data
t3 <- temp$trials[[1]]

p1 <- ggplot(t2, aes(values, y)) +
  geom_hline(aes(yintercept = 0), color = "gray90") +
  geom_hline(aes(yintercept = 1), color = "gray90") +
  ggstance::geom_violinh(data = t3, aes(x = value, y = fate, group = fate),
                         width = 0.1, color = NA, fill = "gray90") +
  geom_point(data = t3, aes(x = value, y = fate),
             # position = position_jitter(width = 0, height = 0.05),
             size = 0.5, alpha = 0.1, shape = 3) +
  stat_smooth(method = glm, se = TRUE, n = 80, fill = "#B2182B",
              color = "#B2182B", fullrange = FALSE, level = 0.95, na.rm = FALSE,
              method.args = list(family = "binomial"), span = 0.75) +
  theme_fc() +
  labs(x = "\nScaled rainfall during coldest/driest quarter",
       y = "Predicted probability of survival\n",
       title = "Newborn Sifakas") +
  theme(legend.key.width = unit(0.75, "cm"),
        panel.grid = element_blank(),
        plot.background = element_blank())


my_site <- "kakamega"
my_quarter <- "annual"
my_var <- "rainfall"
my_age_class <- "juvenile"

temp <- by_site_qua_var_ac %>%
  dplyr::filter(site == my_site & quarter == my_quarter & var == my_var &
                  age_class == my_age_class)
temp1 <- temp$model[[1]]
temp2 <- as.data.frame(temp$trials[[1]])
temp1@call$data <- as.name(deparse(substitute(temp2)))

t1 <- sjp.glmer(temp1, type = "fe.pc", prnt.plot = FALSE, show.ci = TRUE,
                axis.lim = c(0, 1), facet.grid = FALSE)$plot.list[[1]]

t2 <- t1$data
t3 <- temp$trials[[1]]

p2 <- ggplot(t2, aes(value, y)) +
  geom_hline(aes(yintercept = 0), color = "gray90") +
  geom_hline(aes(yintercept = 1), color = "gray90") +
  ggstance::geom_violinh(data = t3, aes(x = value, y = fate, group = fate),
                         width = 0.1, color = NA, fill = "gray90") +
  geom_point(data = t3, aes(x = value, y = fate),
             size = 0.5, alpha = 0.1, shape = 3,
             position = position_jitter(width = 0, height = 0.05)) +
  stat_smooth(method = glm, se = TRUE, n = 80, fill = "#B2182B",
              color = "#B2182B", fullrange = FALSE, level = 0.95, na.rm = FALSE,
              method.args = list(family = "binomial"), span = 0.75) +
  theme_fc() +
  labs(x = "\nScaled rainfall during year",
       y = "Predicted probability of survival\n",
       title = "Juvenile Blue Monkeys") +
  theme(legend.key.width = unit(0.75, "cm"),
        panel.grid = element_blank(),
        plot.background = element_blank())

my_site <- "karisoke"
my_quarter <- "annual"
my_var <- "index_nino3.4"
my_age_class <- "adult"

temp <- by_site_qua_var_ac %>%
  dplyr::filter(site == my_site & quarter == my_quarter & var == my_var &
                  age_class == my_age_class)
temp1 <- temp$model[[1]]
temp2 <- as.data.frame(temp$trials[[1]])
temp1@call$data <- as.name(deparse(substitute(temp2)))

t1 <- sjp.glmer(temp1, type = "fe.pc", prnt.plot = FALSE, show.ci = TRUE,
                axis.lim = c(0, 1), facet.grid = FALSE)$plot.list[[1]]

t2 <- t1$data
t3 <- temp$trials[[1]]

p3 <- ggplot(t2, aes(value, y)) +
  geom_hline(aes(yintercept = 0), color = "gray90") +
  geom_hline(aes(yintercept = 1), color = "gray90") +
  ggstance::geom_violinh(data = t3, aes(x = value, y = fate, group = fate),
                         width = 0.1, color = NA, fill = "gray90") +
  geom_point(data = t3, aes(x = value, y = fate),
             size = 0.5, alpha = 0.1, shape = 3,
             position = position_jitter(width = 0, height = 0.05)) +
  stat_smooth(method = glm, se = TRUE, n = 80, fill = "#B2182B",
              color = "#B2182B", fullrange = FALSE, level = 0.95, na.rm = FALSE,
              method.args = list(family = "binomial"), span = 0.75) +
  theme_fc() +
  labs(x = "\nScaled ENSO index during year",
       y = "Predicted probability of survival\n",
       title = "Adult Gorillas") +
  theme(legend.key.width = unit(0.75, "cm"),
        panel.grid = element_blank(),
        plot.background = element_blank())

cowplot::plot_grid(p1, p2, p3, nrow = 1, scale = 0.95, labels = c("a", "b", "c"))



# ==== Q1A
# ---- q1a_models ---------------------------------------------------------

by_site_qua_var <- anti_join(full_df, remove_years) %>%
  arrange(site, quarter, var, year_of) %>%
  group_by(site, quarter, var) %>%
  nest()

get_trials <- function(df) {

  fates <- list()

  for (i in 1:nrow(df)) {
    fate <- c(rep(1L, df[i, ]$successes),
              rep(0L, df[i, ]$trials - df[i, ]$successes))

    f <- data.frame(fate = fate)

    if (nrow(f) > 0) {
      f$year_of <- df[i, ]$year_of
      f$value <- df[i, ]$value
      f$age_class <- df[i, ]$age_class

      fates[[i]] <- f
    }

  }

  fates <- dplyr::bind_rows(fates)
  fates$year_of <- factor(fates$year_of)

  return(fates)
}

by_site_qua_var <- by_site_qua_var %>%
  mutate(trials = purrr::map(data, ~ get_trials(.)))

by_site_qua_var <- by_site_qua_var %>%
  mutate(model = purrr::map(trials, ~ glmer(fate ~ value * age_class + (1 | age_class/year_of),
                                            family = binomial, data = .)),
         null_model = purrr::map(trials, ~ glmer(fate ~ age_class + (1 | age_class/year_of),
                                             family = binomial, data = .)))

by_site_qua_var <- by_site_qua_var %>%
  mutate(AICc = purrr::map_dbl(model, MuMIn::AICc),
         null_AICc = purrr::map_dbl(null_model, MuMIn::AICc)) %>%
  arrange(site, quarter)

q1a <- by_site_qua_var %>%
  select(-data, -model, -null_model) %>%
  group_by(site, quarter) %>%
  mutate(delta = AICc - null_AICc)

q1a_coef <- by_site_qua_var %>%
  unnest(model %>% purrr::map(., ~ broom::tidy(., effects = "fixed",
                                               # conf.method = "profile",
                                               conf.int = TRUE))) %>%
  filter(!str_detect(term, "Intercept"))

q1a_coef$p_cat <- cut(q1a_coef$p.value, breaks = c(0, 0.05, 1), labels = c("p < 0.05", "N.S."))
q1a_coef$p_cat <- factor(q1a_coef$p_cat, levels = rev(levels(q1a_coef$p_cat)))

temp <- filter(q1a, var != "spei_03")
# temp <- filter(temp, quarter %ni% c("warmest_q", "coldest_q"))
# temp$var <- mapvalues(temp$var,
#                      from = c("index_nino3.4", "mean_temp", "rainfall"),
#                      to = c("Niño3.4", "Tavg", "Rainfall"))
temp$quarter <- revalue(temp$quarter, quarter_map)
# temp$site <- revalue(temp$site, site_map)
lim <- max(abs(q1$delta))

ggplot(temp, aes(x = site, y = quarter, fill = delta)) +
  geom_tile(color = "black", size = 0.1) +
  facet_grid(var ~ ., switch = "y", labeller = global_labeller) +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       limits = c(-lim, lim),
                       name = "AICc units from\nnull model") +
  coord_equal() +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.35, "cm"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.margin = unit(c(0.35, 0.35, 0.35, 0.35), "cm")) +
  labs(x = "", y = "")

ggplot(temp,
       aes(x = delta, y = quarter, fill = delta)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ site, switch = "y", labeller = global_labeller) +
  scale_x_continuous(#breaks = seq(-ceiling(lim), ceiling(lim), by = 4),
                     limits = c(-ceiling(lim), ceiling(lim))) +
  scale_fill_gradientn(limits = c(-lim, lim),
                       breaks = seq(-ceiling(lim), ceiling(lim), by = 2),
                       colours = brewer.pal(11, "RdGy"),
                       name = "", guide = FALSE) +
  theme_fc() +
  labs(x = "\nAICc units from the null model", y = "Climate Model\n")

fil <- "site == 'beza'"

lim <- max(abs(filter_(q1a_coef, fil)$conf.high),
           abs(filter_(q1a_coef, fil)$conf.low))

ggplot(filter_(q1a_coef, fil),
       aes(x = estimate, y = quarter, fill = p_cat)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low),
                 height = 0.5, size = 0.25) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ term, switch = "y", labeller = global_labeller) +
  scale_x_continuous(limits = c(-lim, lim)) +
  scale_fill_manual(values = brewer.pal(3, "RdGy")[c(3, 1)],
                    name = "", drop = FALSE) +
  theme_fc() +
  theme(legend.key.width = unit(0.5, "cm")) +
  labs(x = "\nCoefficient Estimate", y = "Climate Model\n",
       title = stringi::stri_trans_totitle(filter_(q1a_coef, fil)$site[[1]]))

ggplot(filter(q1a_coef, term == "value"),
       aes(x = estimate, y = quarter, fill = p_cat)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low),
                 height = 0.5, size = 0.25) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ site, switch = "y", labeller = global_labeller) +
  # scale_x_continuous(limits = c(-lim, lim)) +
  scale_fill_manual(values = brewer.pal(3, "RdGy")[c(3, 1)],
                    name = "", drop = FALSE) +
  theme_fc() +
  theme(legend.key.width = unit(0.5, "cm")) +
  labs(x = "\nCoefficient Estimate", y = "Climate Model\n")


# ==== Q2
# ---- q2_models ----------------------------------------------------------

by_qua_ac <- full_df %>%
  spread(var, value) %>%
  group_by(quarter, age_class) %>%
  nest()

get_trials <- function(df) {

  fates <- list()

  for (i in 1:nrow(df)) {
    fate <- c(rep(1L, df[i, ]$successes),
              rep(0L, df[i, ]$trials - df[i, ]$successes))

    f <- data.frame(fate = fate)

    if (nrow(f) > 0) {
      f$year_of <- df[i, ]$year_of
      f$index_nino3.4 <- df[i, ]$index_nino3.4
      f$mean_temp <- df[i, ]$mean_temp
      f$rainfall <- df[i, ]$rainfall
      f$spei_03 <- df[i, ]$spei_03
      f$site <- df[i, ]$site

      fates[[i]] <- f
    }

  }

  fates <- dplyr::bind_rows(fates)
  fates$year_of <- factor(fates$year_of)

  return(fates)
}

by_qua_ac <- by_qua_ac %>%
  mutate(trials = purrr::map(data, ~ get_trials(.)))

by_qua_ac <- by_qua_ac %>%
  mutate(null_model = purrr::map(trials, ~ glmer(fate ~ site + (1 | site/year_of),
                                             family = binomial, data = .)),
         ind_model = purrr::map(trials, ~ glmer(fate ~ site * index_nino3.4 + (1 | site/year_of),
                                            family = binomial, data = .)),
         temp_model = purrr::map(trials, ~ glmer(fate ~ site * mean_temp + (1 | site/year_of),
                                             family = binomial, data = .)),
         rainfall_model = purrr::map(trials, ~ glmer(fate ~ site * rainfall + (1 | site/year_of),
                                                 family = binomial, data = .)),
         drought_model = purrr::map(trials, ~ glmer(fate ~ site * spei_03 + (1 | site/year_of),
                                                 family = binomial, data = .)))

q2 <- by_qua_ac %>%
  mutate(null_AICc = purrr::map_dbl(null_model, MuMIn::AICc),
         index_nino3.4 = purrr::map_dbl(ind_model, MuMIn::AICc),
         mean_temp = purrr::map_dbl(temp_model, MuMIn::AICc),
         rainfall = purrr::map_dbl(rainfall_model, MuMIn::AICc),
         spei_03 = purrr::map_dbl(drought_model, MuMIn::AICc)) %>%
  select(-contains("model"), -data, -trials) %>%
  gather(var, AICc, -quarter, -age_class, -null_AICc) %>%
  mutate(delta = AICc - null_AICc)

lim <- max(abs(q2$delta))
temp <- q2
temp$quarter <- revalue(temp$quarter, quarter_map)
temp <- filter(temp, var != "spei_03")

p1 <- ggplot(temp,
       aes(x = delta, y = quarter, fill = delta)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ age_class, switch = "y", labeller = global_labeller) +
  scale_x_continuous(limits = c(-ceiling(lim), ceiling(lim))) +
  scale_fill_gradientn(limits = c(-lim, lim),
                       breaks = seq(-ceiling(lim), ceiling(lim), by = 2),
                       colours = brewer.pal(11, "RdGy"),
                       name = "", guide = FALSE) +
  theme_fc() +
  labs(x = "\nAICc units from the null model", y = "Climate Model\n",
       title = "Survival")

# sjPlot::sjp.glmer(by_qua_ac$ind_model[[8]], printPlot = FALSE)$plot + theme_fc()


# ---- q2a_models ---------------------------------------------------------

by_qua <- full_df %>%
  spread(var, value) %>%
  group_by(quarter) %>%
  nest()

get_trials <- function(df) {

  fates <- list()

  for (i in 1:nrow(df)) {
    fate <- c(rep(1L, df[i, ]$successes),
              rep(0L, df[i, ]$trials - df[i, ]$successes))

    f <- data.frame(fate = fate)

    if (nrow(f) > 0) {
      f$year_of <- df[i, ]$year_of
      f$index_nino3.4 <- df[i, ]$index_nino3.4
      f$mean_temp <- df[i, ]$mean_temp
      f$rainfall <- df[i, ]$rainfall
      f$spei_03 <- df[i, ]$spei_03
      f$site <- df[i, ]$site
      f$age_class <- df[i, ]$age_class

      fates[[i]] <- f
    }

  }

  fates <- dplyr::bind_rows(fates)
  fates$year_of <- factor(fates$year_of)

  return(fates)
}

by_qua <- by_qua %>%
  mutate(trials = purrr::map(data, ~ get_trials(.)))

by_qua <- by_qua %>%
  mutate(null_model = map(trials, ~ glmer(fate ~ site * age_class + (1 | site/age_class/year_of),
                                          family = binomial, data = .)),
         ind_model = purrr::map(trials, ~ glmer(fate ~ (site + age_class + index_nino3.4) ^ 2 + (1 | site/age_class/year_of),
                                                family = binomial, data = .)),
         temp_model = purrr::map(trials, ~ glmer(fate ~ (site + age_class + mean_temp) ^ 2 + (1 | site/age_class/year_of),
                                                 family = binomial, data = .)),
         rainfall_model = purrr::map(trials, ~ glmer(fate ~ (site + age_class + rainfall) ^ 2 + (1 | site/age_class/year_of),
                                                     family = binomial, data = .)))

q2a <- by_qua %>%
  mutate(null_AICc = purrr::map_dbl(null_model, MuMIn::AICc),
         index_nino3.4 = purrr::map_dbl(ind_model, MuMIn::AICc),
         mean_temp = purrr::map_dbl(temp_model, MuMIn::AICc),
         rainfall = purrr::map_dbl(rainfall_model, MuMIn::AICc)) %>%
  select(-contains("model"), -data, -trials) %>%
  gather(var, AICc, -quarter, -null_AICc) %>%
  mutate(delta = AICc - null_AICc)

lim <- max(abs(q2a$delta))
temp <- q2a
temp$quarter <- revalue(temp$quarter, quarter_map)
temp <- filter(temp, var != "spei_03")

ggplot(temp,
       aes(x = delta, y = quarter, fill = delta)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ ., switch = "y", labeller = global_labeller) +
  scale_x_continuous(limits = c(-ceiling(lim), ceiling(lim))) +
  scale_fill_gradientn(limits = c(-lim, lim),
                       colours = brewer.pal(11, "RdGy"),
                       name = "", guide = FALSE) +
  theme_fc() +
  labs(x = "\nAICc units from the null model", y = "Climate Model\n")


# ---- fert_all_females ---------------------------------------------------

# Takes ~11 minutes
f <- list()

# Set up a progress bar
pb <- txtProgressBar(min = 1, max = ncol(qua), style = 3)
n <- 1

# Set up a timer
ptm <- proc.time()

for (i in 2:ncol(qua)) {

  if (i == 2) {
    setTxtProgressBar(pb, 1)
  }

  temp_f <- stage_specific_fertility(lh, fert, adult_definition = "minimum",
                                     weaning_ages = weaning,
                                     census_start_month = qua[, i][[1]])
  temp_f$quarter <- paste0(names(qua[, i]), "_q")
  f[[i - 1]] <- temp_f

  # n is a counter that updates the progress bar
  n <- n + 1
  setTxtProgressBar(pb, n)
}

# Evaluate timer and close progress bar
dur <- proc.time() - ptm
close(pb)

f <- bind_rows(f)
f <- f %>%
  mutate(successes = ifelse(trials - successes >= 0, successes, trials),
         failures = trials - successes) %>%
  dplyr::rename(site = Study.Id)


# ---- fert_fertile_only --------------------------------------------------

# Fertile only
new_fert <- fertile_periods(lh, fert, weaning)

# Takes ~11 minutes
ff <- list()

# Set up a progress bar
pb <- txtProgressBar(min = 1, max = ncol(qua), style = 3)
n <- 1

# Set up a timer
ptm <- proc.time()

for (i in 2:ncol(qua)) {

  if (i == 2) {
    setTxtProgressBar(pb, 1)
  }

  temp_f <- stage_specific_fertility(lh, new_fert, adult_definition = "minimum",
                                     weaning_ages = weaning,
                                     census_start_month = qua[, i][[1]])

  temp_f$quarter <- paste0(names(qua[, i]), "_q")
  ff[[i - 1]] <- temp_f

  # n is a counter that updates the progress bar
  n <- n + 1
  setTxtProgressBar(pb, n)
}

# Evaluate timer and close progress bar
dur <- proc.time() - ptm
close(pb)

ff <- bind_rows(ff)
ff <- ff %>%
  mutate(successes = ifelse(trials - successes >= 0, successes, trials),
         failures = trials - successes) %>%
  dplyr::rename(site = Study.Id) %>%
  filter(trials >= 1)


# ---- combine_climate_fertility ------------------------------------------

# Adjust year_of for climate predictors so that lag1 is matched to fertility
# temp <- mutate(climate_predictors, year_of = year_of)
temp <- climate_predictors

f$quarter <- revalue(f$quarter, c("annual_q" = "annual"))
full_f_df <- inner_join(temp, f)
full_f_df <- filter(full_f_df, var %ni% c("spei_01", "spei_06", "spei_12") & age_class == "adult")

# Rescale variables (within site, var, and quarter)
full_f_df <- full_f_df %>%
  group_by(site, var, quarter) %>%
  mutate(value = scale(value)) %>%
  ungroup()

full_f_df <- full_f_df %>%
  group_by(site, quarter, var) %>%
  arrange(site, quarter, var, year_of)

by_site_qua_var_f <- full_f_df %>%
  group_by(site, quarter, var) %>%
  arrange(site, quarter, var) %>%
  nest()


# ff$quarter <- revalue(ff$quarter, c("annual_q" = "annual"))
# full_ff_df <- inner_join(temp, ff)
# full_ff_df <- filter(full_ff_df, var %ni% c("spei_01", "spei_06", "spei_12") & age_class == "adult")
#
# # Rescale variables (within site, var, and quarter)
# full_ff_df <- full_ff_df %>%
#   group_by(site, var, quarter) %>%
#   mutate(value = scale(value)) %>%
#   ungroup()
#
# full_ff_df <- full_ff_df %>%
#   group_by(site, quarter, var) %>%
#   arrange(site, quarter, var, year_of)
#
# by_site_qua_var_ff <- full_ff_df %>%
#   group_by(site, quarter, var) %>%
#   arrange(site, quarter, var) %>%
#   nest()


# ---- q1_models_f --------------------------------------------------------

get_trials <- function(df) {

  fates <- list()

  for (i in 1:nrow(df)) {
    fate <- c(rep(1L, df[i, ]$successes),
              rep(0L, df[i, ]$trials - df[i, ]$successes))

    f <- data.frame(fate = fate)

    if (nrow(f) > 0) {
      f$year_of <- df[i, ]$year_of
      f$value <- df[i, ]$value

      fates[[i]] <- f
    }

  }

  fates <- dplyr::bind_rows(fates)
  fates$year_of <- factor(fates$year_of)

  return(fates)
}

by_site_qua_var_f <- by_site_qua_var_f %>%
  mutate(trials = purrr::map(data, ~ get_trials(.)))

by_site_qua_var_f <- by_site_qua_var_f %>%
  mutate(model = purrr::map(trials, ~ glmer(fate ~ value + (1 | year_of),
                                        family = binomial, data = .)),
         null_model = purrr::map(trials, ~ glmer(fate ~ 1 + (1 | year_of),
                                             family = binomial, data = .)))

# Verify
filter(by_site_qua_var_f, site == "ssr")

by_site_qua_var_f <- by_site_qua_var_f %>%
  mutate(AICc = purrr::map_dbl(model, MuMIn::AICc),
         null_AICc = purrr::map_dbl(null_model, MuMIn::AICc)) %>%
  arrange(site, quarter)

q1_f <- by_site_qua_var_f %>%
  select(-data, -model, -null_model) %>%
  group_by(site, quarter) %>%
  mutate(delta = AICc - null_AICc)

q1_f_coef <- by_site_qua_var_f %>%
  filter(var != "spei_03") %>%
  unnest(model %>% purrr::map(., ~ broom::tidy(., effects = "fixed",
                                               conf.int = TRUE,
                                               conf.method = "boot",
                                               .progress="txt", PBargs=list(style=3)))) %>%
  filter(!str_detect(term, "Intercept"))

q1_f_coef <- q1_f_coef %>%
  mutate(ci_sig = (0 < conf.low) | (0 > conf.high))

q1_f_coef$p_cat <- factor(q1_f_coef$ci_sig, levels = c("FALSE", "TRUE"),
                        labels = c("N.S.", "p < 0.05"))

# q1_f_coef <- by_site_qua_var_f %>%
#   unnest(model %>% purrr::map(., ~ broom::tidy(., effects = "fixed",
#                                                conf.int = TRUE))) %>%
#   filter(!str_detect(term, "Intercept"))
#
# q1_f_coef$p_cat <- cut(q1_f_coef$p, breaks = c(0, 0.05, 1), labels = c("p < 0.05", "N.S."))
# q1_f_coef$p_cat <- factor(q1_f_coef$p_cat, levels = rev(levels(q1_f_coef$p_cat)))

lim <- max(abs(q1_f$delta))

ggplot(filter(q1_f, var != "spei_03"),
       aes(x = site, y = quarter, fill = delta)) +
  geom_tile(color = "black", size = 0.1) +
  facet_grid(var ~ ., switch = "y", labeller = global_labeller) +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       # trans = sqrt_sign_trans(),
                       limits = c(-lim, lim),
                       name = "AICc units from\nnull model") +
  coord_equal() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.35, "cm"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.margin = unit(c(0.35, 0.35, 0.35, 0.35), "cm"))


# ---- q1_f_plot_AIC ------------------------------------------------------

temp <- filter(q1_f, var != "spei_03")
temp$quarter <- revalue(temp$quarter, quarter_map)
lim <- max(abs(q1_f$delta))

p1 <- ggplot(temp,
       aes(x = delta, y = quarter, fill = delta)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ site, switch = "y", labeller = global_labeller) +
  scale_x_continuous(#breaks = c(-11, -7, -4, -2, 0, 2, 4, 11),
    # breaks = seq(-ceiling(lim), ceiling(lim), by = 2),
    limits = c(-ceiling(lim), ceiling(lim))) +
  scale_fill_gradientn(limits = c(-lim, lim),
                       colours = brewer.pal(11, "RdGy"),
                       # trans = sqrt_sign_trans(),
                       name = "") +
  theme_fc() +
  labs(x = "\nAICc units from the null model", y = "Climate Model\n")


# ---- q1_f_plot_coefficients ---------------------------------------------

lim <- max(c(abs(q1_f_coef$conf.low),
             abs(q1_f_coef$conf.high)))

temp <- filter(q1_f_coef, var != "spei_03")
temp$quarter <- revalue(temp$quarter, quarter_map)

p2 <- ggplot(temp,
       aes(x = estimate, y = quarter, fill = p_cat)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low),
                 height = 0.5, size = 0.25) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ site, switch = "y", labeller = global_labeller) +
  scale_x_continuous(limits = c(-lim, lim)) +
  scale_fill_manual(values = brewer.pal(3, "RdGy")[c(3, 1)],
                    name = "", drop = FALSE) +
  theme_fc() +
  theme(legend.key.width = unit(0.5, "cm")) +
  labs(x = "\nCoefficient Estimate", y = "Climate Model\n")

cowplot::plot_grid(p1, p2, nrow = 2, scale = 0.95, labels = c("a", "b"))
ggsave("~/Desktop/temp.pdf", width = 10, height = 8.5, units = "in")


# ---- q1_f_curve ---------------------------------------------------------

my_site <- "ssr"
my_quarter <- "driest_q"
my_var <- "index_nino3.4"

temp <- by_site_qua_var_f %>%
  dplyr::filter(site == my_site & quarter == my_quarter & var == my_var)
temp1 <- temp$model[[1]]
temp2 <- as.data.frame(temp$trials[[1]])
temp1@call$data <- as.name(deparse(substitute(temp2)))

t1 <- sjp.glmer(temp1, type = "fe.pc", prnt.plot = FALSE, show.ci = TRUE,
                axis.lim = c(0, 1), facet.grid = FALSE)$plot.list[[1]]

t2 <- t1$data
t3 <- temp$trials[[1]]

ggplot(t2, aes(values, y)) +
  geom_hline(aes(yintercept = 0), color = "gray90") +
  geom_hline(aes(yintercept = 1), color = "gray90") +
  ggstance::geom_violinh(data = t3, aes(x = value, y = fate, group = fate),
                         width = 0.1, color = NA, fill = "gray90") +
  geom_point(data = t3, aes(x = value, y = fate),
             # position = position_jitter(width = 0, height = 0.05),
             size = 0.5, alpha = 0.1, shape = 3) +
  stat_smooth(method = glm, se = TRUE, n = 80, fill = "#B2182B",
              color = "#B2182B", fullrange = FALSE, level = 0.95, na.rm = FALSE,
              method.args = list(family = "binomial"), span = 0.75) +
  theme_fc() +
  labs(x = "\nScaled ENSO index during driest quarter",
       y = "Predicted probability of reproducing\n",
       title = "Female Capuchin Fertility") +
  theme(legend.key.width = unit(0.75, "cm"),
        panel.grid = element_blank(),
        plot.background = element_blank())

# ---- q1_models_ff -------------------------------------------------------

get_trials <- function(df) {

  fates <- list()

  for (i in 1:nrow(df)) {
    fate <- c(rep(1L, df[i, ]$successes),
              rep(0L, df[i, ]$trials - df[i, ]$successes))

    f <- data.frame(fate = fate)

    if (nrow(f) > 0) {
      f$year_of <- df[i, ]$year_of
      f$value <- df[i, ]$value

      fates[[i]] <- f
    }

  }

  fates <- dplyr::bind_rows(fates)
  fates$year_of <- factor(fates$year_of)

  return(fates)
}

by_site_qua_var_ff <- by_site_qua_var_ff %>%
  mutate(trials = purrr::map(data, ~ get_trials(.)))

by_site_qua_var_ff <- by_site_qua_var_ff %>%
  mutate(model = purrr::map(trials, ~ glmer(fate ~ value + (1 | year_of),
                                            family = binomial, data = .)),
         null_model = purrr::map(trials, ~ glmer(fate ~ 1 + (1 | year_of),
                                                 family = binomial, data = .)))

# Verify
# filter(by_site_qua_var_ff, site == "ssr")

by_site_qua_var_ff <- by_site_qua_var_ff %>%
  mutate(AICc = purrr::map_dbl(model, MuMIn::AICc),
         null_AICc = purrr::map_dbl(null_model, MuMIn::AICc)) %>%
  arrange(site, quarter)

q1_ff <- by_site_qua_var_ff %>%
  select(-data, -model, -null_model) %>%
  group_by(site, quarter) %>%
  mutate(delta = AICc - null_AICc)

q1_ff_coef <- by_site_qua_var_ff %>%
  unnest(model %>% purrr::map(., ~ broom::tidy(., effects = "fixed",
                                               conf.int = TRUE))) %>%
  filter(!str_detect(term, "Intercept"))

q1_ff_coef$p_cat <- cut(q1_ff_coef$p, breaks = c(0, 0.05, 1), labels = c("p < 0.05", "N.S."))
q1_ff_coef$p_cat <- factor(q1_ff_coef$p_cat, levels = rev(levels(q1_ff_coef$p_cat)))


lim <- max(abs(q1_ff$delta))

ggplot(q1_ff, aes(x = site, y = quarter, fill = delta)) +
  geom_tile(color = "white", size = 0.1) +
  facet_grid(var ~ ., switch = "y", labeller = global_labeller) +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       # trans = sqrt_sign_trans(),
                       limits = c(-lim, lim),
                       name = "AICc units from\nnull model") +
  coord_equal() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.35, "cm"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.margin = unit(c(0.35, 0.35, 0.35, 0.35), "cm"))

t4 <- filter(by_site_qua_var_ff, quarter == "wettest_q" & site == "ssr" & var == "rainfall")$model[[1]]
sjp.glmer(t4, type = "fe.pc", printPlot = FALSE, show.ci = TRUE)$plot.mp[[1]] + theme_fc()


# ---- q1_ff_plot_AIC -----------------------------------------------------

lim <- max(abs(q1_ff$delta))
temp <- filter(q1_ff, var != "spei_03")
temp$quarter <- revalue(temp$quarter, quarter_map)

p1 <- ggplot(temp,
       aes(x = delta, y = quarter, fill = delta)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ site, switch = "y", labeller = global_labeller) +
  scale_x_continuous(#breaks = c(-11, -7, -4, -2, 0, 2, 4, 11),
    # breaks = seq(-ceiling(lim), ceiling(lim), by = 2),
    limits = c(-ceiling(lim), ceiling(lim))) +
  scale_fill_gradientn(limits = c(-lim, lim),
                       colours = brewer.pal(11, "RdGy"),
                       # trans = sqrt_sign_trans(),
                       name = "") +
  theme_fc() +
  labs(x = "\nAICc units from the null model", y = "Climate Model\n")


# ---- q1_ff_plot_coefficients --------------------------------------------

lim <- max(c(abs(q1_ff_coef$conf.low),
             abs(q1_ff_coef$conf.high)))

temp <- filter(q1_ff_coef, var != "spei_03")
temp$quarter <- revalue(temp$quarter, quarter_map)

p2 <- ggplot(temp,
       aes(x = estimate, y = quarter, fill = p_cat)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low),
                 height = 0.5, size = 0.25) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ site, switch = "y", labeller = global_labeller) +
  scale_x_continuous(limits = c(-lim, lim)) +
  scale_fill_manual(values = brewer.pal(3, "RdGy")[c(3, 1)],
                    name = "", drop = FALSE) +
  theme_fc() +
  theme(legend.key.width = unit(0.5, "cm")) +
  labs(x = "\nCoefficient Estimate", y = "Climate Model\n")

cowplot::plot_grid(p1, p2, nrow = 2, scale = 0.95, labels = c("a", "b"))


# ==== Q2_f
# ---- q2_f_models --------------------------------------------------------

by_qua_f <- full_f_df %>%
  spread(var, value) %>%
  group_by(quarter) %>%
  nest()

get_trials <- function(df) {

  fates <- list()

  for (i in 1:nrow(df)) {
    fate <- c(rep(1L, df[i, ]$successes),
              rep(0L, df[i, ]$trials - df[i, ]$successes))

    f <- data.frame(fate = fate)

    if (nrow(f) > 0) {
      f$site <- df[i, ]$site
      f$year_of <- df[i, ]$year_of
      f$index_nino3.4 <- df[i, ]$index_nino3.4
      f$mean_temp <- df[i, ]$mean_temp
      f$rainfall <- df[i, ]$rainfall
      f$spei_03 <- df[i, ]$spei_03

      fates[[i]] <- f
    }

  }

  fates <- dplyr::bind_rows(fates)
  fates$year_of <- factor(fates$year_of)

  return(fates)
}

by_qua_f <- by_qua_f %>%
  mutate(trials = purrr::map(data, ~ get_trials(.)))

by_qua_f <- by_qua_f %>%
  mutate(null_model = purrr::map(trials, ~ glmer(fate ~ site + (1 | site/year_of),
                                             family = binomial, data = .)),
         ind_model = purrr::map(trials, ~ glmer(fate ~ site * index_nino3.4 + (1 | site/year_of),
                                            family = binomial, data = .)),
         temp_model = purrr::map(trials, ~ glmer(fate ~ site * mean_temp + (1 | site/year_of),
                                             family = binomial, data = .)),
         rainfall_model = purrr::map(trials, ~ glmer(fate ~ site * rainfall + (1 | site/year_of),
                                                 family = binomial, data = .)),
         drought_model = purrr::map(trials, ~ glmer(fate ~ site * spei_03 + (1 | site/year_of),
                                                   family = binomial, data = .)))

q2_f <- by_qua_f %>%
  mutate(null_AICc = purrr::map_dbl(null_model, MuMIn::AICc),
         index_nino3.4 = purrr::map_dbl(ind_model, MuMIn::AICc),
         mean_temp = purrr::map_dbl(temp_model, MuMIn::AICc),
         rainfall = purrr::map_dbl(rainfall_model, MuMIn::AICc),
         spei_03 = purrr::map_dbl(drought_model, MuMIn::AICc)) %>%
  select(-matches("model"), -data, -trials) %>%
  gather(var, AICc, -quarter, -null_AICc) %>%
  mutate(delta = AICc - null_AICc)


lim <- max(abs(q2a$delta))
temp <- q2
temp$quarter <- revalue(temp$quarter, quarter_map)
temp <- filter(temp, var != "spei_03")

p1 <- ggplot(temp,
             aes(x = delta, y = quarter, fill = delta)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ age_class, switch = "y", labeller = global_labeller) +
  scale_x_continuous(limits = c(-7, ceiling(lim))) +
  scale_fill_gradientn(limits = c(-lim, lim),
                       breaks = seq(-ceiling(lim), ceiling(lim), by = 2),
                       colours = brewer.pal(11, "RdGy"),
                       name = "", guide = FALSE) +
  theme_fc() +
  labs(x = "\nAICc units from the null model", y = "Climate Model\n",
       title = "Survival")

# lim <- max(abs(q2a$delta))
temp <- q2a
temp$quarter <- revalue(temp$quarter, quarter_map)
temp <- filter(temp, var != "spei_03")
temp$age_class <- "combined"

p2 <- ggplot(temp,
       aes(x = delta, y = quarter, fill = delta)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ age_class, switch = "y", labeller = global_labeller) +
  scale_x_continuous(limits = c(-ceiling(lim), ceiling(lim))) +
  scale_fill_gradientn(limits = c(-lim, lim),
                       colours = brewer.pal(11, "RdGy"),
                       name = "", guide = FALSE) +
  theme_fc() +
  labs(x = "\nAICc units from the null model", y = "Climate Model\n",
       title = "Survival")

# lim <- max(abs(q2_f$delta))
q2_f$quarter <- revalue(q2_f$quarter, quarter_map)
q2_f$age_class <- "adult"

p3 <- ggplot(filter(q2_f, var != "spei_03"),
       aes(x = delta, y = quarter, fill = delta)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ age_class, switch = "y", labeller = global_labeller) +
  scale_x_continuous(limits = c(-7, ceiling(lim))) +
  scale_fill_gradientn(limits = c(-lim, lim),
                       trans = sqrt_sign_trans(),
                       colours = brewer.pal(11, "RdGy"),
                       name = "", guide = FALSE) +
  theme_fc() +
  labs(x = "\nAICc units from the null model", y = "Climate Model\n",
       title = "Fertility")

cowplot::ggdraw() +
  cowplot::draw_plot(p1, 0, 0, 0.50, 1) +
  cowplot::draw_plot(p2, 0.50, 0, .25, 1) +
  cowplot::draw_plot(p3, 0.75, 0, .25, 1) +
  cowplot::draw_plot_label(c("a", "b", "c"), c(0, 0.50, 0.75), c(1, 1, 1), size = 15)

ggsave("~/Desktop/q2-plots.pdf",
       width = 11, height = 4, units = "in")


# q2_f_coef <- by_qua_f %>%
#   mutate(ind_params = purrr::map(ind_model, ~ broom::tidy(., effects = "fixed",
#                                                           conf.int = TRUE)),
#          temp_params = purrr::map(temp_model, ~ broom::tidy(., effects = "fixed",
#                                                             conf.int = TRUE)),
#          rainfall_params = purrr::map(rainfall_model, ~ broom::tidy(., effects = "fixed",
#                                                                     conf.int = TRUE))) %>%
#   select(-data, -trials, -contains("model"))
#
# t1 <- q2_f_coef %>%
#   select(quarter, ind_params) %>%
#   mutate(var = "index_nino3.4") %>%
#   unnest() %>%
#   filter(term != "(Intercept)")
#
# t2 <- q2_f_coef %>%
#   select(quarter, temp_params) %>%
#   mutate(var = "mean_temp") %>%
#   unnest() %>%
#   filter(term != "(Intercept)")
#
# t3 <- q2_f_coef %>%
#   select(quarter, rainfall_params) %>%
#   mutate(var = "rainfall") %>%
#   unnest() %>%
#   filter(term != "(Intercept)")
#
# q2_f_coef <- bind_rows(t1, t2, t3)
# rm(t1, t2, t3)
#
# q2_f_coef$p_cat <- cut(q2_f_coef$p, breaks = c(0, 0.05, 1), labels = c("p < 0.05", "N.S."))
# q2_f_coef$p_cat <- factor(q2_f_coef$p_cat, levels = rev(levels(q2_f_coef$p_cat)))
#
# lim <- max(c(abs(q2_f_coef$conf.low),
#              abs(q2_f_coef$conf.high)))
#
# ggplot(filter(q2_f_coef, term %in% c("rainfall", "index_nino3.4", "mean_temp")),
#        aes(x = estimate, y = quarter, fill = p_cat)) +
#   geom_vline(xintercept = 0, lty = 3) +
#   geom_errorbarh(aes(xmax = conf.high, xmin = conf.low),
#                  height = 0.5, size = 0.25) +
#   geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
#   facet_grid(var ~ ., switch = "y", labeller = global_labeller) +
#   scale_x_continuous(limits = c(-lim, lim)) +
#   scale_fill_manual(values = brewer.pal(3, "RdGy")[c(3, 1)],
#                     name = "", drop = FALSE) +
#   theme_fc() +
#   theme(legend.key.width = unit(0.5, "cm")) +
#   labs(x = "\nCoefficient Estimate", y = "Climate Model\n")


# ==== Q2_ff
# ---- q2_ff_models -------------------------------------------------------

by_qua_ff <- full_ff_df %>%
  spread(var, value) %>%
  group_by(quarter) %>%
  nest()

get_trials <- function(df) {

  fates <- list()

  for (i in 1:nrow(df)) {
    fate <- c(rep(1L, df[i, ]$successes),
              rep(0L, df[i, ]$trials - df[i, ]$successes))

    f <- data.frame(fate = fate)

    if (nrow(f) > 0) {
      f$site <- df[i, ]$site
      f$year_of <- df[i, ]$year_of
      f$index_nino3.4 <- df[i, ]$index_nino3.4
      f$mean_temp <- df[i, ]$mean_temp
      f$rainfall <- df[i, ]$rainfall
      f$spei_03 <- df[i, ]$spei_03

      fates[[i]] <- f
    }

  }

  fates <- dplyr::bind_rows(fates)
  fates$year_of <- factor(fates$year_of)

  return(fates)
}

by_qua_ff <- by_qua_ff %>%
  mutate(trials = purrr::map(data, ~ get_trials(.)))

by_qua_ff <- by_qua_ff %>%
  mutate(null_model = purrr::map(trials, ~ glmer(fate ~ site + (1 | site/year_of),
                                                 family = binomial, data = .)),
         ind_model = purrr::map(trials, ~ glmer(fate ~ site * index_nino3.4 + (1 | site/year_of),
                                                family = binomial, data = .)),
         temp_model = purrr::map(trials, ~ glmer(fate ~ site * mean_temp + (1 | site/year_of),
                                                 family = binomial, data = .)),
         rainfall_model = purrr::map(trials, ~ glmer(fate ~ site * rainfall + (1 | site/year_of),
                                                     family = binomial, data = .)),
         drought_model = purrr::map(trials, ~ glmer(fate ~ site * spei_03 + (1 | site/year_of),
                                                    family = binomial, data = .)))

q2_ff <- by_qua_ff %>%
  mutate(null_AICc = purrr::map_dbl(null_model, MuMIn::AICc),
         index_nino3.4 = purrr::map_dbl(ind_model, MuMIn::AICc),
         mean_temp = purrr::map_dbl(temp_model, MuMIn::AICc),
         rainfall = purrr::map_dbl(rainfall_model, MuMIn::AICc),
         spei_03 = purrr::map_dbl(drought_model, MuMIn::AICc)) %>%
  select(-matches("model"), -data, -trials) %>%
  gather(var, AICc, -quarter, -null_AICc) %>%
  mutate(delta = AICc - null_AICc)

lim <- max(abs(q2_ff$delta))

ggplot(q2_ff,
       aes(x = delta, y = quarter, fill = delta)) +
  geom_vline(xintercept = 0, lty = 3) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.25) +
  facet_grid(var ~ ., switch = "y", labeller = global_labeller) +
  scale_x_continuous(limits = c(-ceiling(lim), ceiling(lim))) +
  scale_fill_gradientn(limits = c(-lim, lim),
                       trans = sqrt_sign_trans(),
                       colours = brewer.pal(11, "RdGy"),
                       name = "") +
  theme_fc() +
  labs(x = "\nAICc units from the null model", y = "Climate Model\n")


# sjplot ------------------------------------------------------------------

# P1
q <- "wettest_q"
mod <- "rainfall_model"

# P2
q <- "wettest_q"
mod <- "temp_model"

# P3
q <- "annual"
mod <- "ind_model"

sjp.glmer(by_qua_f[by_qua_f$quarter == q, mod][[1]][[1]], type = "re")
sjp.glmer(by_qua_f[by_qua_f$quarter == q, mod][[1]][[1]], type = "fe.cor")
sjp.glmer(by_qua_f[by_qua_f$quarter == q, mod][[1]][[1]], type = "re.qq")
sjp.glmer(by_qua_f[by_qua_f$quarter == q, mod][[1]][[1]], type = "fe.pc", facet.grid = TRUE)

sjp.glmer(by_qua_f[by_qua_f$quarter == q, mod][[1]][[1]],
          type = "fe", printPlot = FALSE, y.offset = 0.25, fade.ns = TRUE,
          sort.coef = TRUE, showIntercept = FALSE)$plot +
  theme_fc()

# ri.pc
temp <- sjPlot::sjp.glmer(by_qua_f[by_qua_f$quarter == q, mod][[1]][[1]], type = "ri.pc", facet.grid = FALSE, printPlot = FALSE)
temp$plot[[1]] + scale_color_discrete(guide = FALSE) + theme_fc()
temp$plot[[2]] + scale_color_discrete(guide = FALSE) + theme_fc()
temp$plot[[3]] + theme_fc()
temp$plot[[4]] + theme_fc()


sjp.glmer(by_qua_f[by_qua_f$quarter == q, mod][[1]][[1]], type = "y.pc", facet.grid = TRUE)
sjp.glmer(by_qua_f[by_qua_f$quarter == q, mod][[1]][[1]], type = "ma")

sjp.int(by_qua_f[by_qua_f$quarter == q, mod][[1]][[1]], type = "cond")

# eff plot
temp <- by_qua_f[by_qua_f$quarter == q, mod][[1]][[1]]
temp1 <- as.data.frame(by_qua_f[by_qua_f$quarter == q, "trials"][[1]][[1]])
temp@call$data <- as.name(deparse(substitute(temp1)))

t1 <- sjp.int(temp, type = "eff", printPlot = FALSE, showCI = TRUE,
        facet.grid = FALSE, geom.colors = "Dark2")$plot.list[[1]]

t1$data$grp <- factor(t1$data$grp, levels = levels(sites$site))

# t1 +
#   theme_fc() +
#   theme(legend.key.width = unit(0.75, "cm")) +
#   facet_grid(. ~ grp, labeller = global_labeller)

t2 <- t1$data
t2$site = factor(t2$grp, levels = levels(sites$site))
t3 <- by_qua_f[by_qua_f$quarter == q, "trials"][[1]][[1]]

v <- names(fixef(temp))
v <- v[v %in% c("index_nino3.4", "mean_temp", "rainfall")]

t2$quarter <- q
t2$var <- v

t2 <- inner_join(t2, dplyr::select(q1_f_coef, site, quarter, var, p_cat),
           by = c("site", "quarter", "var"))

p1 <- ggplot(t2, aes(x, y)) +
  geom_hline(aes(yintercept = 0), color = "gray90") +
  geom_hline(aes(yintercept = 1), color = "gray90") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = p_cat),
              alpha = 0.3, color = NA) +
  geom_line(aes(color = p_cat, fill = p_cat)) +
  ggstance::geom_violinh(data = t3, aes(x = rainfall, y = fate, group = fate),
                         width = 0.1, color = NA, fill = "gray90") +
  geom_point(data = t3, aes(x = rainfall, y = fate),
             size = 0.5, alpha = 0.1,
             shape = 3) +
  scale_color_manual(values = brewer.pal(10, "RdGy")[c(7, 2)], guide = FALSE) +
  scale_fill_manual(values = brewer.pal(10, "RdGy")[c(7, 2)], guide = FALSE) +
  facet_grid(. ~ site, scales = "free_x", labeller = global_labeller) +
  theme_fc() +
  labs(x = "\nScaled rainfall during wettest quarter",
       y = "Probability of reproducing before next census\n") +
  theme(legend.key.width = unit(0.75, "cm"),
        panel.grid = element_blank(),
        plot.background = element_blank())

cowplot::plot_grid(p1, p2, p3, ncol = 1, scale = 0.95, labels = c("a", "b", "c"))
