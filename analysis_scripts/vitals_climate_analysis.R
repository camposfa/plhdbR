rm(list = ls())
`%ni%` = Negate(`%in%`)

# If running the script from scratch
# load("ClimatePred1.RData")
# load("ClimatePred2.RData")

# If script already run and resuming workspace
load(".RData")

# ---- survival -----------------------------------------------------------

# Takes about 1 minute
m <- stage_specific_survival(lh)
summary(m)

# Make trials
surv_trials <- make_survivorship_trials(m)

surv_trials <- surv_trials %>%
  ungroup() %>%
  filter(year_of < 2014) %>%
  select(site = Study.Id, year_of, age_class, fate)


temp <- climate_predictors %>%
  ungroup() %>%
  group_by(site) %>%
  mutate_each(funs(scale), -site, -year_of) %>%
  gather(var, lag0, -site, -year_of) %>%
  group_by(site, var) %>%
  arrange(year_of) %>%
  mutate(lag1 = lag(lag0),
         lag2 = lag(lag0, n = 2),
         lag3 = lag(lag0, n = 3))

# Models
mod_df1 <- surv_trials %>%
  left_join(temp) %>%
  filter(!is.na(lag0))

mod_df1$site <- mapvalues(mod_df1$site,
                          from = levels(mod_df1$site),
                          to = c("Muriqui", "Baboon", "Blue Monkey",
                                 "Chimpanzee", "Gorilla", "Sifaka",
                                 "Capuchin"))

mod3 <- mod_df1 %>%
  ungroup() %>%
  group_by(site, age_class, var) %>%
  do(mod_null = glmer(fate ~ 1 + (1 | year_of), data = ., family = "binomial"),
     mod_0 = glmer(fate ~ lag0 + (1 | year_of), data = ., family = "binomial"),
     mod_1 = glmer(fate ~ lag1 + (1 | year_of), data = ., family = "binomial"),
     mod_2 = glmer(fate ~ lag2 + (1 | year_of), data = ., family = "binomial"))

mod4 <- NULL
k <- 1
for (i in 1:nrow(mod3)) {
  for (j in 4:ncol(mod3)) {
    temp <- select(mod3[i, ], 1:3, j)
    temp$scenario <- names(temp)[4]
    names(temp)[4] <- "model"
    mod4[[k]] <- temp
    k <- k + 1
  }
}

mod4 <- bind_rows(mod4)

mod4 <- filter(mod4, scenario != "mod_null" | (scenario == "mod_null" & var == "precip_annual"))
var_levels <- c(levels(factor(mod4$var)), "null")
mod4$var <- factor(mod4$var, levels = var_levels)
mod4[mod4$scenario == "mod_null", ]$var <- "null"

mod_sel <- mod4 %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$model),
     vars = data.frame(var = .$var),
     scenarios = data.frame(scenario = .$scenario),
     # deviance = data.frame(deviance = unlist(lapply(.$model, deviance))))
     deviance = data.frame(deviance = .$model %>% map(summary) %>% lapply(., "[[", "AICtab") %>% map_dbl("deviance")))

temp <- NULL

for (i in 1:nrow(mod_sel)) {
  m_table <- data.frame(mod_sel[i, ]$m_table[[1]])
  m_table$num <- rownames(m_table)

  vars <- mod_sel[i, ]$vars[[1]]
  vars$num <- rownames(vars)

  scenarios <- mod_sel[i, ]$scenarios[[1]]
  scenarios$num <- rownames(scenarios)

  deviance <- mod_sel[i, ]$deviance[[1]]
  deviance$num <- rownames(deviance)

  temp1 <- suppressMessages(inner_join(m_table, vars))
  temp1 <- suppressMessages(inner_join(temp1, scenarios))
  temp1 <- suppressMessages(inner_join(temp1, deviance))
  temp1$site <- mod_sel[i, ]$site
  temp1$age_class <- mod_sel[i, ]$age_class
  temp1$rank <- as.numeric(rownames(temp1))

  temp1 <- temp1 %>%
    select(site, age_class, var, scenario, deviance, rank, model_num = num, 1:9)

  temp[[i]] <- temp1

}

surv_models <- tbl_df(bind_rows(temp))

# Since MuMIn fails to get intercepts for null models
temp <- mod4 %>%
  filter(var == "null") %>%
  group_by(site, age_class) %>%
  summarise(intercept = lapply(lapply(model, summary), coef)[[1]][, "Estimate"]) %>%
  mutate(var = "null")

for (i in 1:length(levels(surv_models$site))) {
  current_site <- levels(surv_models$site)[i]
  surv_models[surv_models$site == current_site & surv_models$var == "null", ]$X.Intercept. <- temp[temp$site == current_site, ]$intercept
}

null_aic <- surv_models %>%
  filter(var == "null") %>%
  select(site, age_class, null_AICc = AICc, null_delta = delta,
         null_deviance = deviance)

surv_models <- inner_join(surv_models, null_aic)
surv_models$scenario <- mapvalues(surv_models$scenario,
                                  from = c("mod_0", "mod_1", "mod_2", "mod_null"),
                                  to = c("Lag 0", "Lag 1", "Lag 2", "Null"))

surv_models$scenario <- factor(surv_models$scenario,
                               levels = rev(c("Null", "Lag 0", "Lag 1", "Lag 2")))

surv_models %>%
  group_by(site, age_class) %>%
  top_n(1, -rank) %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance)) %>%
  select(-null_AICc, -null_delta, -null_deviance) %>%
  View()

best_surv_scenarios <- surv_models %>%
  group_by(site, age_class, var) %>%
  top_n(1, -rank) %>%
  ungroup() %>%
  group_by(site, age_class) %>%
  top_n(1, -rank) %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance)) %>%
  select(-null_AICc, -null_delta, -null_deviance)

best_surv_scenarios2 <- surv_models %>%
  group_by(site, age_class, var) %>%
  top_n(1, -rank) %>%
  ungroup() %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance)) %>%
  select(-null_AICc, -null_delta, -null_deviance)


# All lag scenarios

surv_plots <- surv_models %>%
  filter(var != "null") %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))

lim <-  max(c(abs(min(surv_plots$null_AICc - surv_plots$AICc, na.rm = TRUE)),
              abs(max(surv_plots$null_AICc - surv_plots$AICc, na.rm = TRUE))))

ggplot(surv_plots, aes(x = age_class, y = var, fill = (AICc - null_AICc))) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = "AICc relative to Null Model",
                       # trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  facet_grid(scenario ~ site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "\nAge Class", y = "Cliamte Variable\n") +
  coord_equal()

ggsave("plots/models/Survival_AllLagScenarios_AIC.pdf",
       width = 9, height = 16, units = "in")

# Plot Deviance
ggplot(surv_plots, aes(x = age_class, y = var, fill = D)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = c("#FFFFFF", brewer.pal(9, "Greens")),
                       # trans = sqrt_sign_trans(),
                       name = "Proportional Reduction in Deviance") +
  facet_grid(scenario ~ site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        # axis.text.y = element_text(color = colvec),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Climate Variable\n", x = "\nAge Class") +
  coord_equal()

ggsave("plots/models/Survival_AllLagScenarios_Deviance.pdf",
       width = 9, height = 16, units = "in")

# temp1 <- temp %>%
#   group_by(site, age_class) %>%
#   mutate(min_AICc = min(AICc),
#          delta = AICc - min_AICc) %>%
#   ungroup() %>%
#   group_by(site, age_class, var) %>%
#   top_n(n = 1, wt = -rank)
#
# ggplot(temp1, aes(y = var, x = age_class, fill = delta)) +
#   geom_tile(size = 0.1, color = "black") +
#   scale_fill_gradientn(colours = c(rev(brewer.pal(9, "Reds")), "#FFFFFF"),
#                        trans = log1p_trans(),
#                        name = expression(paste(Delta, "AICc"))) +
#   facet_grid(. ~ site) +
#   theme_bw() +
#   theme(strip.background = element_blank(),
#         legend.position = "bottom",
#         panel.grid = element_blank(),
#         legend.key.width = unit(2, "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         legend.key.height = unit(0.2, "cm")) +
#   labs(y = "Scale of Climate Variable\n", x = "\nAge Class") +
#   coord_equal()

# Pick best lag scenario
ggplot(best_surv_scenarios2, aes(x = age_class, y = var, fill = delta_AICc_vs_null)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = "AICc relative to Null Model",
                       # trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  facet_grid(. ~ site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Climate Variable\n", x = "\nAge Class") +
  coord_equal()

ggsave("plots/models/Survival_BestLagScenarios_AIC.pdf",
       width = 11, height = 8.5, units = "in")



# All CIs / SEs

sc_0 <- mod4 %>%
  filter(scenario == "mod_0" & var %ni% c("null"))

sc_01 <- cbind(sc_0[, 1:3], ldply(sc_0$model, .fun = function(x) coef(summary(x))["lag0", "Std. Error"]))
sc_02 <- cbind(sc_0[, 1:3], ldply(sc_0$model, .fun = function(x) confint(x, method = "Wald")["lag0", ]))

sc_0 <- surv_models %>%
  filter(scenario == "Lag 0" & var %ni% c("null"))

sc_0$var <- factor(sc_0$var)
sc_02$var <- factor(sc_02$var, levels = levels(factor(sc_0$var)))
sc_01$var <- factor(sc_01$var, levels = levels(factor(sc_0$var)))

sc_0_lag0 <- sc_0 %>% inner_join(sc_01) %>% inner_join(sc_02)
names(sc_0_lag0)[(ncol(sc_0_lag0) - 2):ncol(sc_0_lag0)] <- c("se", "lower_ci", "upper_ci")

sc_1 <- mod4 %>%
  filter(scenario == "mod_1" & var %ni% c("null"))

sc_11 <- cbind(sc_1[, 1:3], ldply(sc_1$model, .fun = function(x) coef(summary(x))["lag1", "Std. Error"]))
sc_12 <- cbind(sc_1[, 1:3], ldply(sc_1$model, .fun = function(x) confint(x, method = "Wald")["lag1", ]))

sc_1 <- surv_models %>%
  filter(scenario == "Lag 1" & var %ni% c("null"))

sc_1$var <- factor(sc_1$var)
sc_12$var <- factor(sc_12$var, levels = levels(factor(sc_1$var)))
sc_11$var <- factor(sc_11$var, levels = levels(factor(sc_1$var)))

sc_1_lag1 <- sc_1 %>% inner_join(sc_11) %>% inner_join(sc_12)
names(sc_1_lag1)[(ncol(sc_1_lag1) - 2):ncol(sc_1_lag1)] <- c("se", "lower_ci", "upper_ci")

sc_2 <- mod4 %>%
  filter(scenario == "mod_2" & var %ni% c("null"))

sc_21 <- cbind(sc_2[, 1:3], ldply(sc_2$model, .fun = function(x) coef(summary(x))["lag2", "Std. Error"]))
sc_22 <- cbind(sc_2[, 1:3], ldply(sc_2$model, .fun = function(x) confint(x, method = "Wald")["lag2", ]))

sc_2 <- surv_models %>%
  filter(scenario == "Lag 2" & var %ni% c("null"))

sc_2$var <- factor(sc_2$var)
sc_22$var <- factor(sc_22$var, levels = levels(factor(sc_2$var)))
sc_21$var <- factor(sc_21$var, levels = levels(factor(sc_2$var)))

sc_2_lag2 <- sc_2 %>% inner_join(sc_21) %>% inner_join(sc_22)
names(sc_2_lag2)[(ncol(sc_2_lag2) - 2):ncol(sc_2_lag2)] <- c("se", "lower_ci", "upper_ci")

sc <- bind_rows(sc_0_lag0, sc_1_lag1, sc_2_lag2)

st <- sc %>%
  select(-scenario) %>%
  gather(lag, estimate, lag0, lag1, lag2) %>%
  filter(!is.na(estimate))

st$lag <- mapvalues(st$lag,
                    from = c("lag0", "lag1", "lag2"),
                    to = c("Lag 0", "Lag 1", "Lag 2"))


# Plot both lag scenarios for each site separately
st <- st %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))

lim <-  max(c(abs(min(st$D, na.rm = TRUE)),
              abs(max(st$D, na.rm = TRUE))))

for (i in 1:length(levels(st$site))) {

  current_site = levels(st$site)[i]

  temp4 <- filter(st, site == current_site)

  ggplot(temp4, aes(x = estimate, y = var, color = D)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = estimate - se, xmax = estimate + se),
                   height = 0.3, size = 0.75) +
    scale_color_gradientn(colours = brewer.pal(9, "Greens")[2:9],
                          name = "Proportional Reduction\nin Deviance",
                          trans = sqrt_sign_trans(),
                          limits = c(0, lim)) +
    geom_vline(xintercept = 0, lty = 2) +
    theme_bw() +
    facet_grid(lag ~ age_class) +
    theme(strip.background = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(0.5, "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "\nScaled Coefficient Estimate", y = "Variable\n",
         title = paste(current_site, ":\n",
                       "Scaled Coefficient Estimates for Models of Survival\n",
                       sep = ""))

  ggsave(paste("plots/models/Survival_Lags_Coefficients_",
               i, "_", current_site, ".pdf", sep = ""),
         width = 8.5, height = 11, units = "in")
}

rm(sc_0, sc_0_lag0, sc_01, sc_02, sc_1, sc_1_lag1, sc_11, sc_12, sc_2,
   sc_2_lag2, sc_21, sc_22)


# ---- fertility ----------------------------------------------------------

# Takes a few minutes to run
f <- stage_specific_fertility(lh, fert)

# Make trials
fert_trials <- make_survivorship_trials(f)

fert_trials <- fert_trials %>%
  ungroup() %>%
  filter(year_of < 2014) %>%
  select(site = Study.Id, year_of, age_class, fate)

temp <- climate_predictors %>%
  ungroup() %>%
  group_by(site) %>%
  mutate_each(funs(scale), -site, -year_of) %>%
  gather(var, lag0, -site, -year_of) %>%
  group_by(site, var) %>%
  arrange(year_of) %>%
  mutate(lag1 = lag(lag0),
         lag2 = lag(lag0, n = 2))

# Models
fert_mod_df <- fert_trials %>%
  left_join(temp) %>%
  filter(!is.na(lag0))

fert_mod_df$site <- mapvalues(fert_mod_df$site,
                              from = levels(fert_mod_df$site),
                              to = c("Muriqui", "Baboon", "Blue Monkey",
                                     "Chimpanzee", "Gorilla", "Sifaka",
                                     "Capuchin"))

mod <- fert_mod_df %>%
  filter(age_class == "adult") %>%
  ungroup() %>%
  group_by(site, var) %>%
  do(mod_null = glmer(fate ~ 1 + (1 | year_of), data = ., family = "binomial"),
     mod_0 = glmer(fate ~ lag0 + (1 | year_of), data = ., family = "binomial"),
     mod_1 = glmer(fate ~ lag1 + (1 | year_of), data = ., family = "binomial"),
     mod_2 = glmer(fate ~ lag2 + (1 | year_of), data = ., family = "binomial"))

mod5 <- NULL
k <- 1
for (i in 1:nrow(mod)) {
  for (j in 3:ncol(mod)) {
    temp <- select(mod[i, ], 1:2, j)
    temp$scenario <- names(temp)[3]
    names(temp)[3] <- "model"
    mod5[[k]] <- temp
    k <- k + 1
  }
}

mod5 <- bind_rows(mod5)

mod5 <- filter(mod5, scenario != "mod_null" | (scenario == "mod_null" & var == "precip_annual"))
var_levels <- c(levels(factor(mod5$var)), "null")
mod5$var <- factor(mod5$var, levels = var_levels)
mod5[mod5$scenario == "mod_null", ]$var <- "null"

fert_mod_sel <- mod5 %>%
  group_by(site) %>%
  do(m_table = model.sel(.$model),
     vars = data.frame(var = .$var),
     scenarios = data.frame(scenario = .$scenario),
     deviance = data.frame(deviance = .$model %>% map(summary) %>% lapply(., "[[", "AICtab") %>% map_dbl("deviance")))

temp <- NULL

for (i in 1:nrow(fert_mod_sel)) {
  m_table <- data.frame(fert_mod_sel[i, ]$m_table[[1]])
  m_table$num <- rownames(m_table)

  vars <- fert_mod_sel[i, ]$vars[[1]]
  vars$num <- rownames(vars)

  scenarios <- fert_mod_sel[i, ]$scenarios[[1]]
  scenarios$num <- rownames(scenarios)

  deviance <- fert_mod_sel[i, ]$deviance[[1]]
  deviance$num <- rownames(deviance)

  temp1 <- suppressMessages(inner_join(m_table, vars))
  temp1 <- suppressMessages(inner_join(temp1, scenarios))
  temp1 <- suppressMessages(inner_join(temp1, deviance))
  temp1$site <- fert_mod_sel[i, ]$site
  temp1$rank <- as.numeric(rownames(temp1))

  temp1 <- temp1 %>%
    select(site, var, scenario, deviance, rank, model_num = num, 1:9)

  temp[[i]] <- temp1

}

fert_models <- tbl_df(bind_rows(temp))

# Since MuMIn fails to get intercepts for null models
temp <- mod5 %>%
  filter(var == "null") %>%
  group_by(site) %>%
  summarise(intercept = lapply(lapply(model, summary), coef)[[1]][, "Estimate"]) %>%
  mutate(var = "null")

for (i in 1:length(levels(fert_models$site))) {
  current_site <- levels(fert_models$site)[i]
  fert_models[fert_models$site == current_site & fert_models$var == "null", ]$X.Intercept. <- temp[temp$site == current_site, ]$intercept
}


null_aic <- fert_models %>%
  filter(var == "null") %>%
  select(site, null_AICc = AICc, null_delta = delta,
         null_deviance = deviance)

fert_models <- inner_join(fert_models, null_aic)

fert_models$scenario <- mapvalues(fert_models$scenario,
                                  from = c("mod_0", "mod_1", "mod_2", "mod_null"),
                                  to = c("Lag 0", "Lag 1", "Lag 2", "Null"))

fert_models$scenario <- factor(fert_models$scenario,
                               levels = rev(c("Null", "Lag 0", "Lag 1", "Lag 2")))

fert_models %>%
  group_by(site) %>%
  top_n(2, -rank) %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance)) %>%
  select(-null_AICc, -null_delta, -null_deviance) %>%
  View()

fert_models %>%
  group_by(site, var) %>%
  top_n(1, -rank) %>%
  ungroup() %>%
  group_by(site) %>%
  top_n(20, -rank) %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance)) %>%
  select(-null_AICc, -null_delta, -null_deviance) %>%
  View()

best_fert_scenarios2 <- fert_models %>%
  group_by(site, var) %>%
  top_n(1, -rank) %>%
  ungroup() %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance)) %>%
  select(-null_AICc, -null_delta, -null_deviance)


# Plot

fert_plots <- fert_models %>%
  filter(var != "null") %>%
  mutate(D = 1 - (deviance / null_deviance))

lim <-  max(c(abs(min(fert_plots$null_AICc - fert_plots$AICc, na.rm = TRUE)),
              abs(max(fert_plots$null_AICc - fert_plots$AICc, na.rm = TRUE))))

ggplot(fert_plots, aes(y = var, x = scenario, fill = (AICc - null_AICc))) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = "AICc relative to Null Model",
                       # trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  facet_grid(. ~ site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "\nClimate Variable", y = "Lag Scenario\n") +
  coord_equal()

ggsave("plots/models/Fertility_AllLagScenarios_AIC.pdf",
       width = 11, height = 8.5, units = "in")


# Deviance
lim <- max(fert_plots$D)

ggplot(fert_plots, aes(y = var, x = scenario, fill = D)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = c("#FFFFFF", brewer.pal(9, "Greens")),
                       name = "Proportional Reduction in Deviance",
                       limits = c(0, lim)) +
  facet_grid(. ~ site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "\nClimate Variable", y = "Lag Scenario\n") +
  coord_equal()

ggsave("plots/models/Fertility_AllLagScenarios_Deviance.pdf",
       width = 8.5, height = 11, units = "in")

# temp1 <- temp %>%
#   group_by(site) %>%
#   mutate(min_AICc = min(AICc),
#          delta = AICc - min_AICc) %>%
#   ungroup() %>%
#   group_by(site, var) %>%
#   top_n(n = 1, wt = -rank)
#
# ggplot(temp1, aes(y = var, x = site, fill = delta)) +
#   geom_tile(size = 0.1, color = "black") +
#   scale_fill_gradientn(colours = c(rev(brewer.pal(9, "Reds")), "#FFFFFF"),
#                        trans = log1p_trans(),
#                        name = expression(paste(Delta, "AICc"))) +
#   # facet_grid(. ~ site) +
#   theme_bw() +
#   theme(strip.background = element_blank(),
#         legend.position = "bottom",
#         panel.grid = element_blank(),
#         legend.key.width = unit(2, "cm"),
#         # axis.text.y = element_text(color = colvec),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         legend.key.height = unit(0.2, "cm")) +
#   labs(y = "Scale of Climate Variable\n", x = "\nPopulation") +
#   coord_equal()
#
# ggsave("plots/models/Fertility_AllLagScenarios_AIC.pdf",
#        width = 8, height = 12, units = "in")

ggplot(best_fert_scenarios2, aes(x = site, y = var, fill = delta_AICc_vs_null)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = "AICc relative to Null Model",
                       limits = c(-lim, lim)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Climate Variable\n", x = "\nPopulation") +
  coord_equal()

ggsave("plots/models/Fertility_BestLagScenarios_AIC.pdf",
       width = 11, height = 8.5, units = "in")


# Coefficients
fc_0 <- mod5 %>%
  filter(scenario == "mod_0" & var %ni% c("null"))

fc_01 <- cbind(fc_0[, 1:3], ldply(fc_0$model, .fun = function(x) coef(summary(x))["lag0", "Std. Error"]))
fc_02 <- cbind(fc_0[, 1:3], ldply(fc_0$model, .fun = function(x) confint(x, method = "Wald")["lag0", ]))

fc_01 <- select(fc_01, -model)
fc_02 <- select(fc_02, -model)

fc_0 <- fert_models %>%
  filter(scenario == "Lag 0" & var %ni% c("null"))

fc_0$var <- factor(fc_0$var)
fc_02$var <- factor(fc_02$var, levels = levels(factor(fc_0$var)))
fc_01$var <- factor(fc_01$var, levels = levels(factor(fc_0$var)))

fc_0_lag0 <- fc_0 %>%
  inner_join(fc_01, by = c("site", "var")) %>%
  inner_join(fc_02, by = c("site", "var"))

names(fc_0_lag0)[(ncol(fc_0_lag0) - 2):ncol(fc_0_lag0)] <- c("se", "lower_ci", "upper_ci")

fc_1 <- mod5 %>%
  filter(scenario == "mod_1" & var %ni% c("null"))

fc_11 <- cbind(fc_1[, 1:3], ldply(fc_1$model, .fun = function(x) coef(summary(x))["lag1", "Std. Error"]))
fc_12 <- cbind(fc_1[, 1:3], ldply(fc_1$model, .fun = function(x) confint(x, method = "Wald")["lag1", ]))

fc_11 <- select(fc_11, -model)
fc_12 <- select(fc_12, -model)

fc_1 <- fert_models %>%
  filter(scenario == "Lag 1" & var %ni% c("null"))

fc_1$var <- factor(fc_1$var)
fc_12$var <- factor(fc_12$var, levels = levels(factor(fc_1$var)))
fc_11$var <- factor(fc_11$var, levels = levels(factor(fc_1$var)))

fc_1_lag1 <- fc_1 %>%
  inner_join(fc_11) %>%
  inner_join(fc_12)

names(fc_1_lag1)[(ncol(fc_1_lag1) - 2):ncol(fc_1_lag1)] <- c("se", "lower_ci", "upper_ci")

fc_2 <- mod5 %>%
  filter(scenario == "mod_2" & var %ni% c("null"))

fc_21 <- cbind(fc_2[, 1:3], ldply(fc_2$model, .fun = function(x) coef(summary(x))["lag2", "Std. Error"]))
fc_22 <- cbind(fc_2[, 1:3], ldply(fc_2$model, .fun = function(x) confint(x, method = "Wald")["lag2", ]))

fc_21 <- select(fc_21, -model)
fc_22 <- select(fc_22, -model)

fc_2 <- fert_models %>%
  filter(scenario == "Lag 2" & var %ni% c("null"))

fc_2$var <- factor(fc_2$var)
fc_22$var <- factor(fc_22$var, levels = levels(factor(fc_2$var)))
fc_21$var <- factor(fc_21$var, levels = levels(factor(fc_2$var)))

fc_2_lag2 <- fc_2 %>%
  inner_join(fc_21) %>%
  inner_join(fc_22)

names(fc_2_lag2)[(ncol(fc_2_lag2) - 2):ncol(fc_2_lag2)] <- c("se", "lower_ci", "upper_ci")

fc <- bind_rows(fc_0_lag0, fc_1_lag1, fc_2_lag2)

ft <- fc %>%
  select(-scenario) %>%
  gather(lag, estimate, lag0, lag1, lag2) %>%
  filter(!is.na(estimate))

ft$lag <- mapvalues(ft$lag,
                   from = c("lag0", "lag1", "lag2"),
                   to = c("Lag 0", "Lag 1", "Lag 2"))

# Plot both lag scenarios for each site separately
ft <- ft %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))

lim <-  max(c(abs(min(ft$D, na.rm = TRUE)),
              abs(max(ft$D, na.rm = TRUE))))

for (i in 1:length(levels(ft$site))) {

  current_site = levels(ft$site)[i]

  temp4 <- filter(ft, site == current_site)

  ggplot(temp4, aes(x = estimate, y = var, color = D)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = estimate - se, xmax = estimate + se),
                   height = 0.3, size = 0.75) +
    scale_color_gradientn(colours = brewer.pal(9, "Greens")[2:9],
                          name = "Proportional Reduction\nin Deviance",
                          # trans = sqrt_sign_trans(),
                          limits = c(0, lim)) +
    geom_vline(xintercept = 0, lty = 2) +
    theme_bw() +
    facet_grid(lag ~ .) +
    theme(strip.background = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.5, "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "\nScaled Coefficient Estimate", y = "Variable\n",
         title = paste(current_site, ":\n",
                       "Scaled Coefficient Estimates for Models of Fertility\n",
                       sep = ""))

  ggsave(paste("plots/models/Fertility_Lags_Coefficients_",
               i, "_", current_site, ".pdf", sep = ""),
         width = 6.5, height = 11, units = "in")
}

rm(fc_0, fc_0_lag0, fc_01, fc_02, fc_1, fc_1_lag1, fc_11, fc_12, fc_2,
   fc_2_lag2, fc_21, fc_22)