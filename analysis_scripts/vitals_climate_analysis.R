
# ---- survival -----------------------------------------------------------

# Takes about 1 minute
m <- stage_specific_survival(lh)
summary(m)

# Make trials
surv_trials <- make_survivorship_trials(m)
surv_trials <- filter(surv_trials, year_of < 2015)

temp <- climate_predictors %>%
  select(-n_months) %>%
  ungroup() %>%
  mutate_each(funs(scale), -site, -year_of) %>%
  group_by(site) %>%
  gather(var, lag0, -site, -year_of) %>%
  group_by(site, var) %>%
  arrange(year_of) %>%
  mutate(lag1 = lag(lag0),
         lag2 = lag(lag0, n = 2),
         lag3 = lag(lag0, n = 3))

# Models
mod_df <- surv_trials %>%
  rename(site = Study.Id) %>%
  left_join(temp) %>%
  filter(!is.na(lag0))

mod_df$site <- mapvalues(mod_df$site,
                         from = levels(mod_df$site),
                         to = c("Muriqui", "Baboon", "Blue Monkey",
                                "Chimpanzee", "Gorilla", "Sifaka",
                                "Capuchin"))

mod3 <- mod_df %>%
  ungroup() %>%
  filter(!str_detect(var, "monthly") & !str_detect(var, "tavg")) %>%
  group_by(site, age_class, var) %>%
  do(mod_null = glmer(fate ~ 1 + (1 | year_of), data = ., family = "binomial"),
     mod_0 = glmer(fate ~ lag0 + (1 | year_of), data = ., family = "binomial"),
     mod_1 = glmer(fate ~ lag1 + (1 | year_of), data = ., family = "binomial"),
     mod_0_1 = glmer(fate ~ lag0 + lag1 + (1 | year_of), data = ., family = "binomial"))

mod4 <- NULL
k <- 1
for(i in 1:nrow(mod3)){
  for(j in 4:ncol(mod3)){
    temp <- select(mod3[i, ], 1:3, j)
    temp$scenario <- names(temp)[4]
    names(temp)[4] <- "model"
    mod4[[k]] <- temp
    k <- k + 1
  }
}

mod4 <- bind_rows(mod4)

mod4 <- filter(mod4, scenario != "mod_null" | (scenario == "mod_null" & var == "rain_anomaly_mean"))
var_levels <- c(levels(factor(mod4$var)), "null")
mod4$var <- factor(mod4$var, levels = var_levels)
mod4[mod4$scenario == "mod_null", ]$var <- "null"

mod_sel <- mod4 %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$model, extra = c("BIC", "DIC")),
     vars = data.frame(var = .$var),
     scenarios = data.frame(scenario = .$scenario),
     deviance = data.frame(deviance = unlist(lapply(.$model, deviance))))

temp <- NULL

for(i in 1:nrow(mod_sel)){
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
    select(site, age_class, var, scenario, deviance, rank, model_num = num, 1:10)

  temp[[i]] <- temp1

}

surv_models <- tbl_df(bind_rows(temp))

# Since MuMIn fails to get intercepts for null models
temp <- mod4 %>%
  filter(var == "null") %>%
  group_by(site, age_class) %>%
  summarise(intercept = lapply(lapply(model, summary), coef)[[1]][, "Estimate"]) %>%
  mutate(var = "null")

for(i in 1:length(levels(surv_models$site))){
  current_site <- levels(surv_models$site)[i]
  surv_models[surv_models$site == current_site & surv_models$var == "null", ]$X.Intercept. <- temp[temp$site == current_site, ]$intercept
}

null_aic <- surv_models %>%
  filter(var == "null") %>%
  select(site, age_class, null_AICc = AICc, null_delta = delta,
         null_weight = weight, null_deviance = deviance)

surv_models <- inner_join(surv_models, null_aic)

surv_models$var <- factor(surv_models$var,
                          levels = c("rain_total_mm", "rain_anomaly_mean",
                                     "wettest_anomaly", "driest_anomaly",
                                     "shannon_rain", "spei_01_mean", "spei_03_mean",
                                     "spei_06_mean", "tmax_anomaly_mean",
                                     "tmax_detrended_mean", "hottest_tmax_anomaly",
                                     "tmin_anomaly_mean", "tmin_detrended_mean",
                                     "coldest_tmin_anomaly", "amo_mean", "nao_mean",
                                     "oni_mean", "pdo_mean", "dmi_mean", "sam_mean",
                                     "ao_mean", "null"))

surv_models$scenario <- mapvalues(surv_models$scenario,
                                  from = c("mod_0", "mod_1", "mod_0_1"),
                                  to = c("Lag 0", "Lag 1", "Lag 0 + Lag 1"))

surv_models$scenario <- factor(surv_models$scenario,
                               levels = rev(c("Lag 0", "Lag 1", "Lag 0 + Lag 1")))

surv_models %>%
  group_by(site, age_class) %>%
  filter(scenario != "Lag 0 + Lag 1") %>%
  # filter(str_detect(var, "spei")) %>%
  top_n(1, -rank) %>%
  mutate(evidence_vs_null = weight / null_weight,
         delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance)) %>%
  select(-null_AICc, -null_delta, -null_weight, -null_deviance) %>%
  View()

best_surv_scenarios <- surv_models %>%
  group_by(site, age_class, var) %>%
  filter(scenario != "Lag 0 + Lag 1") %>%
  top_n(1, -rank) %>%
  ungroup() %>%
  group_by(site, age_class) %>%
  top_n(1, -rank) %>%
  mutate(evidence_vs_null = weight / null_weight,
         delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance)) %>%
  select(-null_AICc, -null_delta, -null_weight, -null_deviance)


# All lag scenarios

temp <- surv_models %>%
  filter(var %ni% c("null", "spei_03_mean", "spei_06_mean"))

lim <-  max(c(abs(min(temp$null_AICc - temp$AICc, na.rm = TRUE)),
              abs(max(temp$null_AICc - temp$AICc, na.rm = TRUE))))

ggplot(temp, aes(x = var, y = scenario, fill = (AICc - null_AICc))) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = expression(paste(Delta, "AICc relative to Null Model")),
                       trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  facet_wrap(site ~ age_class, nrow = 7) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "\nClimate Variable", y = "Lag Scenario\n")

ggsave("plots/models/Survival_AllLagScenarios_AIC.pdf",
       width = 16, height = 10, units = "in")


# Plot Deviance

temp <- surv_models %>%
  filter(var %ni% c("null", "spei_03_mean", "spei_06_mean")) %>%
  mutate(evidence_vs_null = weight / null_weight,
         delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))

lim <-  max(c(abs(min(temp$D, na.rm = TRUE)),
              abs(max(temp$D, na.rm = TRUE))))

ggplot(temp, aes(x = var, y = scenario, fill = D)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = rev(brewer.pal(9, "RdGy")),
                       name = "Proportional Reduction in Deviance",
                       # trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  facet_wrap(site ~ age_class, nrow = 7) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(2, "cm"),
        legend.key.height=unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "\nClimate Variable", y = "Population\n")

ggsave("plots/models/Survival_AllLagScenarios_Deviance.pdf",
       width = 16, height = 10, units = "in")


# Plot delta AICc

# temp <- surv_models %>%
#   filter(scenario == "Lag 1" & var %ni% c("null"))
#
# lim <-  max(c(abs(min(temp$null_AICc - temp$AICc, na.rm = TRUE)),
#               abs(max(temp$null_AICc - temp$AICc, na.rm = TRUE))))
#
# ggplot(temp, aes(x = var, y = age_class, fill = (AICc - null_AICc))) +
#   geom_tile(size = 0.1, color = "black") +
#   scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
#                        name = expression(paste(Delta, "AICc relative to Null Model")),
#                        limits = c(-lim, lim),
#                        trans = sqrt_sign_trans()) +
#   facet_grid(site ~ .) +
#   theme_bw() +
#   theme(strip.background = element_blank(),
#         legend.position = "bottom",
#         legend.key.width = unit(2, "cm"),
#         legend.key.height = unit(0.2, "cm"),
#         panel.grid = element_blank(),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   labs(x = "\nClimate Variable", y = "Population\n",
#        title = "Lag-1 Models of Stage-specific Survival\n")
#
# ggsave("plots/models/Survival_Lag1_AIC.pdf",
#        width = 8, height = 11, units = "in")


# All CIs / SEs

temp <- mod4 %>%
  filter(scenario == "mod_0" & var %ni% c("null"))

temp1 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) coef(summary(x))["lag0", "Std. Error"]))
temp2 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) confint(x, method = "Wald")["lag0", ]))

temp <- surv_models %>%
  filter(scenario == "Lag 0" & var %ni% c("null"))

temp$var <- factor(temp$var)
temp2$var <- factor(temp2$var, levels = levels(factor(temp$var)))
temp1$var <- factor(temp1$var, levels = levels(factor(temp$var)))

temp_lag0 <- temp %>% inner_join(temp1) %>% inner_join(temp2)
names(temp_lag0)[(ncol(temp_lag0) - 2):ncol(temp_lag0)] <- c("se", "lower_ci", "upper_ci")

temp <- mod4 %>%
  filter(scenario == "mod_1" & var %ni% c("null"))

temp1 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) coef(summary(x))["lag1", "Std. Error"]))
temp2 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) confint(x, method = "Wald")["lag1", ]))

temp <- surv_models %>%
  filter(scenario == "Lag 1" & var %ni% c("null"))

temp$var <- factor(temp$var)
temp2$var <- factor(temp2$var, levels = levels(factor(temp$var)))
temp1$var <- factor(temp1$var, levels = levels(factor(temp$var)))

temp_lag1 <- temp %>% inner_join(temp1) %>% inner_join(temp2)
names(temp_lag1)[(ncol(temp_lag1) - 2):ncol(temp_lag1)] <- c("se", "lower_ci", "upper_ci")

temp <- bind_rows(temp_lag0, temp_lag1)

t <- temp %>%
  select(-scenario) %>%
  gather(lag, estimate, lag0, lag1) %>%
  filter(!is.na(estimate))

t$lag <- mapvalues(t$lag,
                   from = c("lag0", "lag1"),
                   to = c("Lag 0", "Lag 1"))

lim <-  max(c(abs(min(t$null_AICc - t$AICc, na.rm = TRUE)),
              abs(max(t$null_AICc - t$AICc, na.rm = TRUE))))


# Plot coefficients separately for each population

for(i in 1:length(levels(temp$site))){

  current_site = levels(temp$site)[i]

  temp4 <- filter(temp, site == current_site)

  lim <-  max(c(abs(min(temp4$null_AICc - temp4$AICc, na.rm = TRUE)),
                abs(max(temp4$null_AICc - temp4$AICc, na.rm = TRUE))))

  ggplot(temp4, aes(x = lag1, y = var, color = (AICc - null_AICc))) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = lag1 - se, xmax = lag1 + se),
                   height = 0.3, size = 0.75) +
    scale_color_gradientn(colours = brewer.pal(9, "RdGy"),
                          name = expression(paste(Delta, "AICc relative to Null Model")),
                          limits = c(-lim, lim),
                          trans = sqrt_sign_trans()) +
    geom_vline(xintercept = 0, lty = 2) +
    facet_grid(age_class ~ .) +
    theme_bw() +
    theme(strip.background = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(0.2, "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "Scaled Coefficient Estimate\n", y = "\nPopulation",
         title = paste(current_site, ":\n",
                       "Scaled Coefficient Estimates for Lag-1 Models of Stage-specific Survival\n",
                       sep = "")) +
    coord_flip()

  ggsave(paste("plots/models/Survival_Lag1_Coefficients_",
               i, "_", current_site, ".pdf", sep = ""),
         width = 8, height = 11, units = "in")
}

# Plot both lag scenarios for each site separately

for(i in 1:length(levels(t$site))){

  current_site = levels(t$site)[i]

  temp4 <- filter(t, site == current_site & var %ni% c("spei_03_mean", "spei_06_mean"))

  lim <-  max(c(abs(min(temp4$null_AICc - temp4$AICc, na.rm = TRUE)),
                abs(max(temp4$null_AICc - temp4$AICc, na.rm = TRUE))))

  ggplot(temp4, aes(x = estimate, y = var, color = (AICc - null_AICc))) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = estimate - se, xmax = estimate + se),
                   height = 0.3, size = 0.75) +
    scale_color_gradientn(colours = brewer.pal(9, "RdGy"),
                          name = expression(paste(Delta, "AICc relative to Null Model")),
                          limits = c(-lim, lim),
                          trans = sqrt_sign_trans()) +
    geom_vline(xintercept = 0, lty = 2) +
    theme_bw() +
    facet_grid(lag ~ age_class) +
    theme(strip.background = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(0.2, "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "Scaled Coefficient Estimate\n", y = "\nPopulation",
         title = paste(current_site, ":\n",
                       "Scaled Coefficient Estimates for Lag-0 and Lag-1 Models of Survival\n",
                       sep = "")) +
    coord_flip()

  ggsave(paste("plots/models/Survival_Lag0_Lag1_Coefficients_",
               i, "_", current_site, ".pdf", sep = ""),
         width = 14, height = 11, units = "in")
}




# ---- fertility ----------------------------------------------------------

# Takes a few minutes to run
f <- stage_specific_fertility(lh, fert)

# Make trials
fert_trials <- make_survivorship_trials(f)
fert_trials <- fert_trials %>%
  filter(year_of < 2015 & age_class == "adult")

temp <- climate_predictors %>%
  select(-n_months) %>%
  ungroup() %>%
  mutate_each(funs(scale), -site, -year_of) %>%
  group_by(site) %>%
  gather(var, lag0, -site, -year_of) %>%
  group_by(site, var) %>%
  arrange(year_of) %>%
  mutate(lag1 = lag(lag0),
         lag2 = lag(lag0, n = 2),
         lag3 = lag(lag0, n = 3))

# Models
fert_mod_df <- fert_trials %>%
  rename(site = Study.Id) %>%
  left_join(temp) %>%
  filter(!is.na(lag0))

fert_mod_df$site <- mapvalues(fert_mod_df$site,
                              from = levels(fert_mod_df$site),
                              to = c("Muriqui", "Baboon", "Blue Monkey",
                                     "Chimpanzee", "Gorilla", "Sifaka",
                                     "Capuchin"))

mod <- fert_mod_df %>%
  ungroup() %>%
  filter(!str_detect(var, "monthly") & !str_detect(var, "tavg")) %>%
  group_by(site, age_class, var) %>%
  do(mod_null = glmer(fate ~ 1 + (1 | year_of), data = ., family = "binomial"),
     mod_0 = glmer(fate ~ lag0 + (1 | year_of), data = ., family = "binomial"),
     mod_1 = glmer(fate ~ lag1 + (1 | year_of), data = ., family = "binomial"),
     mod_0_1 = glmer(fate ~ lag0 + lag1 + (1 | year_of), data = ., family = "binomial"))

mod5 <- NULL
k <- 1
for(i in 1:nrow(mod)){
  for(j in 4:ncol(mod)){
    temp <- select(mod[i, ], 1:3, j)
    temp$scenario <- names(temp)[4]
    names(temp)[4] <- "model"
    mod5[[k]] <- temp
    k <- k + 1
  }
}

mod5 <- bind_rows(mod5)

mod5 <- filter(mod5, scenario != "mod_null" | (scenario == "mod_null" & var == "rain_anomaly_mean"))
var_levels <- c(levels(factor(mod5$var)), "null")
mod5$var <- factor(mod5$var, levels = var_levels)
mod5[mod5$scenario == "mod_null", ]$var <- "null"

fert_mod_sel <- mod5 %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$model, extra = c("BIC", "DIC")),
     vars = data.frame(var = .$var),
     scenarios = data.frame(scenario = .$scenario),
     deviance = data.frame(deviance = unlist(lapply(.$model, deviance))))

temp <- NULL

for(i in 1:nrow(fert_mod_sel)){
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
  temp1$age_class <- fert_mod_sel[i, ]$age_class
  temp1$rank <- as.numeric(rownames(temp1))

  temp1 <- temp1 %>%
    select(site, age_class, var, scenario, deviance, rank, model_num = num, 1:10)

  temp[[i]] <- temp1

}

fert_models <- tbl_df(bind_rows(temp))

# Since MuMIn fails to get intercepts for null models
temp <- mod5 %>%
  filter(var == "null") %>%
  group_by(site, age_class) %>%
  summarise(intercept = lapply(lapply(model, summary), coef)[[1]][, "Estimate"]) %>%
  mutate(var = "null")

for(i in 1:length(levels(fert_models$site))){
  current_site <- levels(fert_models$site)[i]
  fert_models[fert_models$site == current_site & fert_models$var == "null", ]$X.Intercept. <- temp[temp$site == current_site, ]$intercept
}


null_aic <- fert_models %>%
  filter(var == "null") %>%
  select(site, age_class, null_AICc = AICc, null_delta = delta,
         null_weight = weight, null_deviance = deviance)

fert_models <- inner_join(fert_models, null_aic)

fert_models$var <- factor(fert_models$var,
                          levels = c("rain_total_mm", "rain_anomaly_mean",
                                     "wettest_anomaly", "driest_anomaly",
                                     "shannon_rain", "spei_01_mean", "spei_03_mean",
                                     "spei_06_mean", "tmax_anomaly_mean",
                                     "tmax_detrended_mean", "hottest_tmax_anomaly",
                                     "tmin_anomaly_mean", "tmin_detrended_mean",
                                     "coldest_tmin_anomaly", "amo_mean", "nao_mean",
                                     "oni_mean", "pdo_mean", "dmi_mean", "sam_mean",
                                     "ao_mean", "null"))

fert_models$scenario <- mapvalues(fert_models$scenario,
                                  from = c("mod_0", "mod_1", "mod_0_1"),
                                  to = c("Lag 0", "Lag 1", "Lag 0 + Lag 1"))

fert_models$scenario <- factor(fert_models$scenario,
                               levels = rev(c("Lag 0", "Lag 1", "Lag 0 + Lag 1")))

fert_models %>%
  group_by(site, age_class) %>%
  # filter(scenario == "Lag 1") %>%
  top_n(2, -rank) %>%
  mutate(evidence_vs_null = weight / null_weight,
         delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance)) %>%
  select(-null_AICc, -null_delta, -null_weight, -null_deviance) %>%
  View()

fert_models %>%
  group_by(site, age_class, var) %>%
  top_n(1, -rank) %>%
  ungroup() %>%
  group_by(site, age_class) %>%
  top_n(20, -rank) %>%
  mutate(evidence_vs_null = weight / null_weight,
         delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance)) %>%
  select(-null_AICc, -null_delta, -null_weight, -null_deviance) %>%
  View()


# Plot

temp <- fert_models %>%
  filter(var %ni% c("null", "spei_03_mean", "spei_06_mean")) %>%
  mutate(D = 1 - (deviance / null_deviance))

lim <-  max(c(abs(min(temp$null_AICc - temp$AICc, na.rm = TRUE)),
              abs(max(temp$null_AICc - temp$AICc, na.rm = TRUE))))

ggplot(temp, aes(x = var, y = scenario, fill = (AICc - null_AICc))) +
# ggplot(temp, aes(x = var, y = scenario, fill = D)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = expression(paste(Delta, "AICc relative to Null Model")),
                       limits = c(-lim, lim),
                       trans = sqrt_sign_trans()) +
  facet_wrap(site ~ age_class, nrow = 7) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "\nClimate Variable", y = "Lag Scenario\n")

ggsave("plots/models/Fertility_AllLagScenarios_AIC.pdf",
       width = 8, height = 12, units = "in")

# Deviance

temp <- fert_models %>%
  filter(var %ni% c("null", "spei_03_mean", "spei_06_mean")) %>%
  mutate(D = 1 - (deviance / null_deviance))

lim <-  max(c(abs(min(temp$D, na.rm = TRUE)),
              abs(max(temp$D, na.rm = TRUE))))

ggplot(temp, aes(x = var, y = scenario, fill = D)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = rev(brewer.pal(9, "RdGy")),
                       name = expression(paste(Delta, "Proportional Reduction in Deviance")),
                       # trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  facet_wrap(site ~ age_class, nrow = 7) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "\nClimate Variable", y = "Lag Scenario\n")

ggsave("plots/models/Fertility_AllLagScenarios_Deviance.pdf",
       width = 8, height = 12, units = "in")


# Coefficients
temp <- mod5 %>%
  filter(scenario == "mod_0" & var %ni% c("null"))

temp1 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) coef(summary(x))["lag0", "Std. Error"]))
temp2 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) confint(x, method = "Wald")["lag0", ]))

temp <- fert_models %>%
  filter(scenario == "Lag 0" & var %ni% c("null"))

temp$var <- factor(temp$var)
temp2$var <- factor(temp2$var, levels = levels(factor(temp$var)))
temp1$var <- factor(temp1$var, levels = levels(factor(temp$var)))

temp_lag0 <- temp %>% inner_join(temp1) %>% inner_join(temp2)
names(temp_lag0)[20:22] <- c("se", "lower_ci", "upper_ci")

temp <- mod5 %>%
  filter(scenario == "mod_1" & var %ni% c("null"))

temp1 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) coef(summary(x))["lag1", "Std. Error"]))
temp2 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) confint(x, method = "Wald")["lag1", ]))

temp <- fert_models %>%
  filter(scenario == "Lag 1" & var %ni% c("null"))

temp$var <- factor(temp$var)
temp2$var <- factor(temp2$var, levels = levels(factor(temp$var)))
temp1$var <- factor(temp1$var, levels = levels(factor(temp$var)))

temp_lag1 <- temp %>% inner_join(temp1) %>% inner_join(temp2)
names(temp_lag1)[20:22] <- c("se", "lower_ci", "upper_ci")

temp <- bind_rows(temp_lag0, temp_lag1)

t <- temp %>%
  select(-scenario) %>%
  gather(lag, estimate, lag0, lag1) %>%
  filter(!is.na(estimate))

t$lag <- mapvalues(t$lag,
                   from = c("lag0", "lag1"),
                   to = c("Lag 0", "Lag 1"))

for(i in 1:length(levels(t$site))){

  current_site = levels(t$site)[i]

  temp4 <- filter(t, site == current_site & var %ni% c("spei_03_mean", "spei_06_mean"))

  lim <-  max(c(abs(min(temp4$null_AICc - temp4$AICc, na.rm = TRUE)),
                abs(max(temp4$null_AICc - temp4$AICc, na.rm = TRUE))))

  ggplot(temp4, aes(x = estimate, y = var, color = (AICc - null_AICc))) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = estimate - se, xmax = estimate + se),
                   height = 0.3, size = 0.75) +
    scale_color_gradientn(colours = brewer.pal(9, "RdGy"),
                          name = expression(paste(Delta, "AICc relative to Null Model")),
                          limits = c(-lim, lim),
                          trans = sqrt_sign_trans()) +
    geom_vline(xintercept = 0, lty = 2) +
    theme_bw() +
    facet_grid(lag ~ .) +
    theme(strip.background = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(0.2, "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "Scaled Coefficient Estimate\n", y = "\nPopulation",
         title = paste(current_site, ":\n",
                       "Scaled Coefficient Estimates for Lag-0 and Lag-1 Models of Adult Female Fertility\n",
                       sep = "")) +
    coord_flip()

  ggsave(paste("plots/models/Fertility_Lag0_Lag1_Coefficients_",
               i, "_", current_site, ".pdf", sep = ""),
         width = 8, height = 9, units = "in")
}






# ---- following_Adler_2012 -----------------------------------------------

mod3 <- mod_df %>%
  ungroup() %>%
  filter(!str_detect(var, "monthly") & !str_detect(var, "tavg")) %>%
  group_by(site, age_class, var) %>%
  do(mod_constant = glm(fate ~ 1, data = ., family = "binomial"),
     mod_climate_0 = glm(fate ~ lag0, data = ., family = "binomial"),
     mod_full_0 = glmer(fate ~ lag0 + (1 | year_of), data = ., family = "binomial"),
     mod_climate_1 = glm(fate ~ lag1, data = ., family = "binomial"),
     mod_full_1 = glmer(fate ~ lag1 + (1 | year_of), data = ., family = "binomial"))

mod4 <- NULL
k <- 1
for(i in 1:nrow(mod3)){
  for(j in 4:ncol(mod3)){
    temp <- select(mod3[i, ], 1:3, j)
    temp$scenario <- names(temp)[4]
    names(temp)[4] <- "model"
    mod4[[k]] <- temp
    k <- k + 1
  }
}

mod4 <- bind_rows(mod4)

mod4 <- filter(mod4, scenario != "mod_constant" | (scenario == "mod_constant" & var == "rain_anomaly_mean"))
var_levels <- c(levels(factor(mod4$var)), "constant")
mod4$var <- factor(mod4$var, levels = var_levels)
mod4[mod4$scenario == "mod_constant", ]$var <- "constant"

mod_sel <- mod4 %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$model),
     vars = data.frame(var = .$var),
     scenarios = data.frame(scenario = .$scenario),
     deviance = data.frame(deviance = unlist(lapply(.$model, deviance))))

temp <- NULL

for(i in 1:nrow(mod_sel)){
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

null_aic <- surv_models %>%
  filter(var == "constant") %>%
  select(site, age_class, null_AICc = AICc, null_delta = delta,
         null_weight = weight, null_deviance = deviance)

full_aic <- surv_models %>%
  filter(str_detect(scenario, "full_1")) %>%
  select(site, age_class, var, full_AICc = AICc, full_delta = delta,
         full_weight = weight, full_deviance = deviance)

climate_aic <- surv_models %>%
  filter(str_detect(scenario, "climate_1")) %>%
  select(site, age_class, var, climate_AICc = AICc, climate_delta = delta,
         climate_weight = weight, climate_deviance = deviance)

temp <- full_aic %>%
  inner_join(climate_aic) %>%
  inner_join(null_aic)

temp <- temp %>%
  mutate(D = (climate_deviance - null_deviance) / (full_deviance - null_deviance))
