rm(list = ls())
`%ni%` = Negate(`%in%`)

# If running the script from scratch
# load("ClimatePred1.RData")
load("ClimatePred2.RData")

# If script already run and resuming workspace
# load(".RData")

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

temp <- surv_models %>%
  filter(var != "null") %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))

lim <-  max(c(abs(min(temp$null_AICc - temp$AICc, na.rm = TRUE)),
              abs(max(temp$null_AICc - temp$AICc, na.rm = TRUE))))

ggplot(temp, aes(x = age_class, y = var, fill = (AICc - null_AICc))) +
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

temp1 <- temp %>%
  group_by(site, age_class) %>%
  mutate(min_AICc = min(AICc),
         delta = AICc - min_AICc) %>%
  ungroup() %>%
  group_by(site, age_class, var) %>%
  top_n(n = 1, wt = -rank)

colvec <- c(rep("#53777A", 4), rep("#542437", 4), rep("#C02942", 5), rep("#D95B43", 4))

ggplot(temp1, aes(y = var, x = age_class, fill = delta)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = c(rev(brewer.pal(9, "Reds")), "#FFFFFF"),
                       trans = log1p_trans(),
                       name = expression(paste(Delta, "AICc"))) +
  facet_grid(. ~ site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        legend.key.width = unit(2, "cm"),
        # axis.text.y = element_text(color = colvec),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.key.height = unit(0.2, "cm")) +
  labs(y = "Scale of Climate Variable\n", x = "\nAge Class") +
  coord_equal()

ggsave("plots/models/Survival_AllLagScenarios_AIC.pdf",
       width = 11, height = 8.5, units = "in")

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
  labs(y = "Climate Variable\n", y = "\nAge Class") +
  coord_equal()


# Plot Deviance
ggplot(temp, aes(x = age_class, y = var, fill = D)) +
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

temp <- mod4 %>%
  filter(scenario == "mod_2" & var %ni% c("null"))

temp1 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) coef(summary(x))["lag2", "Std. Error"]))
temp2 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) confint(x, method = "Wald")["lag2", ]))

temp <- surv_models %>%
  filter(scenario == "Lag 2" & var %ni% c("null"))

temp$var <- factor(temp$var)
temp2$var <- factor(temp2$var, levels = levels(factor(temp$var)))
temp1$var <- factor(temp1$var, levels = levels(factor(temp$var)))

temp_lag2 <- temp %>% inner_join(temp1) %>% inner_join(temp2)
names(temp_lag2)[(ncol(temp_lag2) - 2):ncol(temp_lag2)] <- c("se", "lower_ci", "upper_ci")

temp <- bind_rows(temp_lag0, temp_lag1, temp_lag2)

t <- temp %>%
  select(-scenario) %>%
  gather(lag, estimate, lag0, lag1, lag2) %>%
  filter(!is.na(estimate))

t$lag <- mapvalues(t$lag,
                   from = c("lag0", "lag1", "lag2"),
                   to = c("Lag 0", "Lag 1", "Lag 2"))

lim <-  max(c(abs(min(t$null_AICc - t$AICc, na.rm = TRUE)),
              abs(max(t$null_AICc - t$AICc, na.rm = TRUE))))


# Plot coefficients separately for each population

# for(i in 1:length(levels(temp$site))){
#
#   current_site = levels(temp$site)[i]
#
#   temp4 <- filter(temp, site == current_site)
#
#   lim <-  max(c(abs(min(temp4$null_AICc - temp4$AICc, na.rm = TRUE)),
#                 abs(max(temp4$null_AICc - temp4$AICc, na.rm = TRUE))))
#
#   ggplot(temp4, aes(x = lag1, y = var, color = (AICc - null_AICc))) +
#     geom_point(size = 3) +
#     geom_errorbarh(aes(xmin = lag1 - se, xmax = lag1 + se),
#                    height = 0.3, size = 0.75) +
#     scale_color_gradientn(colours = brewer.pal(9, "RdGy"),
#                           name = expression(paste(Delta, "AICc relative to Null Model")),
#                           limits = c(-lim, lim),
#                           trans = sqrt_sign_trans()) +
#     geom_vline(xintercept = 0, lty = 2) +
#     facet_grid(age_class ~ .) +
#     theme_bw() +
#     theme(strip.background = element_blank(),
#           legend.position = "bottom",
#           legend.key.width = unit(2, "cm"),
#           legend.key.height = unit(0.2, "cm"),
#           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#     labs(x = "Scaled Coefficient Estimate\n", y = "\nPopulation",
#          title = paste(current_site, ":\n",
#                        "Scaled Coefficient Estimates for Lag-1 Models of Stage-specific Survival\n",
#                        sep = "")) +
#     coord_flip()
#
# #   ggsave(paste("plots/models/Survival_Lag1_Coefficients_",
# #                i, "_", current_site, ".pdf", sep = ""),
# #          width = 8, height = 11, units = "in")
# }

# Plot both lag scenarios for each site separately

t <- t %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))

lim <-  max(c(abs(min(t$D, na.rm = TRUE)),
              abs(max(t$D, na.rm = TRUE))))

for (i in 1:length(levels(t$site))) {

  current_site = levels(t$site)[i]

  temp4 <- filter(t, site == current_site)

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
    labs(x = "Scaled Coefficient Estimate\n", y = "\nVariable",
         title = paste(current_site, ":\n",
                       "Scaled Coefficient Estimates for Models of Survival\n",
                       sep = "")) +
    coord_flip()

  ggsave(paste("plots/models/Survival_Lags_Coefficients_",
               i, "_", current_site, ".pdf", sep = ""),
         width = 14, height = 11, units = "in")
}




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


# Plot

temp <- fert_models %>%
  filter(var != "null") %>%
  mutate(D = 1 - (deviance / null_deviance))

lim <-  max(c(abs(min(temp$null_AICc - temp$AICc, na.rm = TRUE)),
              abs(max(temp$null_AICc - temp$AICc, na.rm = TRUE))))

ggplot(temp, aes(x = var, y = scenario, fill = (AICc - null_AICc))) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = expression(paste(Delta, "AICc relative to Null Model")),
                       # trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  facet_wrap(~site, nrow = 7) +
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

lim <- max(temp$D)

# Deviance
ggplot(temp, aes(y = var, x = scenario, fill = D)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = c("#FFFFFF", brewer.pal(9, "Greens")),
                       name = "Proportional Reduction in Deviance",
                       limits = c(0, lim)) +
  facet_grid(. ~ site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.5, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "\nClimate Variable", y = "Lag Scenario\n") +
  coord_equal()

ggsave("plots/models/Fertility_AllLagScenarios_Deviance.pdf",
       width = 8, height = 12, units = "in")


# Coefficients
temp <- mod5 %>%
  filter(scenario == "mod_0" & var %ni% c("null"))

temp1 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) coef(summary(x))["lag0", "Std. Error"]))
temp2 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) confint(x, method = "Wald")["lag0", ]))

temp1 <- select(temp1, -model)
temp2 <- select(temp2, -model)

temp <- fert_models %>%
  filter(scenario == "Lag 0" & var %ni% c("null"))

temp$var <- factor(temp$var)
temp2$var <- factor(temp2$var, levels = levels(factor(temp$var)))
temp1$var <- factor(temp1$var, levels = levels(factor(temp$var)))

temp_lag0 <- temp %>%
  inner_join(temp1, by = c("site", "var")) %>%
  inner_join(temp2, by = c("site", "var"))

names(temp_lag0)[(ncol(temp_lag0) - 2):ncol(temp_lag0)] <- c("se", "lower_ci", "upper_ci")

temp <- mod5 %>%
  filter(scenario == "mod_1" & var %ni% c("null"))

temp1 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) coef(summary(x))["lag1", "Std. Error"]))
temp2 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) confint(x, method = "Wald")["lag1", ]))

temp1 <- select(temp1, -model)
temp2 <- select(temp2, -model)

temp <- fert_models %>%
  filter(scenario == "Lag 1" & var %ni% c("null"))

temp$var <- factor(temp$var)
temp2$var <- factor(temp2$var, levels = levels(factor(temp$var)))
temp1$var <- factor(temp1$var, levels = levels(factor(temp$var)))

temp_lag1 <- temp %>%
  inner_join(temp1) %>%
  inner_join(temp2)

names(temp_lag1)[(ncol(temp_lag1) - 2):ncol(temp_lag1)] <- c("se", "lower_ci", "upper_ci")

temp <- mod5 %>%
  filter(scenario == "mod_2" & var %ni% c("null"))

temp1 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) coef(summary(x))["lag2", "Std. Error"]))
temp2 <- cbind(temp[, 1:3], ldply(temp$model, .fun = function(x) confint(x, method = "Wald")["lag2", ]))

temp1 <- select(temp1, -model)
temp2 <- select(temp2, -model)

temp <- fert_models %>%
  filter(scenario == "Lag 2" & var %ni% c("null"))

temp$var <- factor(temp$var)
temp2$var <- factor(temp2$var, levels = levels(factor(temp$var)))
temp1$var <- factor(temp1$var, levels = levels(factor(temp$var)))

temp_lag2 <- temp %>%
  inner_join(temp1) %>%
  inner_join(temp2)

names(temp_lag2)[(ncol(temp_lag2) - 2):ncol(temp_lag2)] <- c("se", "lower_ci", "upper_ci")

temp <- bind_rows(temp_lag0, temp_lag1, temp_lag2)

t <- temp %>%
  select(-scenario) %>%
  gather(lag, estimate, lag0, lag1, lag2) %>%
  filter(!is.na(estimate))

t$lag <- mapvalues(t$lag,
                   from = c("lag0", "lag1", "lag2"),
                   to = c("Lag 0", "Lag 1", "Lag 2"))

lim <- max(t$delta)

# colvec <- c(rep("#53777A", 5), rep("#542437", 4), rep("#C02942", 6), rep("#D95B43", 8))
#
# heat.pal <- heat_hcl(9, c = c(80, 30), l = c(30, 90), power = c(1/5, 2))

for (i in 1:length(levels(t$site))) {

  current_site = levels(t$site)[i]

  temp4 <- filter(t, site == current_site)

  ggplot(temp4, aes(x = estimate, y = var, color = delta)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci),
                   height = 0.3, size = 0.75) +
    scale_color_gradientn(colours = brewer.pal(9, "Greens")[2:9],
                          name = expression(paste(Delta, "AICc")),
                          # trans = sqrt_trans(),
                          limits = c(0, lim)) +
    geom_vline(xintercept = 0, lty = 2) +
    theme_bw() +
    facet_grid(. ~ lag) +
    theme(strip.background = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(0.2, "cm"),
          axis.text.y = element_text(colour = colvec)) +
    labs(x = "\nScaled Coefficient Estimate", y = "Variable\n",
         title = paste(current_site, ":\n",
                       "Scaled Coefficient Estimates for\nModels of Adult Female Fertility\n",
                       sep = ""))

  ggsave(paste("plots/models/Fertility_Lag0_Lag1_Coefficients_",
               i, "_", current_site, ".pdf", sep = ""),
         width = 7, height = 8, units = "in")
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
