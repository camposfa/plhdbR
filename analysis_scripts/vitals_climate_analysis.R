# devtools::install_github("camposfa/plhdbR")
#
# library(ggplot2)
# library(lme4)
# library(MuMIn)
# library(plhdbR)

# Sys.setenv(TZ = 'UTC')
#
# load(".RData")
# `%ni%` = Negate(`%in%`)
#
# load_plhdb_packages()

# f <- "data/biography_2015_06_17.csv"
# lh <- read_bio_table(f)
# summary(lh)
#
# # Fix errors
#
# find_bio_errors(lh)
#
# # Duplicate entries for Karisoke "SUS"
# lh <- lh %>%
#   filter(!(Animal.Id == "SUS" & (year(Birth.Date) == 2014 | year(Entry.Date) == 2014)))
#
# # Date error for "TEKINF"
# lh[lh$Animal.Id == "TEKINF", ]$Entry.Date <- ymd("2014-12-23")
#
#
# f <- "data/fertility_2015_06_17.csv"
# fert <- read_fert_table(f)
# summary(fert)
#
# find_fert_errors(fert)

# Takes about 1 minute
m <- stage_specific_survival(lh)
summary(m)

# Make trials
surv_trials <- make_survivorship_trials(m)
surv_trials <- filter(surv_trials, year_of < 2015)

temp <- climate_predictors %>%
  select(-n_months) %>%
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

climate_models <- tbl_df(bind_rows(temp))

null_aic <- climate_models %>%
  filter(var == "null") %>%
  select(site, age_class, null_AICc = AICc, null_delta = delta,
         null_weight = weight, null_deviance = deviance)

climate_models <- inner_join(climate_models, null_aic)

climate_models %>%
  group_by(site, age_class) %>%
  filter(scenario == "mod_1") %>%
  top_n(1, -rank) %>%
  mutate(evidence_vs_null = weight / null_weight,
         delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance)) %>%
  select(-null_AICc, -null_delta, -null_weight, -null_deviance) %>%
  View()



## CONSIDER SCALING VARIABLES FIRST!!!!
## Consider taking out variable with "monthly", as well as non-detrended variables


temp <- climate_models %>%
  filter(var != "null" & scenario == "mod_0") %>%
  mutate(evidence_vs_null = weight / null_weight,
         delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))

lim <-  max(c(abs(min(temp$D, na.rm = TRUE)),
              abs(max(temp$D, na.rm = TRUE))))

ggplot(temp, aes(x = var, y = site, fill = D)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdGy")),
                       name = "Proportional Reduction in Deviance",
                       limits = c(-lim, lim),
                       trans = sqrt_sign_trans()) +
  facet_wrap(~ age_class) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(2, "cm"),
        legend.key.height=unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "\nClimate Variable", y = "Population\n")


# Plot delta AICc

temp <- climate_models %>%
  filter(scenario == "mod_1" & var %ni% c("null"))

lim <-  max(c(abs(min(temp$null_AICc - temp$AICc, na.rm = TRUE)),
              abs(max(temp$null_AICc - temp$AICc, na.rm = TRUE))))

ggplot(temp, aes(x = var, y = site, fill = (AICc - null_AICc))) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = expression(paste(Delta, "AICc relative to Null Model")),
                       limits = c(-lim, lim)) +
  facet_wrap(~age_class) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "\nClimate Variable", y = "Population\n")


# Plot evidence ratios

temp <- climate_models %>%
  filter(scenario == "mod_1" & var %ni% c("null"))

lim <-  max(c(abs(min(temp$weight / temp$null_weight, na.rm = TRUE)),
              abs(max(temp$weight / temp$null_weight, na.rm = TRUE))))

ggplot(temp, aes(x = var, y = site, fill = log((weight / null_weight)))) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = c("#FFFFFF", brewer.pal(9, "Greens")),
                       name = "Log Evidence Ratio") +
  facet_wrap(~age_class) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "\nClimate Variable", y = "Population\n")




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

climate_models <- tbl_df(bind_rows(temp))

null_aic <- climate_models %>%
  filter(var == "constant") %>%
  select(site, age_class, null_AICc = AICc, null_delta = delta,
         null_weight = weight, null_deviance = deviance)

full_aic <- climate_models %>%
  filter(str_detect(scenario, "full_1")) %>%
  select(site, age_class, var, full_AICc = AICc, full_delta = delta,
         full_weight = weight, full_deviance = deviance)

climate_aic <- climate_models %>%
  filter(str_detect(scenario, "climate_1")) %>%
  select(site, age_class, var, climate_AICc = AICc, climate_delta = delta,
         climate_weight = weight, climate_deviance = deviance)

temp <- full_aic %>%
  inner_join(climate_aic) %>%
  inner_join(null_aic)

temp <- temp %>%
  mutate(D = (climate_deviance - null_deviance) / (full_deviance - null_deviance))
