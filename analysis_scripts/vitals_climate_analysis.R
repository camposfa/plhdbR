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

temp <- ann_mean %>%
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
  group_by(site, age_class, var) %>%
  do(mod_null = glmer(fate ~ 1 + (1 | year_of), data = ., family = "binomial"),
     mod_0 = glmer(fate ~ lag0 + (1 | year_of), data = ., family = "binomial"),
     mod_0_1 = glmer(fate ~ lag0 + lag1 + (1 | year_of), data = ., family = "binomial"),
     mod_0_1_2 = glmer(fate ~ lag0 + lag1 + lag2 + (1 | year_of), data = ., family = "binomial"),
     mod_1 = glmer(fate ~ lag1 + (1 | year_of), data = ., family = "binomial"),
     mod_1_2 = glmer(fate ~ lag1 + lag2 + (1 | year_of), data = ., family = "binomial"),
     mod_2 = glmer(fate ~ lag2 + (1 | year_of), data = ., family = "binomial"))

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

mod4 <- filter(mod4, scenario != "mod_null" | (scenario == "mod_null" & var == "rain_monthly_mm_mean"))
var_levels <- c(levels(factor(mod4$var)), "null")
mod4$var <- factor(mod4$var, levels = var_levels)
mod4[mod4$scenario == "mod_null", ]$var <- "null"

mod_sel <- mod4 %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$model),
     vars = data.frame(var = .$var),
     scenarios = data.frame(scenario = .$scenario))

temp <- NULL

for(i in 1:nrow(mod_sel)){
  m_table <- data.frame(mod_sel[i, ]$m_table[[1]])
  m_table$num <- rownames(m_table)

  vars <- mod_sel[i, ]$vars[[1]]
  vars$num <- rownames(vars)

  scenarios <- mod_sel[i, ]$scenarios[[1]]
  scenarios$num <- rownames(scenarios)

  temp1 <- suppressMessages(inner_join(m_table, vars))
  temp1 <- suppressMessages(inner_join(temp1, scenarios))
  temp1$site <- mod_sel[i, ]$site
  temp1$age_class <- mod_sel[i, ]$age_class
  temp1$rank <- as.numeric(rownames(temp1))

  temp1 <- temp1 %>%
    select(site, age_class, var, scenario, rank, model_num = num, 1:8)

  temp[[i]] <- temp1

}

climate_models <- tbl_df(bind_rows(temp))

null_aic <- climate_models %>%
  filter(var == "null") %>%
  select(site, age_class, null_AICc = AICc, null_delta = delta)

climate_models <- inner_join(climate_models, null_aic)

climate_models %>%
  group_by(site, age_class) %>%
  # filter(scenario %in% c("mod_0", "mod_1", "mod_0_1")) %>%
  top_n(1, -rank) %>%
  mutate(evidence = null_AICc - AICc) %>%
  select(-null_AICc, -null_delta) %>%
  View()



## CONSIDER SCALING VARIABLES FIRST!!!!
## Consider taking out variable with "monthly", as well as non-detrended variables



ggplot(climate_models, aes(x = var, y = scenario, fill = delta)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "Spectral")),
                       name = "AICc") +
  facet_wrap(site ~ age_class, nrow = 7) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(2, "cm"),
        legend.key.height=unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))


temp <- filter(climate_models, site == "beza" & var %ni% c("null"))

lim <-  max(c(abs(min(temp$null_AICc - temp$AICc, na.rm = TRUE)),
              abs(max(temp$null_AICc - temp$AICc, na.rm = TRUE))))

ggplot(temp, aes(x = var, y = scenario, fill = (AICc - null_AICc))) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = "Delta AICc relative to Null Model",
                       limits = c(-lim, lim)) +
  facet_wrap(~age_class) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5))











# Lag scenarios
temp <- ann_mean %>%
  group_by(site) %>%
  arrange(year_of) %>%
  select(-n_months) %>%
  mutate_each(funs(lag), -year_of) %>%
  setNames(c(names(.)[c(1:2)], paste0(names(.)[-c(1:2)],"_lag1"))) %>%
  left_join(ann_mean, .)

# Models
mod_df <- surv_trials %>%
  rename(site = Study.Id) %>%
  left_join(temp) %>%
  gather(var, value, contains("mean"), shannon_rain, contains("lag1"))

mod <- mod_df %>%
  ungroup() %>%
  group_by(site, age_class, var) %>%
  do(climate_mod = glmer(fate ~ value + (1 | year_of), data = ., family = "binomial"))

mod_null <- mod_df %>%
  filter(var == "rain_monthly_mm_mean") %>%
  group_by(site, age_class, var) %>%
  do(climate_mod = glmer(fate ~ 1 + (1 | year_of), data = ., family = "binomial"))

mod_null$var <- mapvalues(mod_null$var,
                          from = "rain_monthly_mm_mean",
                          to = "null")

var_levels <- c(levels(factor(mod$var)), "null")

mod2 <- bind_rows(mod, mod_null)
mod2$var <- factor(mod2$var, levels = var_levels)

mod2 <- mod2 %>%
  arrange(site, age_class, var)


# One-ine summary for each model using glance function
mod_glance <- mod %>%
  do(glance(.$climate_mod))

mod_glance$site <- mod$site
mod_glance$age_class <- mod$age_class
mod_glance$var <- mod$var

# mod_sel <- mod %>%
#   do(site = first(.$site),
#      age_class = first(.$age_class),
#      sel = model.sel(.$climate_mod, .$null_mod)) %>%
#   mutate(site = unlist(site),
#          age_class = unlist(age_class))

mod_sel <- mod2 %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$climate_mod),
     vars = data.frame(var = .$var))

temp <- NULL

for(i in 1:nrow(mod_sel)){
  m_table <- data.frame(mod_sel[i, ]$m_table[[1]])
  m_table$num <- rownames(m_table)

  vars <- mod_sel[i, ]$vars[[1]]
  vars$num <- rownames(vars)

  temp1 <- suppressMessages(inner_join(m_table, vars))
  temp1$site <- mod_sel[i, ]$site
  temp1$age_class <- mod_sel[i, ]$age_class
  temp1$rank <- rownames(temp1)

  temp1 <- temp1 %>%
    select(site, age_class, var, rank, model_num = num, 1:8)

  temp[[i]] <- temp1

}

temp <- tbl_df(bind_rows(temp))

# mod_glance %>%
#   group_by(site, age_class) %>%
#   top_n(2, -AIC) %>%
#   select(site, age_class, var, AIC) %>%
#   mutate(delta = round(AIC - lag(AIC), 3)) %>%
#   View()

mod_glance %>%
  group_by(site, age_class) %>%
  arrange(AIC) %>%
  mutate(delta = round(lead(AIC) - AIC, 3)) %>%
  top_n(1) %>%
  select(site, age_class, var, AIC, delta) %>%
  View()

# models <- list()
# count <- 1
#
# for(i in 1:length(levels(temp$Study.Id))){
#
#   current_study <- levels(temp$Study.Id)[i]
#
#   for(j in 1:length(levels(factor(temp$age_class)))){
#     current_ac <- levels(temp$age_class)[j]
#
#     set <- temp %>%
#       filter(Study.Id == current_study & age_class == current_ac) %>%
#       data.frame()
#
#     models[[count]] <- glmer(fate ~ 1 + (1 | year_of), data = set, family = "binomial")
#
#     count <- count + 1
#   }
#
# }
#
# out <- models[[19]]
#
# x=coef(out)$year_of
# x=as.vector(x[,1])
# x=exp(x)/(1+exp(x))
