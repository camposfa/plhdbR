
# ---- SURVIVAL -----------------------------------------------------------

# Takes about 1 minute
m <- stage_specific_survival(lh)
summary(m)

climate_predictors2 <- bioclim_df %>%
  left_join(select(ann_div, site, year_of, shannon_rain)) %>%
  left_join(indclim_df) %>%
  left_join(speiclim_df)

# Make trials
surv_trials <- make_survivorship_trials(m)

surv_trials <- surv_trials %>%
  ungroup() %>%
  filter(year_of < 2015) %>%
  select(site = Study.Id, year_of, age_class, fate)

temp <- climate_predictors2 %>%
  ungroup() %>%
  mutate_each(funs(scale), -site, -year_of) %>%
  group_by(site) %>%
  gather(var, lag0, -site, -year_of) %>%
  group_by(site, var) %>%
  arrange(year_of) %>%
  mutate(lag1 = lag(lag0),
         lag2 = lag(lag0, n = 2),
         lag3 = lag(lag0, n = 3))

temp$site <- factor(temp$site,
                    levels = c("rppn-fma", "amboseli", "kakamega",
                               "gombe", "karisoke",
                               "beza", "ssr"))

# Models
mod_df <- surv_trials %>%
  left_join(temp) %>%
  filter(!is.na(lag0)) %>%
  arrange(site, var, year_of)

mod_df$site <- mapvalues(mod_df$site,
                         from = levels(mod_df$site),
                         to = c("Muriqui", "Baboon", "Blue Monkey",
                                "Chimpanzee", "Gorilla", "Sifaka",
                                "Capuchin"))

mod_df <- mod_df %>%
  mutate(scale = ifelse(var %in% colnames(bioclim_df), "local",
                        ifelse(var %in% colnames(indclim_df), "global",
                               ifelse(var %in% colnames(speiclim_df), "spei",
                                      "local"))))



# ---- Survival_Null_Models -----------------------------------------------

n <- mod_df %>%
  ungroup() %>%
  filter(scale == "local" & var == levels(mod_df$var)[1]) %>%
  arrange(site, age_class, var, year_of)

n$var <- "null"

n_models <- n %>%
  ungroup() %>%
  group_by(site, age_class, var) %>%
  do(mod_null = glmer(fate ~ 1 + (1 | year_of),
                      data = ., family = "binomial"))

n_models_sel <- NULL
k <- 1
for (i in 1:nrow(n_models)) {
  for (j in 4:ncol(n_models)) {
    temp <- select(n_models[i, ], 1:3, j)
    temp$scenario <- names(temp)[4]
    names(temp)[4] <- "model"
    n_models_sel[[k]] <- temp
    k <- k + 1
  }
}

n_models_sel <- bind_rows(n_models_sel)

var_levels <- c(levels(factor(n_models_sel$var)))
n_models_sel$var <- factor(n_models_sel$var, levels = var_levels)

n_mod_sel <- n_models_sel %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$model, extra = c("AIC")),
     vars = data.frame(var = .$var),
     scenarios = data.frame(scenario = .$scenario),
     deviance = data.frame(deviance = unlist(lapply(.$model, deviance))))



# Since MuMIn fails to get intercepts for null models
temp <- n_models %>%
  group_by(site, age_class) %>%
  summarise(X.Intercept. = lapply(lapply(mod_null, summary), coef)[[1]][, "Estimate"]) %>%
  mutate(var = "null")

for (i in 1:nrow(n_mod_sel)) {
  n_mod_sel[i, ]$m_table[[1]][, 1] <- temp[i, ]$X.Intercept.
  names(n_mod_sel[i, ]$m_table[[1]])[1] <- "X.Intercept."
}

temp <- NULL

for (i in 1:nrow(n_mod_sel)) {
  m_table <- data.frame(n_mod_sel[i, ]$m_table[[1]])
  m_table$num <- rownames(m_table)

  vars <- n_mod_sel[i, ]$vars[[1]]
  vars$num <- rownames(vars)

  scenarios <- n_mod_sel[i, ]$scenarios[[1]]
  scenarios$num <- rownames(scenarios)

  deviance <- n_mod_sel[i, ]$deviance[[1]]
  deviance$num <- rownames(deviance)

  temp1 <- suppressMessages(inner_join(m_table, vars))
  temp1 <- suppressMessages(inner_join(temp1, scenarios))
  temp1 <- suppressMessages(inner_join(temp1, deviance))
  temp1$site <- n_mod_sel[i, ]$site
  temp1$age_class <- n_mod_sel[i, ]$age_class
  temp1$rank <- as.numeric(rownames(temp1))

  temp1 <- temp1 %>%
    select(site, age_class, var, scenario, deviance, rank, model_num = num, 1:11)

  temp[[i]] <- temp1

}

n_surv_models <- tbl_df(bind_rows(temp))

n_models_sel$scenario <- factor(n_models_sel$scenario)

n_best <- inner_join(n_surv_models, n_models_sel)

rm(list = c("n_models", "n_models_sel", "n_mod_sel", "temp", "n_surv_models",
            "m_table", "vars", "scenarios", "deviance", "temp1", "n"))





# ---- Survival_Global_Variables ------------------------------------------

# Find best model for each global climate oscillation index
g <- filter(mod_df, scale == "global")
g <- separate(g, var, c("index", "var"), fixed("_v"))

res <- list()
res_avg <- list()
counter <- 1

for (j in levels(factor(g$site))) {

  current_site <- as.character(j)

  g_models <- g %>%
    filter(site == current_site) %>%
    ungroup() %>%
    group_by(index, age_class, var) %>%
    do(mod_0 = glmer(fate ~ lag0 + (1 | year_of),
                     data = ., family = "binomial"),
       mod_1 = glmer(fate ~ lag1 + (1 | year_of),
                     data = ., family = "binomial"),
       mod_2 = glmer(fate ~ lag2 + (1 | year_of),
                     data = ., family = "binomial"))


  g_models_sel <- NULL
  k <- 1
  for (i in 1:nrow(g_models)) {
    for (ii in 4:ncol(g_models)) {
      temp <- select(g_models[i, ], 1:3, ii)
      temp$scenario <- names(temp)[4]
      names(temp)[4] <- "model"
      g_models_sel[[k]] <- temp
      k <- k + 1
    }
  }

  g_models_sel <- bind_rows(g_models_sel)

  var_levels <- c(levels(factor(g_models_sel$var)))
  g_models_sel$var <- factor(g_models_sel$var, levels = var_levels)

  g_mod_sel <- g_models_sel %>%
    group_by(index, age_class) %>%
    do(m_table = model.sel(.$model, extra = c("AIC")),
       vars = data.frame(var = .$var),
       scenarios = data.frame(scenario = .$scenario),
       deviance = data.frame(deviance = unlist(lapply(.$model, deviance))))

  temp <- NULL

  for (i in 1:nrow(g_mod_sel)) {
    m_table <- data.frame(g_mod_sel[i, ]$m_table[[1]])
    m_table$num <- rownames(m_table)

    vars <- g_mod_sel[i, ]$vars[[1]]
    vars$num <- rownames(vars)

    scenarios <- g_mod_sel[i, ]$scenarios[[1]]
    scenarios$num <- rownames(scenarios)

    deviance <- g_mod_sel[i, ]$deviance[[1]]
    deviance$num <- rownames(deviance)

    temp1 <- suppressMessages(inner_join(m_table, vars))
    temp1 <- suppressMessages(inner_join(temp1, scenarios))
    temp1 <- suppressMessages(inner_join(temp1, deviance))
    temp1$index <- g_mod_sel[i, ]$index
    temp1$age_class <- g_mod_sel[i, ]$age_class
    temp1$rank <- as.numeric(rownames(temp1))

    temp1 <- temp1 %>%
      select(index, age_class, var, scenario, deviance, rank, model_num = num, 1:10)

    temp[[i]] <- temp1

  }

  g_surv_models <- tbl_df(bind_rows(temp))

  g_best <- filter(g_surv_models, rank == 1)

  g_avg <- g_surv_models %>%
    filter(str_detect(var, "annual_mean")) %>%
    group_by(index, age_class) %>%
    top_n(n = 1, wt = -rank)

  temp <- suppressMessages(inner_join(g_models_sel,
                                      select(g_best, index, age_class, var, scenario)))

  temp$site <- current_site

  res[[counter]] <- temp

  temp <- suppressMessages(inner_join(g_models_sel,
                                      select(g_avg, index, age_class, var, scenario)))

  temp$site <- current_site

  res_avg[[counter]] <- temp

  counter <- counter + 1
}

# Best among all models
g <- bind_rows(res)

# Best among annual average models only
g_avg <- bind_rows(res_avg)

# Given this set of "best global models" (one for each site/age class/index),
# find the one that is best for each site and age class
temp <- g %>%
  ungroup() %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$model, extra = c("AIC")),
     vars = data.frame(var = .$var),
     inds = data.frame(ind = .$index),
     scenarios = data.frame(scenario = .$scenario),
     deviance = data.frame(deviance = unlist(lapply(.$model, deviance))))

g_best <- list()

for (i in 1:nrow(temp)) {
  m_table <- data.frame(temp[i, ]$m_table[[1]])
  m_table$num <- rownames(m_table)

  inds <- temp[i, ]$inds[[1]]
  inds$num <- rownames(inds)

  vars <- temp[i, ]$vars[[1]]
  vars$num <- rownames(vars)

  scenarios <- temp[i, ]$scenarios[[1]]
  scenarios$num <- rownames(scenarios)

  deviance <- temp[i, ]$deviance[[1]]
  deviance$num <- rownames(deviance)

  temp1 <- suppressMessages(inner_join(m_table, inds))
  temp1 <- suppressMessages(inner_join(temp1, vars))
  temp1 <- suppressMessages(inner_join(temp1, scenarios))
  temp1 <- suppressMessages(inner_join(temp1, deviance))
  temp1$site <- temp[i, ]$site
  temp1$age_class <- temp[i, ]$age_class
  temp1$rank <- as.numeric(rownames(temp1))

  temp1 <- temp1 %>%
    select(site, age_class, ind, var, scenario, deviance, rank, model_num = num, 1:11)

  g_best[[i]] <- temp1

}

g_rank <- bind_rows(g_best)
g_best <- filter(g_rank, rank == 1)

# Obtain final set
g <- rename(g, ind = index)
g$ind <- factor(g$ind, levels = levels(factor(g$ind)))
g_best <- inner_join(g_best, g)
g_rank <- inner_join(g_rank, g)


# Plot
surv_g_models_plot <- bind_rows(g_rank, n_best)

surv_g_models_plot <- surv_g_models_plot %>%
  select(-rank, -model_num, -delta, -weight, -model) %>%
  arrange(site, age_class, AICc)

null_aic <- surv_g_models_plot %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_AICc = AICc)

null_deviance <- surv_g_models_plot %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_deviance = deviance)

surv_g_models_plot <- surv_g_models_plot %>%
  inner_join(null_aic) %>%
  inner_join(null_deviance) %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))

lim <-  max(c(abs(min(surv_g_models_plot$delta_AICc_vs_null, na.rm = TRUE)),
              abs(max(surv_g_models_plot$delta_AICc_vs_null, na.rm = TRUE))))

ggplot(filter(surv_g_models_plot, var != "null"),
              aes(y = ind, x = age_class, fill = delta_AICc_vs_null)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = expression(paste(Delta, "AICc relative to Null Model")),
                       # trans = sqrt_sign_trans() +
                       limits = c(-lim, lim)) +
  facet_wrap(~site, nrow = 1, drop = TRUE, scales = "free") +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "\nClimate Oscillation Index", y = "Age Class\n")

rm(list = c("g_models", "g_models_sel", "g_mod_sel", "temp", "m_table", "inds",
            "vars", "scenarios", "deviance", "temp1", "g", "g_surv_models"))





# ---- Survival_Local_Variables -------------------------------------------

# Find best model for each variable
l <- mod_df %>%
  ungroup() %>%
  filter(scale == "local") %>%
  arrange(site, age_class, var, year_of)

l_models <- l %>%
  ungroup() %>%
  group_by(site, age_class, var) %>%
  do(mod_0 = glmer(fate ~ lag0 + (1 | year_of),
                   data = ., family = "binomial"),
     mod_1 = glmer(fate ~ lag1 + (1 | year_of),
                   data = ., family = "binomial"),
     mod_2 = glmer(fate ~ lag2 + (1 | year_of),
                   data = ., family = "binomial"))


l_models_sel <- NULL
k <- 1
for (i in 1:nrow(l_models)) {
  for (j in 4:ncol(l_models)) {
    temp <- select(l_models[i, ], 1:3, j)
    temp$scenario <- names(temp)[4]
    names(temp)[4] <- "model"
    l_models_sel[[k]] <- temp
    k <- k + 1
  }
}

l_models_sel <- bind_rows(l_models_sel)

var_levels <- c(levels(factor(l_models_sel$var)))
l_models_sel$var <- factor(l_models_sel$var, levels = var_levels)

l_mod_sel <- l_models_sel %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$model, extra = c("AIC")),
     vars = data.frame(var = .$var),
     scenarios = data.frame(scenario = .$scenario),
     deviance = data.frame(deviance = unlist(lapply(.$model, deviance))))

temp <- NULL

for (i in 1:nrow(l_mod_sel)) {
  m_table <- data.frame(l_mod_sel[i, ]$m_table[[1]])
  m_table$num <- rownames(m_table)

  vars <- l_mod_sel[i, ]$vars[[1]]
  vars$num <- rownames(vars)

  scenarios <- l_mod_sel[i, ]$scenarios[[1]]
  scenarios$num <- rownames(scenarios)

  deviance <- l_mod_sel[i, ]$deviance[[1]]
  deviance$num <- rownames(deviance)

  temp1 <- suppressMessages(inner_join(m_table, vars))
  temp1 <- suppressMessages(inner_join(temp1, scenarios))
  temp1 <- suppressMessages(inner_join(temp1, deviance))
  temp1$site <- l_mod_sel[i, ]$site
  temp1$age_class <- l_mod_sel[i, ]$age_class
  temp1$rank <- as.numeric(rownames(temp1))

  temp1 <- temp1 %>%
    select(site, age_class, var, scenario, deviance, rank, model_num = num, 1:11)

  temp[[i]] <- temp1

}

l_surv_models <- tbl_df(bind_rows(temp))

# Get final set
l_best <- filter(l_surv_models, rank == 1)

l_models_sel$scenario <- factor(l_models_sel$scenario, levels = levels(l_best$scenario))
l_best <- inner_join(l_best, l_models_sel)

surv_l_models_plot <- bind_rows(l_surv_models, n_best)

surv_l_models_plot <- surv_l_models_plot %>%
  select(-rank, -model_num, -delta, -weight, -model) %>%
  arrange(site, age_class, AICc)

null_aic <- surv_l_models_plot %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_AICc = AICc)

null_deviance <- surv_l_models_plot %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_deviance = deviance)

surv_l_models_plot <- surv_l_models_plot %>%
  inner_join(null_aic) %>%
  inner_join(null_deviance) %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))

# surv_l_models_plot1 <- filter(surv_l_models_plot, site == "Muriqui")

lim <-  max(c(abs(min(surv_l_models_plot$delta_AICc_vs_null, na.rm = TRUE)),
              abs(max(surv_l_models_plot$delta_AICc_vs_null, na.rm = TRUE))))

ggplot(filter(surv_l_models_plot, var != "null"),
       aes(x = var, y = scenario, fill = delta_AICc_vs_null)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = expression(paste(Delta, "AICc relative to Null Model")),
                       trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  facet_wrap(age_class ~ site, nrow = 3, drop = TRUE, scales = "free") +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "\nLocal Climate Variable", y = "Age Class\n")

ggplot(filter(surv_l_models_plot, var != "null"),
       aes(y = var, x = scenario, fill = delta_AICc_vs_null)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = expression(paste(Delta, "AICc relative to Null Model")),
                       trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  facet_grid(age_class ~ site, drop = TRUE) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "\nLocal Climate Variable", x = "Age Class\n")

rm(list = c("l_models", "l_models_sel", "l_mod_sel", "surv_l_models_plot", "m_table",
            "vars", "scenarios", "deviance", "surv_l_models_plot1", "l"))




# ---- Survival_Drought_Variables -----------------------------------------

# Find best model for each variable
d <- mod_df %>%
  ungroup() %>%
  filter(scale == "spei") %>%
  arrange(site, age_class, var, year_of)

d_models <- d %>%
  ungroup() %>%
  group_by(site, age_class, var) %>%
  do(mod_0 = glmer(fate ~ lag0 + (1 | year_of),
                   data = ., family = "binomial"),
     mod_1 = glmer(fate ~ lag1 + (1 | year_of),
                   data = ., family = "binomial"),
     mod_2 = glmer(fate ~ lag2 + (1 | year_of),
                   data = ., family = "binomial"))


d_models_sel <- NULL
k <- 1
for (i in 1:nrow(d_models)) {
  for (j in 4:ncol(d_models)) {
    temp <- select(d_models[i, ], 1:3, j)
    temp$scenario <- names(temp)[4]
    names(temp)[4] <- "model"
    d_models_sel[[k]] <- temp
    k <- k + 1
  }
}

d_models_sel <- bind_rows(d_models_sel)

var_levels <- c(levels(factor(d_models_sel$var)))
d_models_sel$var <- factor(d_models_sel$var, levels = var_levels)

d_mod_sel <- d_models_sel %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$model, extra = c("AIC")),
     vars = data.frame(var = .$var),
     scenarios = data.frame(scenario = .$scenario),
     deviance = data.frame(deviance = unlist(lapply(.$model, deviance))))

temp <- NULL

for (i in 1:nrow(d_mod_sel)) {
  m_table <- data.frame(d_mod_sel[i, ]$m_table[[1]])
  m_table$num <- rownames(m_table)

  vars <- d_mod_sel[i, ]$vars[[1]]
  vars$num <- rownames(vars)

  scenarios <- d_mod_sel[i, ]$scenarios[[1]]
  scenarios$num <- rownames(scenarios)

  deviance <- d_mod_sel[i, ]$deviance[[1]]
  deviance$num <- rownames(deviance)

  temp1 <- suppressMessages(inner_join(m_table, vars))
  temp1 <- suppressMessages(inner_join(temp1, scenarios))
  temp1 <- suppressMessages(inner_join(temp1, deviance))
  temp1$site <- d_mod_sel[i, ]$site
  temp1$age_class <- d_mod_sel[i, ]$age_class
  temp1$rank <- as.numeric(rownames(temp1))

  temp1 <- temp1 %>%
    select(site, age_class, var, scenario, deviance, rank, model_num = num, 1:11)

  temp[[i]] <- temp1

}

d_surv_models <- tbl_df(bind_rows(temp))

# Get final set
d_best <- filter(d_surv_models, rank == 1)

d_models_sel$scenario <- factor(d_models_sel$scenario, levels = levels(d_best$scenario))
d_best <- inner_join(d_best, d_models_sel)

surv_d_models_plot <- bind_rows(d_surv_models, n_best)

surv_d_models_plot <- surv_d_models_plot %>%
  select(-rank, -model_num, -delta, -weight, -model) %>%
  arrange(site, age_class, AICc)

null_aic <- surv_d_models_plot %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_AICc = AICc)

null_deviance <- surv_d_models_plot %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_deviance = deviance)

surv_d_models_plot <- surv_d_models_plot %>%
  inner_join(null_aic) %>%
  inner_join(null_deviance) %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))


lim <-  max(c(abs(min(surv_d_models_plot$delta_AICc_vs_null, na.rm = TRUE)),
              abs(max(surv_d_models_plot$delta_AICc_vs_null, na.rm = TRUE))))

ggplot(filter(surv_d_models_plot, var != "null"),
       aes(y = var, x = scenario, fill = delta_AICc_vs_null)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = expression(paste(Delta, "AICc relative to Null Model")),
                       trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  # facet_wrap(age_class ~ site, nrow = 3, drop = TRUE, scales = "free") +
  facet_grid(age_class ~ site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "\nDrought Variable", x = "Age Class\n")

lim <-  max(c(abs(min(surv_d_models_plot$D, na.rm = TRUE)),
              abs(max(surv_d_models_plot$D, na.rm = TRUE))))

ggplot(filter(surv_d_models_plot, var != "null"),
       aes(x = var, y = scenario, fill = D)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = c("#FFFFFF", brewer.pal(9, "Reds")),
                       name = "Proportional Reduction\n in Deviance",
                       limits = c(0, lim)) +
  facet_wrap(age_class ~ site, nrow = 3, drop = TRUE, scales = "free") +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "\nDrought Variable", y = "Age Class\n")

rm(list = c("d_models", "d_models_sel", "d_mod_sel", "temp",
            "m_table", "vars", "scenarios", "deviance", "temp1", "d"))



# ---- Survival_Best_Models -----------------------------------------------

rm(mod_sel)

g_best <- g_best %>%
  gather(coefficient, estimate, lag0, lag1, lag2) %>%
  filter(!is.na(estimate)) %>%
  select(-scenario) %>%
  rename(scenario = coefficient) %>%
  unite(ind_var, ind, var) %>%
  rename(var = ind_var) %>%
  mutate(scale = "global")

l_best <- l_best %>%
  gather(coefficient, estimate, lag0, lag1, lag2) %>%
  filter(!is.na(estimate)) %>%
  select(-scenario) %>%
  rename(scenario = coefficient) %>%
  mutate(scale = "local")

d_best <- d_best %>%
  gather(coefficient, estimate, lag0, lag1, lag2) %>%
  filter(!is.na(estimate)) %>%
  select(-scenario) %>%
  rename(scenario = coefficient) %>%
  mutate(scale = "drought")

n_best <- n_best %>%
  mutate(scale = "null")


surv_models_combined <- bind_rows(g_best, l_best, d_best, n_best)

surv_models_combined <- surv_models_combined %>%
  select(-rank, -model_num, -delta, -weight, -model) %>%
  arrange(site, age_class, AICc)

null_aic <- surv_models_combined %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_AICc = AICc)

null_deviance <- surv_models_combined %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_deviance = deviance)

surv_models_combined <- surv_models_combined %>%
  inner_join(null_aic) %>%
  inner_join(null_deviance) %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))

surv_models_combined$site <- factor(surv_models_combined$site, levels = levels(mod_df$site))

ggplot(surv_models_combined, aes(x = scale, y = var, fill = delta_AICc_vs_null)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = rev(c("#FFFFFF", brewer.pal(9, "Reds"))),
                       name = expression(paste(Delta, "AICc relative to Null Model")),
                       # limits = c(-lim, lim),
                       trans = sqrt_sign_trans()) +
  facet_wrap(site ~ age_class, nrow = 7, drop = TRUE, scales = "free") +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "\nClimate Variable", y = "Lag Scenario\n")

ggplot(surv_models_combined, aes(y = scale, x = age_class, fill = delta_AICc_vs_null)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = rev(c("#FFFFFF", brewer.pal(9, "Reds"))),
                       trans = sqrt_sign_trans(),
                       name = expression(paste(Delta, "AICc relative to Null Model"))) +
  facet_grid(. ~ site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "\nScale of Climate Variable", x = "Age Class\n")


temp <- surv_models_combined
temp[temp$D < 0, ]$D <- 0

ggplot(temp, aes(y = scale, x = age_class, fill = D)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = c("#FFFFFF", brewer.pal(9, "Greens")),
                       trans = sqrt_sign_trans(),
                       name = expression(paste(Delta, "Proportional Reduction in Deviance"))) +
  facet_grid(. ~ site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "\nAge Class", y = "Scale of Climate Variable\n")





# ---- FERTILITY ----------------------------------------------------------

# Takes about 1 minute
f <- stage_specific_fertility(lh, fert)
summary(f)

climate_predictors2 <- bioclim_df %>%
  left_join(select(ann_div, site, year_of, shannon_rain)) %>%
  left_join(indclim_df) %>%
  left_join(speiclim_df)

# Make trials
fert_trials <- make_survivorship_trials(f)

fert_trials <- fert_trials %>%
  ungroup() %>%
  filter(year_of < 2015 & age_class == "adult") %>%
  select(site = Study.Id, year_of, age_class, fate)

temp <- climate_predictors2 %>%
  ungroup() %>%
  mutate_each(funs(scale), -site, -year_of) %>%
  group_by(site) %>%
  gather(var, lag0, -site, -year_of) %>%
  group_by(site, var) %>%
  arrange(year_of) %>%
  mutate(lag1 = lag(lag0),
         lag2 = lag(lag0, n = 2),
         lag3 = lag(lag0, n = 3))

temp$site <- factor(temp$site,
                    levels = c("rppn-fma", "amboseli", "kakamega",
                               "gombe", "karisoke",
                               "beza", "ssr"))

# Models
fert_mod_df <- fert_trials %>%
  left_join(temp) %>%
  filter(!is.na(lag0)) %>%
  arrange(site, var, year_of)

fert_mod_df$site <- mapvalues(fert_mod_df$site,
                              from = levels(fert_mod_df$site),
                              to = c("Muriqui", "Baboon", "Blue Monkey",
                                     "Chimpanzee", "Gorilla", "Sifaka",
                                     "Capuchin"))

fert_mod_df <- fert_mod_df %>%
  mutate(scale = ifelse(var %in% colnames(bioclim_df), "local",
                        ifelse(var %in% colnames(indclim_df), "global",
                               ifelse(var %in% colnames(speiclim_df), "spei",
                                      "local"))))




# ---- Fertility_Null_Models ----------------------------------------------

fert_n <- fert_mod_df %>%
  ungroup() %>%
  filter(scale == "local" & var == levels(fert_mod_df$var)[1]) %>%
  arrange(site, age_class, var, year_of)

fert_n$var <- "null"

fert_n_models <- fert_n %>%
  ungroup() %>%
  group_by(site, age_class, var) %>%
  do(mod_null = glmer(fate ~ 1 + (1 | year_of),
                      data = ., family = "binomial"))

fert_n_models_sel <- NULL
k <- 1
for (i in 1:nrow(fert_n_models)) {
  for (j in 4:ncol(fert_n_models)) {
    temp <- select(fert_n_models[i, ], 1:3, j)
    temp$scenario <- names(temp)[4]
    names(temp)[4] <- "model"
    fert_n_models_sel[[k]] <- temp
    k <- k + 1
  }
}

fert_n_models_sel <- bind_rows(fert_n_models_sel)

var_levels <- c(levels(factor(fert_n_models_sel$var)))
fert_n_models_sel$var <- factor(fert_n_models_sel$var, levels = var_levels)

fert_n_mod_sel <- fert_n_models_sel %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$model, extra = c("AIC")),
     vars = data.frame(var = .$var),
     scenarios = data.frame(scenario = .$scenario),
     deviance = data.frame(deviance = unlist(lapply(.$model, deviance))))



# Since MuMIn fails to get intercepts for null models
temp <- fert_n_models %>%
  group_by(site, age_class) %>%
  summarise(X.Intercept. = lapply(lapply(mod_null, summary), coef)[[1]][, "Estimate"]) %>%
  mutate(var = "null")

for (i in 1:nrow(fert_n_mod_sel)) {
  fert_n_mod_sel[i, ]$m_table[[1]][, 1] <- temp[i, ]$X.Intercept.
  names(fert_n_mod_sel[i, ]$m_table[[1]])[1] <- "X.Intercept."
}

temp <- NULL

for (i in 1:nrow(fert_n_mod_sel)) {
  m_table <- data.frame(fert_n_mod_sel[i, ]$m_table[[1]])
  m_table$num <- rownames(m_table)

  vars <- fert_n_mod_sel[i, ]$vars[[1]]
  vars$num <- rownames(vars)

  scenarios <- fert_n_mod_sel[i, ]$scenarios[[1]]
  scenarios$num <- rownames(scenarios)

  deviance <- fert_n_mod_sel[i, ]$deviance[[1]]
  deviance$num <- rownames(deviance)

  temp1 <- suppressMessages(inner_join(m_table, vars))
  temp1 <- suppressMessages(inner_join(temp1, scenarios))
  temp1 <- suppressMessages(inner_join(temp1, deviance))
  temp1$site <- fert_n_mod_sel[i, ]$site
  temp1$age_class <- fert_n_mod_sel[i, ]$age_class
  temp1$rank <- as.numeric(rownames(temp1))

  temp1 <- temp1 %>%
    select(site, age_class, var, scenario, deviance, rank, model_num = num, 1:11)

  temp[[i]] <- temp1

}

fert_n_models <- tbl_df(bind_rows(temp))

fert_n_models_sel$scenario <- factor(fert_n_models_sel$scenario)

fert_n_best <- inner_join(fert_n_models, fert_n_models_sel)

rm(list = c("fert_n_models", "fert_n_models_sel", "fert_n_mod_sel", "temp",
            "m_table", "vars", "scenarios", "deviance",
            "temp1", "fert_n"))




# ---- Fertility_Global_Variables -----------------------------------------

# Find best model for each global climate oscillation index
fert_g <- filter(fert_mod_df, scale == "global")
fert_g <- separate(fert_g, var, c("index", "var"), fixed("_v"))

res <- list()
counter <- 1

for (j in levels(factor(fert_g$site))) {

  current_site <- as.character(j)

  fert_g_models <- fert_g %>%
    filter(site == current_site) %>%
    ungroup() %>%
    group_by(index, age_class, var) %>%
    do(mod_0 = glmer(fate ~ lag0 + (1 | year_of),
                     data = ., family = "binomial"),
       mod_1 = glmer(fate ~ lag1 + (1 | year_of),
                     data = ., family = "binomial"),
       mod_2 = glmer(fate ~ lag2 + (1 | year_of),
                     data = ., family = "binomial"))


  fert_g_models_sel <- NULL
  k <- 1
  for (i in 1:nrow(fert_g_models)) {
    for (ii in 4:ncol(fert_g_models)) {
      temp <- select(fert_g_models[i, ], 1:3, ii)
      temp$scenario <- names(temp)[4]
      names(temp)[4] <- "model"
      fert_g_models_sel[[k]] <- temp
      k <- k + 1
    }
  }

  fert_g_models_sel <- bind_rows(fert_g_models_sel)

  var_levels <- c(levels(factor(fert_g_models_sel$var)))
  fert_g_models_sel$var <- factor(fert_g_models_sel$var, levels = var_levels)

  fert_g_mod_sel <- fert_g_models_sel %>%
    group_by(index, age_class) %>%
    do(m_table = model.sel(.$model, extra = c("AIC")),
       vars = data.frame(var = .$var),
       scenarios = data.frame(scenario = .$scenario),
       deviance = data.frame(deviance = unlist(lapply(.$model, deviance))))

  temp <- NULL

  for (i in 1:nrow(fert_g_mod_sel)) {
    m_table <- data.frame(fert_g_mod_sel[i, ]$m_table[[1]])
    m_table$num <- rownames(m_table)

    vars <- fert_g_mod_sel[i, ]$vars[[1]]
    vars$num <- rownames(vars)

    scenarios <- fert_g_mod_sel[i, ]$scenarios[[1]]
    scenarios$num <- rownames(scenarios)

    deviance <- fert_g_mod_sel[i, ]$deviance[[1]]
    deviance$num <- rownames(deviance)

    temp1 <- suppressMessages(inner_join(m_table, vars))
    temp1 <- suppressMessages(inner_join(temp1, scenarios))
    temp1 <- suppressMessages(inner_join(temp1, deviance))
    temp1$index <- fert_g_mod_sel[i, ]$index
    temp1$age_class <- fert_g_mod_sel[i, ]$age_class
    temp1$rank <- as.numeric(rownames(temp1))

    temp1 <- temp1 %>%
      select(index, age_class, var, scenario, deviance, rank, model_num = num, 1:10)

    temp[[i]] <- temp1

  }

  g_fert_models <- tbl_df(bind_rows(temp))

  fert_g_best <- filter(g_fert_models, rank == 1)

  temp <- suppressMessages(inner_join(fert_g_models_sel,
                                      select(fert_g_best, index, age_class, var, scenario)))

  temp$site <- current_site

  res[[counter]] <- temp

  counter <- counter + 1
}

fert_g <- bind_rows(res)

# Given this set of "best global models" (one for each site/age class/index),
# find the one that is best for each site and age class
temp <- fert_g %>%
  ungroup() %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$model, extra = c("AIC")),
     vars = data.frame(var = .$var),
     inds = data.frame(ind = .$index),
     scenarios = data.frame(scenario = .$scenario),
     deviance = data.frame(deviance = unlist(lapply(.$model, deviance))))

fert_g_best <- list()

for (i in 1:nrow(temp)) {
  m_table <- data.frame(temp[i, ]$m_table[[1]])
  m_table$num <- rownames(m_table)

  inds <- temp[i, ]$inds[[1]]
  inds$num <- rownames(inds)

  vars <- temp[i, ]$vars[[1]]
  vars$num <- rownames(vars)

  scenarios <- temp[i, ]$scenarios[[1]]
  scenarios$num <- rownames(scenarios)

  deviance <- temp[i, ]$deviance[[1]]
  deviance$num <- rownames(deviance)

  temp1 <- suppressMessages(inner_join(m_table, inds))
  temp1 <- suppressMessages(inner_join(temp1, vars))
  temp1 <- suppressMessages(inner_join(temp1, scenarios))
  temp1 <- suppressMessages(inner_join(temp1, deviance))
  temp1$site <- temp[i, ]$site
  temp1$age_class <- temp[i, ]$age_class
  temp1$rank <- as.numeric(rownames(temp1))

  temp1 <- temp1 %>%
    select(site, age_class, ind, var, scenario, deviance, rank, model_num = num, 1:11)

  fert_g_best[[i]] <- temp1

}

fert_g_rank <- bind_rows(fert_g_best)
fert_g_best <- filter(fert_g_rank, rank == 1)

# Obtain final set
fert_g <- rename(fert_g, ind = index)
fert_g$ind <- factor(fert_g$ind, levels = levels(factor(fert_g$ind)))
fert_g_best <- inner_join(fert_g_best, fert_g)
fert_g_rank <- inner_join(fert_g_rank, fert_g)

# Plot
fert_g_models_plot <- bind_rows(fert_g_rank, fert_n_best)

fert_g_best$se <- ldply(fert_g_best$model, .fun = function(x) coef(summary(x))[2, "Std. Error"])$V1
# temp <- ldply(fert_g_best$model,
#               .fun = function(x) confint(x, method = "boot",
#                                          nsim = 100, quiet = TRUE)[3, ])
# fert_g_best$ci_lower <- temp[, 1]
# fert_g_best$ci_upper <- temp[, 2]

fert_g_models_plot <- fert_g_models_plot %>%
  select(-rank, -model_num, -delta, -weight, -model) %>%
  arrange(site, age_class, AICc)

null_aic <- fert_g_models_plot %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_AICc = AICc)

null_deviance <- fert_g_models_plot %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_deviance = deviance)

fert_g_models_plot <- fert_g_models_plot %>%
  inner_join(null_aic) %>%
  inner_join(null_deviance) %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))

lim <-  max(c(abs(min(fert_g_models_plot$delta_AICc_vs_null, na.rm = TRUE)),
              abs(max(fert_g_models_plot$delta_AICc_vs_null, na.rm = TRUE))))

ggplot(filter(fert_g_models_plot, var != "null"),
       aes(x = ind, y = age_class, fill = delta_AICc_vs_null)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = expression(paste(Delta, "AICc relative to Null Model")),
                       limits = c(-lim, lim)) +
  facet_grid(. ~ site, drop = TRUE, scales = "free") +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "\nClimate Oscillation Index", y = "Age Class\n")

rm(list = c("fert_g_models", "fert_g_models_sel", "fert_g_mod_sel", "temp",
            "g_fert_models", "m_table", "inds", "vars", "scenarios", "deviance",
            "temp1", "fert_g"))






# ---- Fertility_Local_Variables ------------------------------------------

# Find best model for each variable
fert_l <- fert_mod_df %>%
  ungroup() %>%
  filter(scale == "local") %>%
  arrange(site, age_class, var, year_of)

fert_l_models <- fert_l %>%
  ungroup() %>%
  group_by(site, age_class, var) %>%
  do(mod_0 = glmer(fate ~ lag0 + (1 | year_of),
                   data = ., family = "binomial"),
     mod_1 = glmer(fate ~ lag1 + (1 | year_of),
                   data = ., family = "binomial"),
     mod_2 = glmer(fate ~ lag2 + (1 | year_of),
                   data = ., family = "binomial"))


fert_l_models_sel <- NULL
k <- 1
for (i in 1:nrow(fert_l_models)) {
  for (j in 4:ncol(fert_l_models)) {
    temp <- select(fert_l_models[i, ], 1:3, j)
    temp$scenario <- names(temp)[4]
    names(temp)[4] <- "model"
    fert_l_models_sel[[k]] <- temp
    k <- k + 1
  }
}

fert_l_models_sel <- bind_rows(fert_l_models_sel)

var_levels <- c(levels(factor(fert_l_models_sel$var)))
fert_l_models_sel$var <- factor(fert_l_models_sel$var, levels = var_levels)

fert_l_mod_sel <- fert_l_models_sel %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$model, extra = c("AIC")),
     vars = data.frame(var = .$var),
     scenarios = data.frame(scenario = .$scenario),
     deviance = data.frame(deviance = unlist(lapply(.$model, deviance))))

temp <- NULL

for (i in 1:nrow(fert_l_mod_sel)) {
  m_table <- data.frame(fert_l_mod_sel[i, ]$m_table[[1]])
  m_table$num <- rownames(m_table)

  vars <- fert_l_mod_sel[i, ]$vars[[1]]
  vars$num <- rownames(vars)

  scenarios <- fert_l_mod_sel[i, ]$scenarios[[1]]
  scenarios$num <- rownames(scenarios)

  deviance <- fert_l_mod_sel[i, ]$deviance[[1]]
  deviance$num <- rownames(deviance)

  temp1 <- suppressMessages(inner_join(m_table, vars))
  temp1 <- suppressMessages(inner_join(temp1, scenarios))
  temp1 <- suppressMessages(inner_join(temp1, deviance))
  temp1$site <- fert_l_mod_sel[i, ]$site
  temp1$age_class <- fert_l_mod_sel[i, ]$age_class
  temp1$rank <- as.numeric(rownames(temp1))

  temp1 <- temp1 %>%
    select(site, age_class, var, scenario, deviance, rank, model_num = num, 1:11)

  temp[[i]] <- temp1

}

fert_l_models <- tbl_df(bind_rows(temp))

# Get final set
fert_l_best <- filter(fert_l_models, rank == 1)

fert_l_models_sel$scenario <- factor(fert_l_models_sel$scenario, levels = levels(fert_l_best$scenario))
fert_l_best <- inner_join(fert_l_best, fert_l_models_sel)

fert_l_best$se <- ldply(fert_l_best$model, .fun = function(x) coef(summary(x))[2, "Std. Error"])$V1
# temp <- ldply(fert_l_best$model,
#               .fun = function(x) confint(x, method = "boot",
#                                          nsim = 100, quiet = TRUE)[3, ])
# fert_l_best$ci_lower <- temp[, 1]
# fert_l_best$ci_upper <- temp[, 2]

fert_l_models_plot <- bind_rows(fert_l_models, fert_n_best)

fert_l_models_plot <- fert_l_models_plot %>%
  select(-rank, -model_num, -delta, -weight, -model) %>%
  arrange(site, age_class, AICc)

null_aic <- fert_l_models_plot %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_AICc = AICc)

null_deviance <- fert_l_models_plot %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_deviance = deviance)

fert_l_models_plot <- fert_l_models_plot %>%
  inner_join(null_aic) %>%
  inner_join(null_deviance) %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))

lim <-  max(c(abs(min(fert_l_models_plot$delta_AICc_vs_null, na.rm = TRUE)),
              abs(max(fert_l_models_plot$delta_AICc_vs_null, na.rm = TRUE))))

ggplot(filter(fert_l_models_plot, var != "null"),
       aes(y = var, x = scenario, fill = delta_AICc_vs_null)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = expression(paste(Delta, "AICc relative to Null Model")),
                       # trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  facet_grid(age_class ~ site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "\nLocal Climate Variable", x = "Age Class\n")

rm(list = c("fert_l_models_sel", "fert_l_mod_sel", "temp",
            "m_table", "vars", "scenarios", "deviance", "temp1", "fert_l"))





# ---- Fertility_Drought_Variables ----------------------------------------

# Find best model for each variable
fert_d <- fert_mod_df %>%
  ungroup() %>%
  filter(scale == "spei") %>%
  arrange(site, age_class, var, year_of)

fert_d_models <- fert_d %>%
  ungroup() %>%
  group_by(site, age_class, var) %>%
  do(mod_0 = glmer(fate ~ lag0 + (1 | year_of),
                   data = ., family = "binomial"),
     mod_1 = glmer(fate ~ lag1 + (1 | year_of),
                   data = ., family = "binomial"),
     mod_2 = glmer(fate ~ lag2 + (1 | year_of),
                   data = ., family = "binomial"))


fert_d_models_sel <- NULL
k <- 1
for (i in 1:nrow(fert_d_models)) {
  for (j in 4:ncol(fert_d_models)) {
    temp <- select(fert_d_models[i, ], 1:3, j)
    temp$scenario <- names(temp)[4]
    names(temp)[4] <- "model"
    fert_d_models_sel[[k]] <- temp
    k <- k + 1
  }
}

fert_d_models_sel <- bind_rows(fert_d_models_sel)

var_levels <- c(levels(factor(fert_d_models_sel$var)))
fert_d_models_sel$var <- factor(fert_d_models_sel$var, levels = var_levels)

fert_d_mod_sel <- fert_d_models_sel %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$model, extra = c("AIC")),
     vars = data.frame(var = .$var),
     scenarios = data.frame(scenario = .$scenario),
     deviance = data.frame(deviance = unlist(lapply(.$model, deviance))))

temp <- NULL

for (i in 1:nrow(fert_d_mod_sel)) {
  m_table <- data.frame(fert_d_mod_sel[i, ]$m_table[[1]])
  m_table$num <- rownames(m_table)

  vars <- fert_d_mod_sel[i, ]$vars[[1]]
  vars$num <- rownames(vars)

  scenarios <- fert_d_mod_sel[i, ]$scenarios[[1]]
  scenarios$num <- rownames(scenarios)

  deviance <- fert_d_mod_sel[i, ]$deviance[[1]]
  deviance$num <- rownames(deviance)

  temp1 <- suppressMessages(inner_join(m_table, vars))
  temp1 <- suppressMessages(inner_join(temp1, scenarios))
  temp1 <- suppressMessages(inner_join(temp1, deviance))
  temp1$site <- fert_d_mod_sel[i, ]$site
  temp1$age_class <- fert_d_mod_sel[i, ]$age_class
  temp1$rank <- as.numeric(rownames(temp1))

  temp1 <- temp1 %>%
    select(site, age_class, var, scenario, deviance, rank, model_num = num, 1:11)

  temp[[i]] <- temp1

}

fert_d_models <- tbl_df(bind_rows(temp))

# Get final set
fert_d_best <- filter(fert_d_models, rank == 1)

fert_d_models_sel$scenario <- factor(fert_d_models_sel$scenario, levels = levels(fert_d_best$scenario))
fert_d_best <- inner_join(fert_d_best, fert_d_models_sel)

fert_d_best$se <- ldply(fert_d_best$model, .fun = function(x) coef(summary(x))[2, "Std. Error"])$V1
# temp <- ldply(fert_d_best$model,
#               .fun = function(x) confint(x, method = "boot",
#                                          nsim = 100, quiet = TRUE)[3, ])
# fert_d_best$ci_lower <- temp[, 1]
# fert_d_best$ci_upper <- temp[, 2]

fert_d_models_plot <- bind_rows(fert_d_models, fert_n_best)

fert_d_models_plot <- fert_d_models_plot %>%
  select(-rank, -model_num, -delta, -weight, -model) %>%
  arrange(site, age_class, AICc)

null_aic <- fert_d_models_plot %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_AICc = AICc)

null_deviance <- fert_d_models_plot %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_deviance = deviance)

fert_d_models_plot <- fert_d_models_plot %>%
  inner_join(null_aic) %>%
  inner_join(null_deviance) %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))


lim <-  max(c(abs(min(fert_d_models_plot$delta_AICc_vs_null, na.rm = TRUE)),
              abs(max(fert_d_models_plot$delta_AICc_vs_null, na.rm = TRUE))))

ggplot(filter(fert_d_models_plot, var != "null"),
       aes(y = var, x = scenario, fill = delta_AICc_vs_null)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       name = expression(paste(Delta, "AICc relative to Null Model")),
                       limits = c(-lim, lim)) +
  facet_grid(age_class ~ site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "\nDrought Variable", x = "Lag Scenario\n")

lim <-  max(c(abs(min(fert_d_models_plot$D, na.rm = TRUE)),
              abs(max(fert_d_models_plot$D, na.rm = TRUE))))

ggplot(filter(fert_d_models_plot, var != "null"),
       aes(y = var, x = scenario, fill = D)) +
  geom_tile(size = 0.1, color = "black") +
  # scale_fill_gradientn(colours = c("#FFFFFF", brewer.pal(9, "Reds")),
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdGy")),
                       name = "Proportional Reduction\n in Deviance",
                       trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  facet_grid(age_class ~ site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "\nDrought Variable", x = "Lag Scenario\n")

rm(list = c("fert_d_models_sel", "fert_d_mod_sel", "temp",
            "m_table", "vars", "scenarios", "deviance", "temp1", "fert_d"))





# ---- Fertility_Best_models ----------------------------------------------

rm(fert_mod_sel)

fert_g_best <- fert_g_best %>%
  gather(coefficient, estimate, lag0, lag1, lag2) %>%
  filter(!is.na(estimate)) %>%
  select(-scenario) %>%
  rename(scenario = coefficient) %>%
  unite(ind_var, ind, var) %>%
  rename(var = ind_var) %>%
  mutate(scale = "global")

fert_l_best <- fert_l_best %>%
  gather(coefficient, estimate, lag0, lag1, lag2) %>%
  filter(!is.na(estimate)) %>%
  select(-scenario) %>%
  rename(scenario = coefficient) %>%
  mutate(scale = "local")

fert_d_best <- fert_d_best %>%
  gather(coefficient, estimate, lag0, lag1, lag2) %>%
  filter(!is.na(estimate)) %>%
  select(-scenario) %>%
  rename(scenario = coefficient) %>%
  mutate(scale = "drought")

fert_n_best <- fert_n_best %>%
  mutate(scale = "null")


fert_models_combined <- bind_rows(fert_g_best, fert_l_best, fert_d_best, fert_n_best)

fert_models_combined <- fert_models_combined %>%
  select(-rank, -model_num, -delta, -weight, -model) %>%
  arrange(site, age_class, AICc)

null_aic <- fert_models_combined %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_AICc = AICc)

null_deviance <- fert_models_combined %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_deviance = deviance)

fert_models_combined <- fert_models_combined %>%
  inner_join(null_aic) %>%
  inner_join(null_deviance) %>%
  mutate(delta_AICc_vs_null = AICc - null_AICc,
         D = 1 - (deviance / null_deviance))

fert_models_combined$site <- factor(fert_models_combined$site, levels = levels(mod_df$site))

ggplot(fert_models_combined, aes(x = scale, y = var, fill = delta_AICc_vs_null)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = rev(c("#FFFFFF", brewer.pal(9, "Reds"))),
                       name = expression(paste(Delta, "AICc relative to Null Model")),
                       # limits = c(-lim, lim),
                       trans = sqrt_sign_trans()) +
  facet_wrap(site ~ age_class, nrow = 7, drop = TRUE, scales = "free") +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "\nClimate Variable", y = "Lag Scenario\n")

ggplot(fert_models_combined, aes(x = age_class, y = scale, fill = delta_AICc_vs_null)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = rev(c("#FFFFFF", brewer.pal(9, "Reds"))),
                       trans = sqrt_sign_trans(),
                       name = expression(paste(Delta, "AICc relative to Null Model"))) +
  facet_grid(. ~ site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "\nScale of Climate Variable", x = "Lag Scenario\n")

ggplot(fert_models_combined, aes(y = scale, x = site, fill = D)) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = c("#FFFFFF", brewer.pal(9, "Reds")),
                       trans = sqrt_sign_trans(),
                       name = expression(paste(Delta, "AICc relative to Null Model"))) +
  # facet_grid(~site) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "\nScale of Climate Variable", x = "Age Class\n")



# ---- coefficient_plots --------------------------------------------------

fert_models_combined$new_var <- paste(toupper(fert_models_combined$scale),
                                      toupper(fert_models_combined$scenario),
                                      fert_models_combined$var, sep = " - ")

ggplot(filter(fert_models_combined, var != "null"),
       aes(x = new_var, y = estimate, color = delta_AICc_vs_null)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - se, ymax = estimate + se),
                 height = 0.3, size = 0.75) +
  scale_color_gradientn(colours = rev(c(brewer.pal(5, "Reds"))),
                        name = expression(paste(Delta, "AICc relative to Null Model")),
                        trans = sqrt_sign_trans()) +
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw() +
  facet_grid(. ~ site, scales = "free_x", drop = TRUE) +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Scaled Coefficient Estimate\n", x = "\nVariable",
       title = "Scaled Coefficient Estimates for Best Models of Adult Female Fertility\n")

temp <- fert_models_combined
temp[temp$D < 0, ]$D <- 0

ggplot(filter(temp, var != "null"),
       aes(x = new_var, y = estimate, color = D)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - se, ymax = estimate + se),
                width = 0.25, size = 0.75) +
  scale_color_gradientn(colours = brewer.pal(7, "Greens")[2:7],
                        name = "Proportional Reduction in Deviance") +
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw() +
  facet_grid(. ~ site, scales = "free_x", drop = TRUE) +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Scaled Coefficient Estimate\n", x = "\nVariable",
       title = "Scaled Coefficient Estimates for Best Models of Adult Female Fertility\n")