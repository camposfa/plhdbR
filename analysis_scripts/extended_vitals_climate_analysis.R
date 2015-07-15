
# ---- survival -----------------------------------------------------------

# Takes about 1 minute
m <- stage_specific_survival(lh)
summary(m)

# Make trials
surv_trials <- make_survivorship_trials(m)
surv_trials <- filter(surv_trials, year_of < 2015)

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
  rename(site = Study.Id) %>%
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


# BEST GLOBAL MODEL
# Find best model for each global climate oscillation index
g <- filter(mod_df, scale == "global")
g <- separate(g, var, c("index", "var"), 4)
g$index <- substr(g$index, 1, 3)

res <- list()
c <- 1

for (j in levels(factor(g$index))) {

  current_ind <- as.character(j)

  g_models <- g %>%
    filter(index == current_ind) %>%
    ungroup() %>%
    group_by(site, age_class, var) %>%
    do(mod_0 = glmer(fate ~ lag0 + (1 | year_of), data = ., family = "binomial"),
       mod_1 = glmer(fate ~ lag1 + (1 | year_of), data = ., family = "binomial"),
       mod_2 = glmer(fate ~ lag2 + (1 | year_of), data = ., family = "binomial"))


  g_models_sel <- NULL
  k <- 1
  for (i in 1:nrow(g_models)) {
    for (j in 4:ncol(g_models)) {
      temp <- select(g_models[i, ], 1:3, j)
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
    group_by(site, age_class) %>%
    do(m_table = model.sel(.$model, extra = c("DIC", "AIC")),
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
    temp1$site <- g_mod_sel[i, ]$site
    temp1$age_class <- g_mod_sel[i, ]$age_class
    temp1$rank <- as.numeric(rownames(temp1))

    temp1 <- temp1 %>%
      select(site, age_class, var, scenario, deviance, rank, model_num = num, 1:10)

    temp[[i]] <- temp1

  }

  g_surv_models <- tbl_df(bind_rows(temp))

  g_best <- filter(g_surv_models, rank == 1)

  temp <- suppressMessages(inner_join(g_models_sel,
                                      select(g_best, site, age_class, var, scenario)))

  temp$index <- current_ind

  res[[c]] <- temp

  c <- c + 1
}

g <- bind_rows(res)

# Given this set of "best global models" (one for each site/age class/index),
# find the one that is best for each site and age class
temp <- g %>%
  ungroup() %>%
  group_by(site, age_class) %>%
  do(m_table = model.sel(.$model, extra = c("DIC", "AIC")),
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

g_best <- bind_rows(g_best)
g_best <- filter(g_best, rank == 1)

# Obtain final set
g <- rename(g, ind = index)
g$ind <- factor(g$ind, levels = levels(g_best$ind))
g_best <- inner_join(g_best, g)

rm(list = c("g_models", "g_models_sel", "g_mod_sel", "temp", "g_surv_models",
        "m_table", "inds", "vars", "scenarios", "deviance", "temp1", "g"))


# BEST LOCAL MODEL
# Find best model for each variable
l <- mod_df %>%
  ungroup() %>%
  filter(scale == "local") %>%
  arrange(site, age_class, var, year_of)

l_models <- l %>%
  ungroup() %>%
  group_by(site, age_class, var) %>%
  do(mod_0 = glmer(fate ~ lag0 + (1 | year_of), data = ., family = "binomial"),
     mod_1 = glmer(fate ~ lag1 + (1 | year_of), data = ., family = "binomial"),
     mod_2 = glmer(fate ~ lag2 + (1 | year_of), data = ., family = "binomial"))


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
  do(m_table = model.sel(.$model, extra = c("DIC", "AIC")),
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

rm(list = c("l_models", "l_models_sel", "l_mod_sel", "temp", "l_surv_models",
            "m_table", "vars", "scenarios", "deviance", "temp1", "l"))


# BEST DROUGHT MODEL
# Find best model for each variable
d <- mod_df %>%
  ungroup() %>%
  filter(scale == "spei") %>%
  arrange(site, age_class, var, year_of)

d_models <- d %>%
  ungroup() %>%
  group_by(site, age_class, var) %>%
  do(mod_0 = glmer(fate ~ lag0 + (1 | year_of), data = ., family = "binomial"),
     mod_1 = glmer(fate ~ lag1 + (1 | year_of), data = ., family = "binomial"),
     mod_2 = glmer(fate ~ lag2 + (1 | year_of), data = ., family = "binomial"))


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
  do(m_table = model.sel(.$model, extra = c("DIC", "AIC")),
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
    select(site, age_class, var, scenario, deviance, rank, moded_num = num, 1:11)

  temp[[i]] <- temp1

}

d_surv_models <- tbl_df(bind_rows(temp))

# Get final set
d_best <- filter(d_surv_models, rank == 1)

d_models_sel$scenario <- factor(d_models_sel$scenario, levels = levels(d_best$scenario))
d_best <- inner_join(d_best, d_models_sel)

rm(list = c("d_models", "d_models_sel", "d_mod_sel", "temp", "d_surv_models",
            "m_table", "vars", "scenarios", "deviance", "temp1", "d"))




# NULL MODELS
n <- mod_df %>%
  ungroup() %>%
  filter(scale == "local" & var == levels(mod_df$var)[1]) %>%
  arrange(site, age_class, var, year_of)

n$var <- "null"

n_models <- n %>%
  ungroup() %>%
  group_by(site, age_class, var) %>%
  do(mod_null = glmer(fate ~ 1 + (1 | year_of), data = ., family = "binomial"))

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
  do(m_table = model.sel(.$model, extra = c("DIC", "AIC")),
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






# ---- final_model_selection ----------------------------------------------

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


temp <- bind_rows(g_best, l_best, d_best, n_best)

temp <- temp %>%
  select(-rank, -model_num, -delta, -weight, -model) %>%
  arrange(site, age_class, AICc)

null_aic <- temp %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_AICc = AICc)

null_deviance <- temp %>%
  group_by(site, age_class) %>%
  filter(var == "null") %>%
  select(null_deviance = deviance)

temp <- temp %>%
  inner_join(null_aic) %>%
  inner_join(null_deviance)



ggplot(temp, aes(x = scale, y = var, fill = (AICc - null_AICc))) +
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

ggplot(temp, aes(x = scale, y = age_class, fill = (AICc - null_AICc))) +
  geom_tile(size = 0.1, color = "black") +
  scale_fill_gradientn(colours = rev(c("#FFFFFF", brewer.pal(9, "Reds"))),
                       name = expression(paste(Delta, "AICc relative to Null Model")),
                       # limits = c(-lim, lim),
                       trans = sqrt_sign_trans()) +
  facet_wrap(~site, nrow = 7, drop = TRUE, scales = "free") +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "\nClimate Variable", y = "Lag Scenario\n")


