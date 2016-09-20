# Nino
var_names <- by_qua_f %>%
  unnest(ind_model %>%
           purrr::map(., ~car::Anova(., type = "3") %>%
                        row.names(.)))
var_names <- c(var_names[, 2][[1]])

aod <- by_qua_f %>%
  unnest(ind_model %>%
           purrr::map(., ~car::Anova(., type = "3")))

aod$term <- var_names
aod <- filter(aod, str_detect(term, ":"))
aod$model <- "index_nino3.4"
aod_ind <- select(aod, quarter, model, term, everything())


# Rainfall
var_names <- by_qua_f %>%
  unnest(rainfall_model %>%
           purrr::map(., ~car::Anova(., type = "3") %>%
                        row.names(.)))
var_names <- c(var_names[, 2][[1]])

aod <- by_qua_f %>%
  unnest(rainfall_model %>%
           purrr::map(., ~car::Anova(., type = "3")))

aod$term <- var_names
aod <- filter(aod, str_detect(term, ":"))
aod$model <- "rainfall"
aod_rainfall <- select(aod, quarter, model, term, everything())


# Temperature
var_names <- by_qua_f %>%
  unnest(temp_model %>%
           purrr::map(., ~car::Anova(., type = "3") %>%
                        row.names(.)))
var_names <- c(var_names[, 2][[1]])

aod <- by_qua_f %>%
  unnest(temp_model %>%
           purrr::map(., ~car::Anova(., type = "3")))

aod$term <- var_names
aod <- filter(aod, str_detect(term, ":"))
aod$model <- "mean_temp"
aod_temp <- select(aod, quarter, model, term, everything())

aod <- bind_rows(aod_ind, aod_rainfall, aod_temp)
names(aod)[6] <- "P-value"


# R-squared
temp <- by_qua_f %>%
  mutate(index_nino3.4 = purrr::map(ind_model, MuMIn::r.squaredGLMM),
         rainfall = purrr::map(rainfall_model, MuMIn::r.squaredGLMM),
         mean_temp = purrr::map(temp_model, MuMIn::r.squaredGLMM)) %>%
  select(quarter, -contains("model"), -data, -trials)

temp1 <- unnest(temp)
temp1$term <- rep(names(temp$rainfall[[1]]), nrow(temp))
temp1 <- gather(temp1, model, value, -quarter, -term)
temp1 <- spread(temp1, term, value)


aod <- inner_join(aod, temp1)
