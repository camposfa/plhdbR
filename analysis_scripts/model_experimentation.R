temp <- by_qua_ac[9, c(1:3)] %>% unnest()
temp$Study.Id  <- temp$site
temp1 <- inner_join(select(temp, quarter, age_class, site, year_of, index_nino3.4, mean_temp, rainfall, Study.Id),
                           make_survivorship_trials(temp))

temp2 <- temp1 %>%
  select(-Study.Id) %>%
  group_by(quarter, age_class) %>%
  nest()

# x * site + (x | site:block)
# fixed effect variation of slope and intercept varying among sites
# and random variation of slope and intercept among years within sites
temp3 <- temp2 %>%
  mutate(null_model = purrr::map(data, ~ glmer(fate ~ site + (1 | site/year_of),
                                             family = "binomial", data = .)),
         ind_model = purrr::map(data, ~ glmer(fate ~ site * index_nino3.4 + (index_nino3.4 | site/year_of),
                                            family = "binomial", data = .)),
         temp_model = purrr::map(data, ~ glmer(fate ~ site * mean_temp + (mean_temp | site/year_of),
                                             family = "binomial", data = .)),
         rainfall_model = purrr::map(data, ~ glmer(fate ~ site * rainfall + (rainfall | site/year_of),
                                                 family = "binomial", data = .)))

sjp.setTheme(base = theme_minimal())

sjp.glmer(temp3$rainfall_model[[1]], type = "fe", showIntercept = FALSE)
sjp.glmer(temp3$rainfall_model[[1]], type = "ri.pc", facet.grid = FALSE)
sjp.glmer(temp3$rainfall_model[[1]], type = "rs.ri", facet.grid = FALSE)
sjp.glmer(temp3$rainfall_model[[1]], type = "eff", facet.grid = FALSE)
sjp.glmer(temp3$rainfall_model[[1]], type = "y.pc", facet.grid = FALSE)
sjp.glmer(temp3$rainfall_model[[1]], type = "ma")

temp3 %>%
  mutate(null_AICc = purrr::map_dbl(null_model, MuMIn::AICc),
         index_nino3.4 = purrr::map_dbl(ind_model, MuMIn::AICc),
         mean_temp = purrr::map_dbl(temp_model, MuMIn::AICc),
         rainfall = purrr::map_dbl(rainfall_model, MuMIn::AICc)) %>%
  select(-matches("model"), -data) %>%
  gather(var, AICc, -quarter, -age_class, -null_AICc) %>%
  mutate(delta = AICc - null_AICc)





