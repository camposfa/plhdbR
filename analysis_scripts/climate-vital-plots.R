# Survival Plots

# Fertility rate plots

# Single quarter (var ~ site)
ggplot(filter(full_f_df, quarter == "wettest_q" & var != "spei_03"),
       aes(x = value, y = successes/trials, color = site, fill = site)) +
  geom_point() +
  facet_wrap(~var + site, labeller = global_labeller, scales = "free_x", ncol = 7) +
  theme_fc() +
  stat_smooth(method = "lm") +
  scale_color_brewer(palette = "Dark2", guide = FALSE) +
  scale_fill_brewer(palette = "Dark2", guide = FALSE) +
  labs(x = "Scaled climate predictor", y = "Fertility Rate")

# Single climate predictor (quarter ~ site)
ggplot(filter(full_f_df, var == "rainfall"),
       aes(x = value, y = successes/trials, color = site, fill = site)) +
  geom_point() +
  facet_grid(quarter ~ site, labeller = global_labeller, scales = "free_x") +
  theme_fc() +
  stat_smooth(method = "lm") +
  scale_color_brewer(palette = "Dark2", guide = FALSE) +
  scale_fill_brewer(palette = "Dark2", guide = FALSE) +
  labs(x = "Scaled climate predictor", y = "Fertility Rate",
       title = "Fertility rates vs. Rainfall")

# Single site (quarter ~ var)
ggplot(filter(full_f_df, site == "ssr"),
       aes(x = value, y = successes/trials, color = quarter, fill = quarter)) +
  geom_point() +
  facet_grid(quarter ~ var, labeller = global_labeller, scales = "free_x", switch = "y") +
  theme_fc() +
  stat_smooth(method = "lm") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Scaled climate predictor", y = "Fertility Rate")

ggplot(full_f_df,
       aes(x = value, y = successes/trials, color = site, fill = site)) +
  geom_point(size = 1, alpha = 0.25, shape = 3) +
  facet_grid(quarter ~ var, labeller = global_labeller, scales = "free_x", switch = "y") +
  theme_fc() +
  stat_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Scaled climate predictor", y = "Fertility Rate")



# Vital rates by year measured in different quarters

ggplot(filter(full_df, var == "rainfall"),
       aes(x = year_of, y = surv, fill = age_class)) +
  geom_line(aes(color = age_class), size = 1) +
  # geom_point(shape = 21, colour = "black",
  #            size = 2, stroke = 0.2, alpha = 0.75) +
  facet_grid(quarter ~ site, labeller = global_labeller) +
  scale_fill_brewer(palette = "Set1", name = "Quarter") +
  scale_color_brewer(palette = "Set1", name = "Quarter") +
  theme_bw() +
  labs(x = "\nYear", y = "Modeled Survival\n",
       title = "Survival Rates") +
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(0.5, "cm"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(1960, 2015)) +
  scale_y_continuous(trans = log1p_trans())



# ---- Modeled ------------------------------------------------------------

temp <- by_site_qua_var_ac %>%
  filter(var == "rainfall") %>%
  mutate(fitted = purrr::map(null_model, ~ broom::augment(.) %>%
                               tbl_df()))

temp1 <- temp %>%
  select(site, quarter, age_class, fitted) %>%
  unnest() %>%
  group_by(site, quarter, age_class, year_of) %>%
  summarise(surv = mean(.mu))

temp1$year_of <- as.numeric(temp1$year_of)

full_df <- left_join(full_df, temp1)

ggplot(filter(full_df, age_class == "newborn" & site == "beza"),
       aes(x = value, y = surv, color = var, fill = var)) +
  geom_point() +
  facet_grid(quarter ~ var, labeller = global_labeller, scales = "free_x") +
  theme_fc() +
  stat_smooth(method = "lm") +
  scale_color_brewer(palette = "Dark2", guide = FALSE) +
  scale_fill_brewer(palette = "Dark2", guide = FALSE) +
  labs(x = "Scaled climate predictor", y = "Survival Rate",
       title = "Fertility rates vs. Rainfall")

p1 <- ggplot(filter(full_df, var == "rainfall" & !is.na(surv)),
       aes(x = year_of, y =  surv, color = age_class)) +
  geom_line(size = 0.5) +
  facet_grid(quarter ~ site, labeller = global_labeller) +
  scale_color_manual(values = brewer.pal(3, "Set1")[c(2, 3, 1)],
                     name = "Age Class",
                     labels = c("Adults", "Juveniles", "Infants")) +
  # scale_color_brewer(palette = "Set1", name = "Age Class",
  #                    labels = c("Adults", "Juveniles", "Newborns")) +
  labs(x = "\nYear", y = "Modeled Survival\n",
       title = "Survival Rates") +
  theme_fc() +
  theme(panel.grid.major.y = element_line(colour = "gray95"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_line(colour = "black"),
        legend.key.width = unit(0.5, "cm")) +
  scale_x_continuous(limits = c(1960, 2015)) +
  scale_y_continuous(trans = exp_trans(),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2))

# Fert
temp <- by_site_qua_var_f %>%
  filter(var == "rainfall") %>%
  mutate(fitted = purrr::map(null_model, ~ broom::augment(.) %>%
                               tbl_df()))

temp1 <- temp %>%
  select(site, quarter, fitted) %>%
  unnest() %>%
  group_by(site, quarter, year_of) %>%
  summarise(fert = mean(.mu))

temp1$year_of <- as.numeric(temp1$year_of)

full_f_df <- left_join(full_f_df, temp1)

temp <- full_f_df %>%
  filter(var == "rainfall" & !(site == "kakamega" & year_of < 1997))

p2 <- ggplot(temp,
       aes(x = year_of, y = fert, color = age_class)) +
  geom_line(size = 0.5) +
  facet_grid(quarter ~ site, labeller = global_labeller, scales = "free_x") +
  # scale_color_brewer(palette = "Set1", guide = FALSE) +
  scale_color_manual(values = brewer.pal(3, "Set1")[2],
                     name = "Age Class",
                     labels = c("Adults")) +
  labs(x = "\nYear", y = "Modeled Fertility\n",
       title = "Adult Female Fertility Rates") +
  theme_fc() +
  theme(panel.grid.major.y = element_line(colour = "gray95"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_line(colour = "black"),
        legend.key.width = unit(0.5, "cm")) +
  scale_x_continuous(limits = c(1960, 2015)) +
  scale_y_continuous(#trans = log1p_trans(),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2))

# 6 x 13
cowplot::plot_grid(p1, p2, nrow = 1, scale = 0.95, labels = c("a", "b"))



# Fert filtered
temp <- by_site_qua_var_ff %>%
  filter(var == "rainfall") %>%
  mutate(fitted = purrr::map(null_model, ~ broom::augment(.) %>%
                               tbl_df()))

temp1 <- temp %>%
  select(site, quarter, fitted) %>%
  unnest() %>%
  group_by(site, quarter, year_of) %>%
  summarise(fert = mean(.mu))

temp1$year_of <- as.numeric(temp1$year_of)

full_ff_df <- left_join(full_ff_df, temp1)

temp <- full_ff_df %>%
  filter(var == "rainfall" & !(site == "kakamega" & year_of < 1997))

ggplot(temp,
       aes(x = year_of, y = fert, color = age_class)) +
  geom_line(size = 0.5) +
  facet_grid(quarter ~ site, labeller = global_labeller, scales = "free_x") +
  # scale_color_brewer(palette = "Set1", guide = FALSE) +
  scale_color_manual(values = brewer.pal(3, "Set1")[2],
                     guide = FALSE) +
  labs(x = "\nYear", y = "Modeled Fertility\n",
       title = "Adult Female Fertility Rates") +
  theme_fc() +
  theme(panel.grid.major.y = element_line(colour = "gray95"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_line(colour = "black"),
        legend.key.width = unit(0.5, "cm")) +
  scale_x_continuous(limits = c(1960, 2015)) +
  scale_y_continuous(#trans = log1p_trans(),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2))





# Extra?

temp <- full_f_df %>%
  filter(!(site == "kakamega" & year_of < 1997) & var != "spei_03")

ggplot(filter(temp, quarter == "wettest_q"),
       aes(x = value, y = fert, color = site, fill = site)) +
  geom_point() +
  facet_grid(var ~ site, labeller = global_labeller, scales = "free_x") +
  theme_fc() +
  stat_smooth(method = "lm") +
  scale_color_brewer(palette = "Dark2", guide = FALSE) +
  scale_fill_brewer(palette = "Dark2", guide = FALSE) +
  labs(x = "Scaled climate predictor", y = "Fertility Rate",
       title = "Fertility rates vs. Rainfall")