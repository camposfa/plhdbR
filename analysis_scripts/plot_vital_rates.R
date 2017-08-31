# Function to for the null models
# This is done automatically by climwin
# But its useful to have it separated for plotting vital rates
get_null_model <- function(df) {

  null_mod <- glmer(fate ~ 1 + (1 | year_of), data = df, family = binomial)

  return(null_mod)
}

surv_set <- surv_set %>%
  mutate(null_model = purrr::map(trials, ~get_null_model(.)))

temp <- surv_set %>%
  select(site, age_class, null_model) %>%
  mutate(fitted = purrr::map(null_model, ~ broom::augment(.) %>%
                               tbl_df()))

temp1 <- temp %>%
  select(site, age_class, fitted) %>%
  unnest() %>%
  group_by(site, age_class, year_of) %>%
  summarise(surv = mean(.mu))

temp1$year_of <- as.numeric(temp1$year_of)

p1 <- ggplot(temp1, aes(x = year_of, y =  surv, color = age_class)) +
  geom_line(size = 0.5) +
  facet_grid(. ~ site, labeller = global_labeller) +
  scale_color_manual(values = brewer.pal(3, "Set1")[c(2, 3, 1)],
                     name = "Age Class",
                     labels = c("Adults", "Juveniles", "Infants")) +
  # scale_color_brewer(palette = "Set1", name = "Age Class",
  #                    labels = c("Adults", "Juveniles", "Newborns")) +
  labs(x = "\nYear", y = "Modeled survival\n",
       title = "Survival Rates") +
  theme_gcb_x2() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_x_continuous(limits = c(1960, 2015)) +
  scale_y_continuous(trans = exp_trans(),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2))


fert_set <- fert_set %>%
  mutate(null_model = purrr::map(trials, ~get_null_model(.)))

temp <- fert_set %>%
  select(site, age_class, null_model) %>%
  mutate(fitted = purrr::map(null_model, ~ broom::augment(.) %>%
                               tbl_df()))

temp2 <- temp %>%
  select(site, age_class, fitted) %>%
  unnest() %>%
  group_by(site, age_class, year_of) %>%
  summarise(fert = mean(.mu))

temp2$year_of <- as.numeric(temp2$year_of)

temp2 <- temp2 %>%
  filter(!(site == "kakamega" & year_of < 1997))

p2 <- ggplot(temp2, aes(x = year_of, y = fert, color = age_class)) +
  geom_line(size = 0.5) +
  facet_grid(. ~ site, labeller = global_labeller, scales = "free_x") +
  scale_color_manual(values = brewer.pal(3, "Set1")[2],
                     name = "Age Class",
                     labels = c("Adults")) +
  labs(x = "\nYear", y = "Modeled fertility\n",
       title = "Adult Female Fertility Rates") +
  theme_gcb_x2() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_x_continuous(limits = c(1960, 2015)) +
  scale_y_continuous(#trans = log1p_trans(),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2))

# 9 x 12
cowplot::plot_grid(p1, p2, nrow = 2, scale = 0.95, labels = c("(a)", "(b)"))
