temp <- rain_selected %>%
  group_by(site, month_of) %>%
  summarise(mean_rain = mean(rain_monthly_mm, na.rm = TRUE))

temp1 <- rain_selected %>%
  group_by(site) %>%
  summarise(med_rain = median(rain_monthly_mm, na.rm = TRUE))

temp2 <- temp %>%
  ungroup() %>%
  group_by(site) %>%
  summarise(med_rain = median(mean_rain, na.rm = TRUE))

ggplot() +
  geom_bar(data = temp,
           aes(x = month_of, y = mean_rain, group = site),
           stat = "identity", color = "black", fill = "gray70",
           alpha = 0.8) +
  geom_hline(data = temp1, aes(yintercept = med_rain), color = "red") +
  # geom_hline(data = temp2, aes(yintercept = med_rain), color = "blue") +
  facet_wrap(~site, ncol = 1, scales = "free_y") +
  expand_limits(y = 0) +
  theme_fc()