rain_sr <- filter(precip_sites, site == "ssr")

rain_sr <- rain_sr %>%
  mutate(decade = factor(round_any(year_of, 10, floor)),
         plot_date = ymd(paste("2000", month(date_of), "01", sep = "-")))

rain_sr <- rain_sr %>%
  group_by(year_of) %>%
  mutate(cum_rain = cumsum(precip))

rain_sr_sum <- rain_sr %>%
  ungroup() %>%
  group_by(decade, month_of) %>%
  summarise(cum_rain_mean = mean(cum_rain, na.rm = TRUE),
            precip_mean = mean(precip, na.rm = TRUE),
            cum_max = max(cum_rain, na.rm = TRUE),
            cum_min = min(cum_rain, na.rm = TRUE))


ggplot() +
  geom_step(data = rain_sr,
            aes(x = month_of, y = cum_rain, color = decade, group = year_of),
            alpha = 0.2) +
  geom_step(data = rain_sr_sum,
            aes(x = month_of, y = cum_rain_mean, color = decade, group = decade)) +
  scale_color_manual(values = viridis(12), guide = FALSE) +
  facet_grid(. ~ decade) +
  theme_journal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot() +
  geom_ribbon(data = rain_sr_sum,
            aes(x = month_of, ymax = cum_max, ymin = cum_min, group = decade, fill = decade),
            alpha = 0.2) +
  geom_step(data = rain_sr_sum,
            aes(x = month_of, y = cum_rain_mean, color = decade, group = decade)) +
  scale_color_manual(values = viridis(12), guide = FALSE) +
  scale_fill_manual(values = viridis(12), guide = FALSE) +
  facet_grid(. ~ decade) +
  theme_journal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot() +
  geom_boxplot(data = rain_sr,
            aes(x = decade, y = precip, group = decade, fill = precip),
            alpha = 0.2, outlier.shape = NA) +
  # geom_point(data = rain_sr_sum,
  #           aes(x = month_of, y = precip_mean, color = decade)) +
  # scale_color_manual(values = viridis(12), guide = FALSE) +
  facet_wrap(~ month_of) +
  theme_journal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
