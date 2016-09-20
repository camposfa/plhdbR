tavg_qua_sum <- as.data.frame(tt1)
names(tavg_qua_sum) <- month.abb
tavg_qua_sum$site <- site_list
tavg_qua_sum$site <- factor(tavg_qua_sum$site, levels = site_list)
tavg_qua_sum <- gather(tavg_qua_sum, month_of, tavg, -site)
tavg_qua_sum$month_of <- factor(tavg_qua_sum$month_of, levels = month.abb)

tavg_max <- tavg_qua_sum %>%
  group_by(site) %>%
  top_n(1, tavg)

tavg_min <- tavg_qua_sum %>%
  group_by(site) %>%
  top_n(1, -tavg)

p1 <- ggplot() +
  geom_bar(data = monthly_clim_means,
           aes(x = month_of, y = tavg_monthly, fill = tavg_monthly),
           stat = "identity", alpha = 0.5, color = "gray60") +
  geom_line(data = tavg_qua_sum, aes(x = month_of, y = tavg, group = site)) +
  geom_point(data = tavg_max, aes(x = month_of, y = tavg),
             color = brewer.pal(11, "RdBu")[2]) +
  geom_point(data = tavg_min, aes(x = month_of, y = tavg),
             color = brewer.pal(11, "RdBu")[10]) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")), guide = FALSE) +
  theme_fc() +
  facet_grid(site ~ ., labeller = global_labeller) +
  coord_cartesian(ylim = c(15, 30)) +
  labs(x = "\nMonth", y = expression(paste("Average Temperature", ~degree, C)))

rain_qua_sum <- as.data.frame(tt2)
names(rain_qua_sum) <- month.abb
rain_qua_sum$site <- site_list
rain_qua_sum$site <- factor(rain_qua_sum$site, levels = site_list)
rain_qua_sum <- gather(rain_qua_sum, month_of, rain, -site)
rain_qua_sum$month_of <- factor(rain_qua_sum$month_of, levels = month.abb)

rain_max <- rain_qua_sum %>%
  group_by(site) %>%
  top_n(1, rain)

rain_min <- rain_qua_sum %>%
  group_by(site) %>%
  top_n(1, -rain)

p2 <- ggplot() +
  geom_bar(data = monthly_clim_means,
           aes(x = month_of, y = rain_monthly_mm, fill = rain_monthly_mm),
           stat = "identity", alpha = 0.5, color = "gray60") +
  geom_line(data = rain_qua_sum, aes(x = month_of, y = rain, group = site)) +
  geom_point(data = rain_max, aes(x = month_of, y = rain),
             color = brewer.pal(11, "BrBG")[10]) +
  geom_point(data = rain_min, aes(x = month_of, y = rain),
             color = brewer.pal(11, "BrBG")[2]) +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"), guide = FALSE,
                       trans = sqrt_trans()) +
  scale_y_sqrt() +
  theme_fc() +
  facet_grid(site ~ ., labeller = global_labeller) +
  labs(x = "\nMonth", y = "Accumulated Rainfall (mm)")

cowplot::plot_grid(p1, p2, nrow = 1, scale = 0.95, labels = c("a", "b"))
