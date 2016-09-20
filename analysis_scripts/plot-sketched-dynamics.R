# Long-term means
p_rain <- climates %>%
  filter(site == "ssr") %>%
  group_by(site, month_of) %>%
  summarise(m_rain = mean(rain_monthly_mm, na.rm = TRUE))

p_temp <- climates %>%
  filter(site == "ssr") %>%
  group_by(site, month_of) %>%
  summarise(m_temp = mean(tavg_monthly, na.rm = TRUE))

p_clim <- inner_join(p_rain, p_temp)

p_clim <- rbind(p_clim, p_clim)
p_clim$year_of <- c(rep("X", 12), rep("X+1", 12))
p_clim$m_index <- factor(1:24)
# p_clim$month2 <- as.character(p_clim$month_of)
# p_clim$month2[13:24] <- paste0(" ", p_clim$month2[13:24])
# p_clim$m_index <- factor(p_clim$m_index, labels = p_clim$month2)

p1 <- ggplot(p_clim, aes(x = m_index, y = site, fill = m_temp)) +
  geom_tile(color = "black", size = 0.2) +
  coord_fixed(ratio = 0.5) +
  theme_minimal() +
  scale_x_discrete(labels = rep(month.abb, 2)) +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"),
                       limits = c(26.47, 30.09),
                       name = expression(paste(Tavg, ~degree, C))) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.25, "cm"))

p2 <- ggplot(p_clim, aes(x = m_index, y = site, fill = m_rain)) +
  geom_tile(color = "black", size = 0.2) +
  coord_fixed(ratio = 0.5) +
  theme_minimal() +
  scale_x_discrete(labels = rep(month.abb, 2)) +
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                       limits = c(0, 425),
                       name = "Rain (mm)") +
  labs(x = "", y = "") +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.25, "cm"))

gridExtra::grid.arrange(p1, p2, ncol = 1)


# Two years
t_clim <- climates %>%
  filter(site == "ssr" & year_of %in% c(2013, 2014)) %>%
  select(site, year_of, month_of, rain_monthly_mm, tavg_monthly)

t_clim$m_index <- factor(1:24)

n_clim <- climates_combined %>%
  select(site, year_of, month_of, nino3.4) %>%
  filter(site == "ssr" & year_of %in% c(2013, 2014))

t_clim <- inner_join(t_clim, n_clim)

p3 <- ggplot(t_clim, aes(x = m_index, y = site, fill = tavg_monthly)) +
  geom_tile(color = "black", size = 0.2) +
  coord_fixed(ratio = 0.5) +
  theme_minimal() +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"),
                       name = expression(paste(Tavg, ~degree, C)),
                       limits = c(26.47, 30.09)) +
  labs(x = "", y = "") +
  scale_x_discrete(labels = rep(month.abb, 2)) +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.25, "cm"))

p4 <- ggplot(t_clim, aes(x = m_index, y = site, fill = rain_monthly_mm)) +
  geom_tile(color = "black", size = 0.2) +
  coord_fixed(ratio = 0.5) +
  theme_minimal() +
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                       name = "Rain (mm)",
                       limits = c(0, 425)) +
  labs(x = "", y = "") +
  scale_x_discrete(labels = rep(month.abb, 2)) +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.25, "cm"))

lim <- max(abs(t_clim$nino3.4))

p5 <- ggplot(t_clim, aes(x = m_index, y = site, fill = nino3.4)) +
  geom_tile(color = "black", size = 0.2) +
  coord_fixed(ratio = 0.5) +
  theme_minimal() +
  scale_fill_gradientn(colours = brewer.pal(11, "PiYG"),
                       name = "Niño3.4",
                       limits = c(-lim, lim)) +
  labs(x = "", y = "") +
  scale_x_discrete(labels = rep(month.abb, 2)) +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.25, "cm"))

gridExtra::grid.arrange(p3, p4, p5, ncol = 1)
