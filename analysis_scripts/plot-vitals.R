temp <- ssf %>%
  filter(!(Study.Id == "kakamega" & year_of < 1990) & age_class != "newborn")

temp1 <- ssf %>%
  filter(Study.Id == "kakamega" & year_of < 1990 & age_class != "newborn")

ggplot() +
  geom_path(data = temp, aes(x = year_of, y = f, color= age_class), size = 1) +
  geom_point(data = temp, aes(x = year_of, y = f, color= age_class)) +
  geom_path(data = temp1, aes(x = year_of, y = f, color= age_class), size = 1) +
  geom_point(data = temp1, aes(x = year_of, y = f, color= age_class)) +
  facet_grid(Study.Id ~ .) +
  labs(x = "Year", y = "Per-capita fertility",
       title = "Stage-specific Fertility\n") +
  theme_bw() +
  scale_color_brewer(palette = "Accent", name = "Age Class") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(ssm, aes(x = year_of, y = s, color = age_class)) +
  geom_line(size = 1) +
  geom_point() +
  facet_grid(Study.Id ~ .) +
  labs(x = "Year", y = "Probability of Survival",
       title = "Stage-specific Survival\n") +
  theme_bw() +
  scale_color_brewer(palette = "Accent", name = "Age Class") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5))


temp <- filter(mod_df1, site == "Capuchin" & age_class == "newborn")
temp <- filter(fert_mod_df, var == "precip_annual" & age_class == "adult")

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

ggplot(temp, aes(x = lag1, y = fate)) +
  geom_point(alpha = 0.1) +
  facet_grid(. ~ site) +
  scale_y_continuous(breaks = c(0, 1)) +
  binomial_smooth(lty = 2, color = "#c0392b", fill = "#c0392b") +
  fte_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.background = element_blank())