temp1 <- surv_set %>%
  select(site, age_class, datasets) %>%
  filter(site == "beza" & age_class == "newborn") %>%
  unnest() %>%
  mutate(deltaAICc_orig = deltaAICc) %>%
  filter(var == "rainfall")

temp2 <- surv_set %>%
  select(site, age_class, pcs) %>%
  filter(site == "beza" & age_class == "newborn") %>%
  unnest() %>%
  rename(var = climate) %>%
  filter(var == "rainfall")

temp1 <- inner_join(temp1, temp2)

# if (length(which(temp1$deltaAICc < -7) > 0)) {
#   temp1[which(temp1$deltaAICc < -7), ]$deltaAICc <- -7
# }
# if (length(which(temp1$deltaAICc > 7) > 0)) {
#   temp1[which(temp1$deltaAICc > 7), ]$deltaAICc <- 7
# }
#
# if (length(which(temp1$ModelBeta < -10) > 0)) {
#   temp1[which(temp1$ModelBeta < -10), ]$ModelBeta <- -10
# }
# if (length(which(temp1$ModelBeta > 10) > 0)) {
#   temp1[which(temp1$ModelBeta > 10), ]$ModelBeta <- 10
# }

ann_text <- distinct(temp1, site, age_class, var, pc)
ann_text$sig <- ifelse(ann_text$pc < 0.5, "sig", "ns")
ann_text$sig <- factor(ann_text$sig, levels = c("ns", "sig"))

lim <- max(abs(temp1$deltaAICc))
p_temp <- ggplot(data = temp1, aes(x = WindowClose, y = WindowOpen)) +
  geom_tile(aes(fill = deltaAICc), color = "white", size = 0.1) +
  theme_gcb_x2() +
  facet_grid(. ~ var, labeller = global_labeller) +
  theme(legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.25, "cm"),
        plot.title = element_text(hjust = 0),
        legend.position = "bottom",
        plot.caption = element_text(size = 16, hjust = 0.5)) +
  labs(title = "Infant Sifaka",
       subtitle = expression(paste(Delta, "AICc for models of survival")),
       caption = expression(paste(Delta, "AICc (compared to null model)")),
       x = "Window close (months)",
       y = "Window open (months)") +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       limits = c(-lim, lim), name = "") +
  coord_fixed() +
  scale_x_continuous(breaks = seq(0, 24, by = 6)) +
  scale_y_continuous(breaks = seq(0, 24, by = 6)) +
  geom_text(data = ann_text, x = 17, y = 6, size = 6,
            aes(label = paste0("p[c] == ", formatC(round(pc, 2), digits = 2, format = "f")),
                color = sig),
            parse = TRUE) +
  scale_color_manual(guide = FALSE, values = c("black", "red"), drop = FALSE)


p_surv_delta_sifaka <- p_temp

lim <- max(abs(temp1$ModelBeta))
p_temp <- plotbetas_fc(temp1, col = brewer.pal(11, "PiYG")) +
  facet_grid(. ~ var, labeller = global_labeller) +
  theme_gcb_x2() +
  theme(legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.25, "cm"),
        plot.title = element_text(hjust = 0),
        plot.caption = element_text(size = 16, hjust = 0.5)) +
  coord_fixed() +
  scale_fill_gradientn(colours = brewer.pal(11, "PiYG"),
                       # trans = sqrt_sign_trans(),
                       limits = c(-lim, lim), name = "") +
  labs(title = "Infant Sifaka",
       subtitle = expression(paste("Coefficient estimates (",
                                   beta, ") for models of survival")),
       caption = expression(paste("Model", ~~beta)))

p_surv_beta_sifaka <- p_temp


p <- cowplot::plot_grid(p_surv_delta_sifaka, p_surv_beta_sifaka,
                        nrow = 1, scale = 0.95, labels = c("(a)", "(b)"))

ggsave(filename = "sifaka_surv_plot.pdf",
       plot = p, width = 13, height = 7, units = "in")
