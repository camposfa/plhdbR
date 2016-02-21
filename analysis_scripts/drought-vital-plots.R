cond <- "site == 'Capuchin' & age_class == 'newborn' & var == 'precip_annual'"

temp1 <- filter_(mod_df1, cond)
llim <- round_any(min(temp1$lag1), 0.5, floor)
ulim <- round_any(max(temp1$lag1), 0.5, ceiling)

temp <- filter_(mod4, paste(cond, "& scenario == 'mod_1'", sep = ""))
temp <- temp$model[[1]]
newdata <- data.frame(seq(llim - 1, ulim + 1, 0.1))
names(newdata)[1] <- names(coef(temp))[2]
pred <- predict(temp, newdata, type = "link", se.fit = TRUE)

critval <- 1.96 ## approx 95% CI
upr <- pred$fit + (critval * pred$se.fit)
lwr <- pred$fit - (critval * pred$se.fit)
fit <- pred$fit
func <- temp$family$linkinv
newdata$predicted <- func(fit)
newdata$upper_ci <- func(upr)
newdata$lower_ci <- func(lwr)

names(newdata)[1] <- "climate_predictor"

int <- newdata[newdata$climate_predictor == 0, ]$predicted

ggplot() +
  geom_ribbon(data = newdata,
              aes(x = climate_predictor, ymin = lower_ci, ymax = upper_ci),
              fill = "red", color = "red", alpha = 0.2,
              size = 0.8) +
  geom_line(data = newdata,
            aes(x = climate_predictor, y = predicted),
            size = 0.8,
            color = "red") +
  geom_point(data = temp1,
             aes(x = lag1,
                 y = successes / (successes + failures),
                 size = (successes + failures)),
             shape = 21, stroke = 0.8) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_size(name = "Sample Size", range = c(0, 10)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_hline(yintercept = int, lty = 3) +
  geom_vline(xintercept = 0, lty = 3) +
  theme_fc() +
  theme(axis.line.x = element_blank(),
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.key = element_blank()) +
  coord_cartesian(x = c(llim, ulim)) +
  labs(x = "\nStandard Deviations from Mean Annual Rainfall",
       y = "Proportion Surviving\n",
       title = "Rainfall and Newborn Survival\n")


# temp <- filter(mod4, site == "Capuchin" & age_class == "newborn" & var == "precip_annual" & scenario == "mod_1")
# names(newdata)[1] <- names(fixef(temp))[2]

# p1 <- ggplot(newdata, aes(x = climate_predictor, y = predicted)) +
#   geom_line(size = 1) +
#   theme_bw() +
#   scale_y_continuous(limits = c(0, 1)) +
#   geom_hline(yintercept = 0, lty = 2) +
#   geom_hline(yintercept = 1, lty = 2) +
#   geom_hline(yintercept = int, lty = 3) +
#   geom_vline(xintercept = 0, lty = 3) +
#   theme(axis.line.x = element_blank()) +
#   scale_x_continuous(breaks = seq(-5, 5, 1)) +
#   labs(x = "\nStandard Deviations from Mean Annual Rainfall",
#        y = "Probability of Survival\n",
#        title = "Rainfall and Newborn Survival\n")

cond <- "site == 'Capuchin' & var == 'precip_annual'"

temp1 <- filter_(fert_mod_df, paste(cond, "& age_class == 'adult'", sep = ""))
llim <- round_any(min(temp1$lag1), 0.5, floor)
ulim <- round_any(max(temp1$lag1), 0.5, ceiling)

temp <- filter_(mod5, paste(cond, "& scenario == 'mod_1'", sep = ""))
temp <- temp$model[[1]]
newdata <- data.frame(seq(llim - 1, ulim + 1, 0.1))
names(newdata)[1] <- names(coef(temp))[2]
pred <- predict(temp, newdata, type = "link", se.fit = TRUE)

critval <- 1.96 ## approx 95% CI
upr <- pred$fit + (critval * pred$se.fit)
lwr <- pred$fit - (critval * pred$se.fit)
fit <- pred$fit
func <- temp$family$linkinv
newdata$predicted <- func(fit)
newdata$upper_ci <- func(upr)
newdata$lower_ci <- func(lwr)

names(newdata)[1] <- "climate_predictor"

int <- newdata[newdata$climate_predictor == 0, ]$predicted

ggplot() +
  geom_ribbon(data = newdata,
              aes(x = climate_predictor, ymin = lower_ci, ymax = upper_ci),
              fill = "red", color = "red", alpha = 0.2,
              size = 0.8) +
  geom_line(data = newdata,
            aes(x = climate_predictor, y = predicted),
            size = 0.8,
            color = "red") +
  geom_point(data = temp1,
             aes(x = lag1,
                 y = successes / (successes + failures),
                 size = (successes + failures)),
             shape = 21, stroke = 0.8) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_size(name = "Sample Size", range = c(0, 10)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_hline(yintercept = int, lty = 3) +
  geom_vline(xintercept = 0, lty = 3) +
  theme_fc() +
  theme(axis.line.x = element_blank(),
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.key = element_blank()) +
  coord_cartesian(x = c(llim, ulim)) +
  labs(x = "\nStandard Deviations from Mean Annual Rainfall",
       y = "Proportion Reproducing\n",
       title = "Rainfall and Female Fertility\n")




# p2 <- ggplot(newdata, aes(x = climate_predictor, y = predicted)) +
#   geom_line(size = 1) +
#   theme_bw() +
#   scale_y_continuous(limits = c(0, 1)) +
#   geom_hline(yintercept = 0, lty = 2) +
#   geom_hline(yintercept = 1, lty = 2) +
#   geom_hline(yintercept = int, lty = 3) +
#   geom_vline(xintercept = 0, lty = 3) +
#   theme(axis.line.x = element_blank()) +
#   scale_x_continuous(breaks = seq(-5, 5, 1)) +
#   labs(x = "\nStandard Deviations from Mean Annual Rainfall",
#        y = "Annual Probability of Parturition\n",
#        title = "Drought and Female Fertility\n")



library(gridExtra)
grid.arrange(p1, p2, nrow = 1)






















d_surv_models %>%
  filter(str_detect(var, "annual_mean")) %>%
  group_by(site, age_class) %>%
  top_n(-rank, n = 1)

d_best <- arrange(d_best, site, age_class)

temp1 <- d_best[19, ]$model[[1]]
newdata <- data.frame(seq(-10 , 10, 0.1))
names(newdata)[1] <- names(fixef(temp1))[2]
newdata$predicted <- predict(temp1, newdata, re.form = NA, type = "resp")

names(newdata)[1] <- "climate_predictor"



# Plot vital rates per year

# temp <- inner_join(m, select(climate_predictors2, site, year_of, contains("spei")),
#                    by = c("Study.Id" = "site", "year_of"))

temp <- m

temp$site <- factor(temp$Study.Id,
                    levels = c("rppn-fma", "amboseli", "kakamega",
                               "gombe", "karisoke",
                               "beza", "ssr"))

temp$site <- mapvalues(temp$site,
                         from = levels(temp$site),
                         to = c("Muriqui", "Baboon", "Blue Monkey",
                                "Chimpanzee", "Gorilla", "Sifaka",
                                "Capuchin"))

temp1 <- mod_df %>%
  filter(scale == "local" & str_detect(var, "v12_annual_precip")) %>%
  distinct(site, year_of, age_class)

temp <- inner_join(temp, temp1, by = c("site", "year_of", "age_class"))

temp <- temp %>%
  mutate(prop_surviving = successes/trials)

stat_sum_single <- function(fun, geom="point", size = 2, ...) {
  stat_summary(fun.y = fun, colour = "red", geom = geom, size = size, ...)
}

temp <- temp %>%
  group_by(Study.Id, age_class) %>%
  filter(!is.na(lag1)) %>%
  mutate(q = ntile(lag0, 10))

# temp1 <- temp %>%
#   ungroup() %>%
#   group_by(Study.Id, age_class, q) %>%
#   summarise(prop = mean(prop_surviving, na.rm = TRUE))



ggplot(temp, aes(x = q, y = prop_surviving)) +
  stat_smooth(method = "lm", color = "red", fill = "red", se = TRUE) +
  stat_sum_single(mean) +
  # stat_sum_single(mean, geom = "line", size = 0.5) +
  facet_grid(age_class ~ Study.Id) +
  theme_bw()

ggplot(temp, aes(x = lag0, y = prop_surviving, group = age_class, color = lag0)) +
  geom_point(size = 3) +
  facet_grid(Study.Id ~ age_class) +
  scale_color_gradientn(colours = brewer.pal(10, "BrBG"), limits = c(-3, 3)) +
  theme_bw()