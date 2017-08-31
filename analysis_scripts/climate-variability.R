
# ---- seasonality --------------------------------------------------------

cv <- function(x) {
  cv <- sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
  return(cv)
}

temp <- clim_nest %>%
  unnest()

temp$site <- revalue(temp$site, site_map)

temp <- temp %>%
  group_by(site, year_of) %>%
  summarise(n = n()) %>%
  inner_join(temp) %>%
  filter(n == 12) %>%
  select(-n)

r_max <- max(temp$rain_monthly_mm, na.rm = TRUE)
t_max <- max(temp$tavg_monthly, na.rm = TRUE)

climatol <- temp %>%
  group_by(site, month_of) %>%
  summarise(rain_ltm = mean(rain_monthly_mm),
            tavg_ltm = mean(tavg_monthly))

calcS <- function(x, no) {

  #x is the monthly climate that is used to calculate the relative entropy distribution
  if (length(which(is.na(x)))!=0) {
    s_ind <- NA
  } else {
    r_bar <- sum(x) #mean rainfall
    pm <- x / r_bar
    pm <- pm[which(pm > 0)]
    qm <- 1 / 12 #prob. distributions
    if (length(pm) >= 1) {
      d_ent <- pm / qm
      d_ent <- sum(pm * log(d_ent, base = 2)) #rel. entropy [D=sum(pm*log2(pm/qm))]
      s_ind <- d_ent * (r_bar / no) #seasonality index
    } else {
      s_ind <- NA
    }
  }
  return(s_ind)
}

si <- climatol %>%
  group_by(site) %>%
  summarise(rain_si = calcS(rain_ltm, r_max),
            tavg_si = calcS(tavg_ltm, t_max)) %>%
  gather(var, value, -site) %>%
  separate(var, c("var", "index"), sep = "_")

si$var <- mapvalues(si$var, from = c("rain", "tavg"),
                       to = c("Rainfall", "Temperature"))


# ---- interannual_variability --------------------------------------------

# mean of monthly SDs
temp1 <- temp %>%
  select(site, year_of, month_of, rain_monthly_mm, tavg_monthly) %>%
  gather(var, value, -site, -year_of, -month_of) %>%
  group_by(site, month_of, var) %>%
  summarise(mu_m = mean(value, na.rm = TRUE),
            sigma_m = cv(value)) %>%
  ungroup() %>%
  group_by(site, var) %>%
  summarise(interannual_var = mean(sigma_m),
            intraannual_var = cv(mu_m)) %>%
  gather(index, value, -site, -var)


temp1$var <- mapvalues(temp1$var, from = c("rain_monthly_mm", "tavg_monthly"),
                       to = c("Rainfall", "Temperature"))

clim_var <- bind_rows(si, temp1) %>%
  spread(index, value) %>%
  group_by(var) %>%
  mutate_if(is.numeric, funs(scale = scale)) %>%
  inner_join(select(births_mean_vec,
                    site = Study.Id, distance))

# ---- clim_var_plot ------------------------------------------------------

p1 <- ggplot(filter(clim_var, var == "Rainfall"),
              aes(x = interannual_var, y = intraannual_var, fill = distance)) +
  geom_abline(slope = 1, intercept = 0, color = "gray80", lty = 2) +
  ggrepel::geom_text_repel(aes(label = site, color = distance), segment.color = "black") +
  geom_point(shape = 21, color = "black", size = 3) +
  facet_wrap(~var, scales = "free") +
  theme_gcb_x2() +
  scale_fill_gradientn(colours = rev(viridis(20, option = "magma")[1:19]),
                       name = "", limits = c(0, 1), breaks = c(0, 1),
                       labels = c("Less-seasonal breeding", "More-seasonal breeding")) +
  scale_color_gradientn(colours = rev(viridis(20, option = "magma")[1:19]),
                       name = "", limits = c(0, 1), breaks = c(0, 1),
                       labels = c("Less-seasonal breeding", "More-seasonal breeding")) +
  theme(legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(1.5, "cm"),
        legend.position = "bottom") +
  labs(x = "Inter-annual variability", y = "Intra-annual variability") +
  coord_equal(xlim = c(25, 150), ylim = c(25, 150))

p2 <- ggplot(filter(clim_var, var == "Temperature"),
       aes(x = interannual_var, y = intraannual_var, fill = distance)) +
  geom_abline(slope = 1, intercept = 0, color = "gray80", lty = 2) +
  ggrepel::geom_text_repel(aes(label = site, color = distance), segment.color = "black") +
  geom_point(shape = 21, color = "black", size = 3) +
  facet_wrap(~var, scales = "free") +
  theme_gcb_x2() +
  scale_fill_gradientn(colours = rev(viridis(20, option = "magma")[1:19]),
                       name = "", limits = c(0, 1), breaks = c(0, 1),
                       labels = c("Less-seasonal breeding", "More-seasonal breeding")) +
  scale_color_gradientn(colours = rev(viridis(20, option = "magma")[1:19]),
                        name = "", limits = c(0, 1), breaks = c(0, 1),
                        labels = c("Less-seasonal breeding", "More-seasonal breeding")) +
  theme(legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(1.5, "cm"),
        legend.position = "bottom") +
  labs(x = "Inter-annual variability", y = "Intra-annual variability") +
  coord_equal(xlim = c(0, 13), ylim = c(0, 13))

cowplot::plot_grid(p1, p2, scale = 0.95, ncol = 2)


# ---- old ----------------------------------------------------------------



get_var <- function(df) {
  cov_df <- df %>%
    group_by(year_of) %>%
    summarise_each(funs(sd(., na.rm = TRUE), cv),
                   rain_anomaly, tavg_anomaly,
                   rain_monthly_mm, tavg_monthly) %>%
    select(-matches("anomaly_cv"))

  return(cov_df)
}

sites_cv <- clim_nest %>%
  mutate(cov_df = purrr::map(climate, get_var)) %>%
  select(site, cov_df) %>%
  unnest() %>%
  gather(var, value, -site, -year_of)

sites_cv$site <- factor(sites_cv$site, levels = site_list)
sites_cv$site <- plyr::revalue(sites_cv$site, site_map)

sites_cv$type <- if_else(str_detect(sites_cv$var, "anomaly"), "Anomaly", "Raw")
sites_cv$type <- factor(sites_cv$type, levels = c("Raw", "Anomaly"))
sites_cv <- separate(sites_cv, var, into = c("var", "measure"), sep = -4)
sites_cv$measure <- str_replace(sites_cv$measure, "_", "")
sites_cv$measure <- toupper(sites_cv$measure)
sites_cv$var <- str_replace_all(sites_cv$var, "_", "")

sites_cv$var <- forcats::fct_recode(sites_cv$var,
                                    "Rainfall" = "rainanomaly",
                                    "Temperature" = "tavganomaly",
                                    "Rainfall" = "rainmonthlymm",
                                    "Temperature" = "tavgmonthly")


cdat <- sites_cv %>%
  group_by(site, var, type, measure) %>%
  summarise(cv_mean = mean(value, na.rm = TRUE),
            n = n())


sites_cv <- inner_join(sites_cv, select(births_mean_vec,
                                        site = Study.Id, distance))

sites_cv_sum <- sites_cv %>%
  # filter(measure == "CV") %>%
  group_by(site, var, measure, type) %>%
  summarise(iqr = IQR(value, na.rm = TRUE),
            distance = first(distance))

ggplot(filter(sites_cv_sum, type == "Anomaly"),
       aes(x = forcats::fct_reorder(site, distance, .desc = TRUE), y = iqr, fill = distance)) +
  geom_col(color = "black") +
  facet_wrap(~var, scales = "free_x", ncol = 1) +
  theme_gcb_x2() +
  scale_fill_gradientn(colours = rev(viridis(11, option = "plasma")),
                       # name = "Degree of breeding seasonality",
                       name = "", limits = c(0, 1), breaks = c(0, 1),
                       labels = c("Less Breeding Seasonality", "More Breeding Seasonality")) +
  coord_flip() +
  theme(legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(1.5, "cm"))

# ggplot(sites_cv, aes(x = value, fill = site, color = site)) +
#   geom_density(alpha = 0.3) +
#   facet_wrap(~ var, scales = "free") +
#   geom_vline(data = cdat, aes(xintercept = cv_mean,  color = site),
#              linetype = 2, size = 0.5) +
#   theme_gcb_x2() +
#   labs(x = "Annual Coefficient of Variation", y = "Density") +
#   scale_fill_brewer(palette = "Dark2") +
#   scale_color_brewer(palette = "Dark2")
#
#
# p1 <- ggplot(filter(sites_cv, type == "Raw"),
#              aes(x = year_of, y = value)) +
#   geom_point(aes(fill = distance, color = distance), shape = 21, size = 1,
#              alpha = 0.7, stroke = 0.25) +
#   facet_grid(var ~ site, scales = "free_y") +
#   geom_hline(data = filter(cdat, type == "Raw"),
#              aes(yintercept = cv_mean),
#              linetype = 2, color = "black") +
#   theme_gcb() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   scale_color_gradientn(colours = rev(viridis(11, option = "plasma")),
#                         name = "Degree of breeding seasonality",
#                         limits = c(0, 1)) +
#   scale_fill_gradientn(colours = rev(viridis(11, option = "plasma")),
#                        name = "Degree of breeding seasonality",
#                        limits = c(0, 1)) +
#   theme(legend.key.height = unit(0.25, "cm"),
#         legend.key.width = unit(1.5, "cm")) +
#   labs(y = "Intra-annual Variability", x = "Year",
#        title = "Raw Values")
#
# p2 <- ggplot(filter(sites_cv, type == "Anomaly"),
#              aes(x = year_of, y = value)) +
#   geom_point(aes(fill = distance, color = distance), shape = 21, size = 1,
#              alpha = 0.7, stroke = 0.25) +
#   facet_grid(var ~ site, scales = "free_y") +
#   geom_hline(data = filter(cdat, type == "Anomaly"),
#              aes(yintercept = cv_mean),
#              linetype = 2, color = "black") +
#   theme_gcb() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   scale_color_gradientn(colours = rev(viridis(11, option = "plasma")),
#                         name = "Degree of breeding seasonality",
#                         limits = c(0, 1)) +
#   scale_fill_gradientn(colours = rev(viridis(11, option = "plasma")),
#                        name = "Degree of breeding seasonality",
#                        limits = c(0, 1)) +
#   theme(legend.key.height = unit(0.25, "cm"),
#         legend.key.width = unit(1.5, "cm")) +
#   labs(y = "Intra-annual Variability", x = "Year",
#        title = "Anomalies")

p1 <- ggplot(filter(sites_cv, var == "Rainfall"),
             aes(x = type, y = value)) +
  geom_point(aes(fill = distance, color = distance), shape = 21, size = 1,
             alpha = 0.7, stroke = 0.25,
             position = position_jitter(width = 0.15, height = 0)) +
  facet_grid(measure ~ site, scales = "free_y") +
  geom_point(data = filter(cdat, var == "Rainfall"), aes(y = cv_mean),
             shape = 21, color = "black", fill = "white", size = 2) +
  theme_gcb() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_color_gradientn(colours = rev(viridis(11, option = "plasma")),
                        # name = "Degree of breeding seasonality",
                        name = "", limits = c(0, 1), breaks = c(0, 1),
                        labels = c("Less Breeding Seasonality", "More Breeding Seasonality")) +
  scale_fill_gradientn(colours = rev(viridis(11, option = "plasma")),
                       # name = "Degree of breeding seasonality",
                       name = "", limits = c(0, 1), breaks = c(0, 1),
                       labels = c("Less Breeding Seasonality", "More Breeding Seasonality")) +
  theme(legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(1.5, "cm")) +
  labs(x = "", y = "SD of Monthly Rainfall (mm)",
       title = "Rainfall Variability")

p2 <- ggplot(filter(sites_cv, var == "Temperature"),
             aes(x = type, y = value)) +
  geom_point(aes(fill = distance, color = distance), shape = 21, size = 1,
             alpha = 0.7, stroke = 0.25,
             position = position_jitter(width = 0.15, height = 0)) +
  facet_grid(measure ~ site, scales = "free_y") +
  geom_point(data = filter(cdat, var == "Temperature"), aes(y = cv_mean),
             shape = 21, color = "black", fill = "white", size = 2) +
  theme_gcb() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_color_gradientn(colours = rev(viridis(11, option = "plasma")),
                        # name = "Degree of breeding seasonality",
                        name = "", limits = c(0, 1), breaks = c(0, 1),
                        labels = c("Less Breeding Seasonality", "More Breeding Seasonality")) +
  scale_fill_gradientn(colours = rev(viridis(11, option = "plasma")),
                       # name = "Degree of breeding seasonality",
                       name = "", limits = c(0, 1), breaks = c(0, 1),
                       labels = c("Less Breeding Seasonality", "More Breeding Seasonality")) +
  theme(legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(1.5, "cm")) +
  labs(x = "", y = expression(paste("SD of Monthly Temperature (", degree~C, ")")),
       title = "Temperature Variability")

cowplot::plot_grid(p1, p2, labels = c("(a)", "(b)"), scale = 0.95, ncol = 1)

# get_cov_monthly <- function(df) {
#   cov_df <- df %>%
#     group_by(month_of) %>%
#     summarise_each(funs(cv), rain_monthly_mm, tavg_monthly)
#
#   return(cov_df)
# }
#
# sites_cv_monthly <- clim_nest %>%
#   mutate(cov_df = purrr::map(climate, get_cov_monthly)) %>%
#   select(site, cov_df) %>%
#   unnest() %>%
#   gather(var, value, -site, -month_of)
#
# sites_cv_monthly$site <- factor(sites_cv_monthly$site, levels = site_list)
# sites_cv_monthly$site <- plyr::revalue(sites_cv_monthly$site, site_map)
# sites_cv_monthly$var <- forcats::fct_recode(sites_cv_monthly$var, Rainfall = "rain_monthly_mm",
#                                             Temperature = "tavg_monthly")
#
# cdat <- sites_cv_monthly %>%
#   group_by(site, var) %>%
#   summarise(cv_mean = mean(value, na.rm = TRUE),
#             n = n())