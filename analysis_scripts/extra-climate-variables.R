load("ClimatePrep.RData")

source("analysis_scripts/bioclim.R")
source("analysis_scripts/ggcorr.R")

site_coords <- read.csv("data/site_coords.csv")
names(site_coords)[2] <- "long_name"
site_list <- c("rppn-fma", "amboseli", "kakamega", "gombe", "karisoke", "beza", "ssr")
site_coords$site <- site_list
site_coords$site <- factor(site_coords$site, levels = site_list)
sites <- dplyr::select(site_coords, site, lat = Lat, lon = Long)
rm(site_coords)

# Calculate hottest/coldest/wettest/driest months for each site
monthly_clim_means <- climates %>%
  ungroup() %>%
  filter(!is.na(tmin_monthly)) %>%
  group_by(site, month_of) %>%
  summarise_each(funs(mean), tmin_monthly, tmax_monthly, rain_monthly_mm, tavg_monthly) %>%
  mutate(month_num = as.numeric(month_of))

mon_extremes <- function(df, col = "month_of"){

  temp <- data.frame(
    warmest_month = df[which(df$tmax_monthly ==
                               max(df$tmax_monthly)), col][[1]],
    coldest_month = df[which(df$tmin_monthly ==
                               min(df$tmin_monthly)), col][[1]],
    wettest_month = df[which(df$rain_monthly_mm ==
                               max(df$rain_monthly_mm)), col][[1]],
    driest_month = df[which(df$rain_monthly_mm ==
                              min(df$rain_monthly_mm)), col][[1]]
  )

  return(temp)
}

# Monthly extremes
monthly_extremes <- monthly_clim_means %>%
  ungroup() %>%
  group_by(site) %>%
  do(mon_extremes(., "month_num"))


# Quarterly
tmin <- monthly_clim_means %>%
  ungroup() %>%
  select(site, month_of, tmin_monthly) %>%
  spread(month_of, tmin_monthly) %>%
  select(-site)

tmax <- monthly_clim_means %>%
  ungroup() %>%
  select(site, month_of, tmax_monthly) %>%
  spread(month_of, tmax_monthly) %>%
  select(-site)

prec <- monthly_clim_means %>%
  ungroup() %>%
  select(site, month_of, rain_monthly_mm) %>%
  spread(month_of, rain_monthly_mm) %>%
  select(-site)

tavg <- monthly_clim_means %>%
  ungroup() %>%
  select(site, month_of, tavg_monthly) %>%
  spread(month_of, tavg_monthly) %>%
  select(-site)

# Calculate hottest/coldest/wettest/driest quarters for each site
qua <- data.frame(extremes(tmin = tmin, tmax = tmax, prec = prec, tmean = tavg))
qua <- bind_cols(monthly_extremes[, "site"], qua)

# quarterly_extremes <- qua %>%
#   mutate_each(funs(factor(month.abb[.], levels = month.abb)), -site) %>%
#   select(site, 1:4)
quarterly_extremes <- qua

names(quarterly_extremes)[-1] <- names(quarterly_extremes)[-1] %>% paste0("_quarter")
names(quarterly_extremes)[-1] <- tolower(names(quarterly_extremes)[-1])

clim_extremes <- inner_join(monthly_extremes, quarterly_extremes) %>%
  data.frame()

row.names(clim_extremes) <- clim_extremes$site
clim_extremes <- clim_extremes[, -1]

# Bioclim
ann_total <- climates %>%
  group_by(site, year_of) %>%
  summarise(rain_total_mm = sum(rain_monthly_mm),
            n_months = n()) %>%
  filter(n_months == 12)

bioclim_df <- climates %>%
  inner_join(select(ann_total, site, year_of, n_months)) %>%
  filter(year_of < 2016) %>%
  group_by(site) %>%
  select(year_of, month_of, tmin_monthly, tmax_monthly, rain_monthly_mm, tavg_monthly) %>%
  do(get_bioclim_annual(., clim_extremes))

bioclim_df <- bioclim_df %>%
  gather(var, value, -site, -year_of)

bioclim_df$var <- mapvalues(bioclim_df$var,
                           from = c("bioclim_1", "bioclim_2", "bioclim_3",
                                    "bioclim_4", "bioclim_5", "bioclim_6",
                                    "bioclim_7", "bioclim_8", "bioclim_9",
                                    "bioclim_10", "bioclim_11", "bioclim_12",
                                    "bioclim_13", "bioclim_14", "bioclim_15",
                                    "bioclim_16", "bioclim_17", "bioclim_18",
                                    "bioclim_19"),
                           to = c("v01_annual_mean_temp", "v02_mean_diurnal_range",
                                  "v03_isothermality", "v04_temp_seasonality",
                                  "v05_max_temp_warmest_m", "v06_min_temp_coldest_m",
                                  "v07_temp_annual_range", "v08_mean_temp_wettest_q",
                                  "v09_mean_temp_driest_q", "v10_mean_temp_warmest_q",
                                  "v11_mean_temp_coldest_q", "v12_annual_precip",
                                  "v13_precip_wettest_m", "v14_precip_driest_m",
                                  "v15_precip_seasonality", "v16_precip_wettest_q",
                                  "v17_precip_driest_q", "v18_precip_warmest_q",
                                  "v19_precip_coldest_q"))

bioclim_df <- spread(bioclim_df, var, value)
bioclim_df <- select(bioclim_df, site, year_of,
                     contains("_q"), contains("v01"), contains("v12"))


# ---- bioclim_plots ------------------------------------------------------

library(corrplot)

# Plot temperatures in different quarters
qua_temp <- bioclim_df %>%
  gather(var, value, -site, -year_of) %>%
  filter((str_detect(var, "_q") | str_detect(var, "v01")) & str_detect(var, "mean_temp"))

qua_temp$var <- str_split_fixed(qua_temp$var, pattern = "_", 5)[, 4]
qua_temp$var <- revalue(qua_temp$var, c("temp" = "annual"))

# Within-site corrplot of temperature in different quarters
par(mfrow = c(2, 4))
for (i in seq_along(sites$site)) {

  cur_site <- sites$site[i]

  temp1 <- qua_temp %>%
    ungroup() %>%
    filter(site == cur_site) %>%
    spread(var, value) %>%
    select(-site, -year_of) %>%
    cor()

  corrplot.mixed(temp1, col = colorRampPalette(rev(brewer.pal(11, "RdBu")))(100),
                 title = cur_site, mar = c(1, 0, 1, 0),
                 tl.col = "black", tl.cex = 0.7)
}

# Between-site corrplot of temperature in each quarter
temp <- qua_temp
temp$site <- revalue(temp$site, site_map)
# par(mfrow = c(2, 3))
par(mfrow = c(1, 5))
for (i in seq_along(levels(factor(temp$var)))) {

  cur_var <- levels(factor(temp$var))[i]

  temp1 <- temp %>%
    ungroup() %>%
    filter(var == cur_var) %>%
    spread(site, value) %>%
    select(-var, -year_of) %>%
    cor(., use = "na.or.complete", method = "spearman")

  corrplot.mixed(temp1, col = colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(100),
                 title = cur_var, mar = c(10,0,10,0), tl.col = "black",
                 tl.cex = 0.8, cl.cex = 0.8, cl.align.text = "l")
}

p1 <- ggplot(data = qua_temp, aes(x = year_of, y = value)) +
  geom_line(aes(group = var), size = 0.2) +
  geom_point(aes(fill = var), shape = 21, colour = "black",
             size = 2, stroke = 0.2, alpha = 0.75) +
  facet_grid(site ~ ., scales = "free_y") +
  # scale_fill_manual(values = brewer.pal(4, "Set3")[c(3, 2, 4, 1)],
  #                   name = "Quarter") +
  theme_fc() +
  labs(x = "\nYear", y = "Precip (mm)\n", title = "Raw Value") +
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11)) +
  scale_x_continuous(limits = c(1955, 2016), breaks = seq(1955, 2015, by = 5))

qua_temp <- qua_temp %>%
  group_by(site, var) %>%
  mutate(scale_value = scale(value)) %>%
  ungroup()

p2 <- ggplot(data = qua_temp, aes(x = year_of, y = scale_value)) +
  geom_line(aes(group = var), size = 0.2) +
  geom_point(aes(fill = var), shape = 21, colour = "black",
             size = 2, stroke = 0.2, alpha = 0.75) +
  facet_grid(site ~ ., scales = "free_y") +
  scale_fill_manual(values = brewer.pal(4, "Set3")[c(3, 2, 4, 1)],
                    name = "Quarter") +
  theme_fc() +
  labs(x = "\nYear", y = "Scaled Precip\n", title = "Scaled Value") +
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11)) +
  scale_x_continuous(limits = c(1955, 2016), breaks = seq(1955, 2015, by = 5))

gridExtra::grid.arrange(p1, p2, ncol = 2)

# Plot precipitation in different quarters
qua_precip <- bioclim_df %>%
  gather(var, value, -site, -year_of) %>%
  filter((str_detect(var, "_q") | str_detect(var, "v12")) & str_detect(var, "precip"))

qua_precip$var <- str_split_fixed(qua_precip$var, pattern = "_", 4)[, 3]
qua_precip$var <- revalue(qua_precip$var, c("precip" = "annual"))

# Precipitation corrplot
par(mfrow = c(2, 4))
for (i in seq_along(sites$site)) {

  cur_site <- sites$site[i]

  temp1 <- qua_precip %>%
    ungroup() %>%
    filter(site == cur_site) %>%
    spread(var, value) %>%
    select(-site, -year_of) %>%
    cor()

  corrplot.mixed(temp1, col = colorRampPalette(brewer.pal(11, "BrBG"))(100),
                 title = cur_site, mar = c(1, 0, 1, 0),
                 tl.col = "black", tl.cex = 0.7)
}

# Between-site corrplot of precipitation in each quarter
temp <- qua_precip
temp$site <- revalue(temp$site, site_map)
par(mfrow = c(2, 3))
for (i in seq_along(levels(factor(temp$var)))) {

  cur_var <- levels(factor(temp$var))[i]

  temp1 <- temp %>%
    ungroup() %>%
    filter(var == cur_var) %>%
    spread(site, value) %>%
    select(-var, -year_of) %>%
    cor(., use = "na.or.complete", method = "spearman")

  corrplot.mixed(temp1, col = colorRampPalette(brewer.pal(11, "BrBG"))(100),
                 title = cur_var, mar = c(1,0,1,0), tl.col = "black",
                 tl.cex = 0.8, cl.cex = 0.8, cl.align.text = "l")
}

ggplot(data = qua_precip, aes(x = year_of, y = value)) +
  geom_line(aes(group = var), size = 0.2) +
  geom_point(aes(fill = var), shape = 21, colour = "black",
             size = 2, stroke = 0.2, alpha = 0.75) +
  facet_grid(site ~ ., scales = "free_y") +
  # scale_fill_manual(values = brewer.pal(4, "Set3")[c(3, 2, 4, 1)],
  #                   name = "Quarter") +
  theme_bw() +
  labs(x = "\nYear", y = "Precip (mm)\n", title = "") +
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11)) +
  scale_x_continuous(limits = c(1955, 2016), breaks = seq(1955, 2015, by = 5)) +
  expand_limits(y = 0)


# ---- ind_clim -----------------------------------------------------------

indclim_df <- ind_df %>%
  filter(index %in% c("nino3.4")) %>%
  mutate(year_of = year(date_of),
         month_of = month(date_of, label = TRUE, abbr = TRUE)) %>%
  group_by(index) %>%
  filter(year_of < 2016) %>%
  do(get_indclim(., clim_extremes)) %>%
  ungroup()

# df <- climates %>%
#   inner_join(select(ann_total, site, year_of, n_months)) %>%
#   group_by(site) %>%
#   select(year_of, month_of, tmin_monthly, tmax_monthly, rain_monthly_mm, tavg_monthly) %>%
#   filter(site == "ssr")

indclim_df <- indclim_df %>%
  gather(var, value, -index, -site, -year_of)

indclim_df$var <- mapvalues(indclim_df$var,
                                 from = c("indclim_1", "indclim_2", "indclim_3",
                                          "indclim_4", "indclim_5", "indclim_6",
                                          "indclim_7", "indclim_8", "indclim_9"),
                                 to = c("v01_annual_mean", "v02_seasonality",
                                        "v03_max", "v04_min", "v05_annual_range",
                                        "v06_mean_warmest_q", "v07_mean_coldest_q",
                                        "v08_mean_wettest_q", "v09_mean_driest_q"))

indclim_df$site <- factor(indclim_df$site,
                          levels = c("rppn-fma", "amboseli", "kakamega",
                                     "gombe", "karisoke",
                                     "beza", "ssr"))

indclim_df <- spread(indclim_df, var, value)
indclim_df <- select(indclim_df, index, site, year_of,
                     contains("_q"), contains("v01"))

qua_ind <- indclim_df %>%
  gather(var, value, -site, -year_of, -index)

qua_ind$var <- str_split_fixed(qua_ind$var, pattern = "_", 4)[, 3]

# qua_ind <- filter(qua_ind, var != "mean")
qua_ind$var <- revalue(qua_ind$var, c("mean" = "annual"))

ggplot(data = qua_ind, aes(x = year_of, y = value)) +
  geom_line(aes(group = var), size = 0.2) +
  geom_point(aes(fill = var), shape = 21, colour = "black",
             size = 2, stroke = 0.2, alpha = 0.75) +
  facet_grid(site ~ ., scales = "free_y") +
  scale_fill_manual(values = brewer.pal(4, "Set3")[c(3, 2, 4, 1)],
                    name = "Quarter") +
  theme_bw() +
  labs(x = "\nYear", y = "Mean index value\n", title = "") +
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11)) +
  scale_x_continuous(limits = c(1955, 2016), breaks = seq(1955, 2015, by = 5))

par(mfrow = c(2, 4))
for (i in seq_along(sites$site)) {

  cur_site <- sites$site[i]

  temp1 <- qua_ind %>%
    ungroup() %>%
    filter(site == cur_site) %>%
    spread(var, value) %>%
    select(-site, -year_of, -index) %>%
    cor()

  corrplot.mixed(temp1, col = rev(colorRampPalette(brewer.pal(11, "PiYG"))(100)),
                 title = cur_site, mar = c(1, 0, 1, 0),
                 tl.col = "black", tl.cex = 0.7)
}

# Between-site corrplot of nino3.4 in each quarter
temp <- qua_ind
temp$site <- revalue(temp$site, site_map)
par(mfrow = c(2, 3))
for (i in seq_along(levels(factor(temp$var)))) {

  cur_var <- levels(factor(temp$var))[i]

  temp1 <- temp %>%
    ungroup() %>%
    filter(var == cur_var) %>%
    spread(site, value) %>%
    select(-var, -year_of, -index) %>%
    cor(., use = "na.or.complete", method = "spearman")

  corrplot.mixed(temp1, col = colorRampPalette(rev(brewer.pal(11, "PiYG")))(100),
                 title = cur_var, mar = c(1,0,1,0), tl.col = "black",
                 tl.cex = 0.8, cl.cex = 0.8, cl.align.text = "l")
}


# Get indices relevant to each site
# Must show significant correlation with rain/temperature anomalies for > 1 month
# temp <- phase_cor %>%
#   select(site, month_of, index, new_rain_cor, new_tmin_d_cor, new_tmax_d_cor) %>%
#   gather(var, value, -site, -month_of, -index) %>%
#   filter(abs(value) > 0) %>%
#   arrange(site, index, month_of)
#
# temp1 <- temp %>%
#   ungroup() %>%
#   group_by(site, index, var) %>%
#   summarise(n = n()) %>%
#   filter(n > 1)
#
# temp2 <- temp1 %>%
#   ungroup() %>%
#   group_by(site) %>%
#   do(inds = levels(factor(.$index)))
#
# indclim_df$site <- factor(indclim_df$site,
#                           levels = c("rppn-fma", "amboseli", "kakamega",
#                                      "gombe", "karisoke",
#                                      "beza", "ssr"))
#
# for (i in 1:length(levels(temp2$site))) {
#   current_site <- levels(temp2$site)[i]
#   ind_set <-  temp2[i, ]$inds[[1]]
#   all_inds <- levels(indclim_df$index)
#   indclim_df <- filter(indclim_df, !(site == current_site & index %ni% ind_set))
# }


# indclim_df <- unite(indclim_df, variable, index, var)
#
# indclim_df <- spread(indclim_df, variable, value)



# spei_clim ---------------------------------------------------------------

speiclim_df <- spei %>%
  gather(period, value, -c(1:4)) %>%
  group_by(site, period) %>%
  do(get_speiclim(., clim_extremes)) %>%
  ungroup()

speiclim_df <- speiclim_df %>%
  gather(var, value, -period, -site, -year_of)

speiclim_df$var <- mapvalues(speiclim_df$var,
                            from = c("indclim_1", "indclim_2", "indclim_3",
                                     "indclim_4", "indclim_5", "indclim_6",
                                     "indclim_7", "indclim_8", "indclim_9"),
                            to = c("v01_annual_mean", "v02_seasonality",
                                   "v03_max", "v04_min", "v05_annual_range",
                                   "v06_mean_warmest_q", "v07_mean_coldest_q",
                                   "v08_mean_wettest_q", "v09_mean_driest_q"))

# speiclim_df <- unite(speiclim_df, variable, period, var)
#
speiclim_df <- spread(speiclim_df, var, value)
speiclim_df <- select(speiclim_df, period, site, year_of,
                      contains("_q"), contains("v01"))

qua_spei <- speiclim_df %>%
  gather(var, value, -site, -year_of, -period)

qua_spei$var <- str_split_fixed(qua_spei$var, pattern = "_", 4)[, 3]
qua_spei <- filter(qua_spei, var != "mean")

ggplot(data = filter(qua_spei, period == "spei_01"), aes(x = year_of, y = value)) +
  geom_line(aes(group = var), size = 0.2) +
  geom_point(aes(fill = var), shape = 21, colour = "black",
             size = 2, stroke = 0.2, alpha = 0.75) +
  facet_grid(site ~ ., scales = "free_y") +
  scale_fill_manual(values = brewer.pal(4, "Set3")[c(3, 2, 4, 1)],
                    name = "Quarter") +
  theme_bw() +
  labs(x = "\nYear", y = "Mean index value\n", title = "") +
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11)) +
  scale_x_continuous(limits = c(1955, 2016), breaks = seq(1955, 2015, by = 5))



# ---- final_set ----------------------------------------------------------

q_temp_df <- bioclim_df %>%
  select(site, year_of, matches("temp")) %>%
  mutate(var = "mean_temp")

names(q_temp_df) <- str_replace(names(q_temp_df), pattern = "v.._mean_temp_", "")
names(q_temp_df) <- str_replace(names(q_temp_df), pattern = "v01_annual_mean_temp", "annual")
q_temp_df <- gather(q_temp_df, quarter, value, contains("_q"), annual)

q_precip_df <- bioclim_df %>%
  select(site, year_of, contains("precip")) %>%
  mutate(var = "precip")

names(q_precip_df) <- str_replace(names(q_precip_df), pattern = "v.._precip_", "")
names(q_precip_df) <- str_replace(names(q_precip_df), pattern = "v12_annual_precip", "annual")
q_precip_df <- gather(q_precip_df, quarter, value, contains("_q"), annual)

q_ind_df <- indclim_df %>%
  mutate(temp_var = "index") %>%
  unite(var, c(temp_var, index))

names(q_ind_df) <- str_replace(names(q_ind_df), pattern = "v.._mean_", "")
names(q_ind_df) <- str_replace(names(q_ind_df), pattern = "v01_annual_mean", "annual")
q_ind_df <- gather(q_ind_df, quarter, value, contains("_q"), annual)

q_spei_df <- speiclim_df
names(q_spei_df) <- str_replace(names(q_spei_df), pattern = "v.._mean_", "")
names(q_spei_df) <- str_replace(names(q_spei_df), pattern = "v01_annual_mean", "annual")
q_spei_df <- q_spei_df %>%
  gather(quarter, value, contains("_q"), annual) %>%
  rename(var = period)



# ---- new_ggcorr_plots ---------------------------------------------------

g_list <- list(15)

temp <- qua_temp
temp$site <- revalue(temp$site, site_map)

for (i in seq_along(levels(factor(temp$var)))) {

  cur_var <- levels(factor(temp$var))[i]

  temp1 <- temp %>%
    ungroup() %>%
    filter(var == cur_var) %>%
    spread(site, value) %>%
    select(-var, -year_of)

  g_list[[i]] <- ggcorr_fc(temp1, method = c("na.or.complete", method = "spearman"),
                           geom = "tile", label = TRUE, hjust = 0.75, digits = 2, label_size = 3,
                           label_round = 2, palette = rev(brewer.pal(11, "RdYlBu")),
                           nbreaks = 10, size = 3, layout.exp = 1, color = "gray50") +
    labs(title = ifelse(cur_var == "annual", capitalize(cur_var),
                        paste(capitalize(cur_var), "Quarter"))) +
    guides(fill = FALSE)

}

temp <- qua_precip
temp$site <- revalue(temp$site, site_map)
for (i in seq_along(levels(factor(temp$var)))) {

  cur_var <- levels(factor(temp$var))[i]

  temp1 <- temp %>%
    ungroup() %>%
    filter(var == cur_var) %>%
    spread(site, value) %>%
    select(-var, -year_of)

  g_list[[i + 5]] <- ggcorr_fc(temp1, method = c("na.or.complete", method = "spearman"),
                           geom = "tile", label = TRUE, hjust = 0.75, digits = 2, label_size = 3,
                           label_round = 2, palette = brewer.pal(11, "BrBG"),
                           nbreaks = 10, size = 3, layout.exp = 1, color = "gray50") +
    labs(title = ifelse(cur_var == "annual", capitalize(cur_var),
         paste(capitalize(cur_var), "Quarter"))) +
    guides(fill = FALSE)
}

temp <- qua_ind
temp$site <- revalue(temp$site, site_map)
for (i in seq_along(levels(factor(temp$var)))) {

  cur_var <- levels(factor(temp$var))[i]

  temp1 <- temp %>%
    ungroup() %>%
    filter(var == cur_var) %>%
    spread(site, value) %>%
    select(-var, -year_of, -index)

  g_list[[i + 10]] <- ggcorr_fc(temp1, method = c("na.or.complete", method = "spearman"),
                           geom = "tile", label = TRUE, hjust = 0.75, digits = 2, label_size = 3,
                           label_round = 2, palette = rev(brewer.pal(11, "PiYG")),
                           nbreaks = 10, size = 3, layout.exp = 1, color = "gray50") +
    labs(title = ifelse(cur_var == "annual", capitalize(cur_var),
                        paste(capitalize(cur_var), "Quarter"))) +
    guides(fill = FALSE)
}

cowplot::plot_grid(plotlist = g_list, nrow = 3, ncol = 5,
                   labels = c("a", rep("", 4), "b", rep("", 4), "c", rep("", 4)))

ggsave("~/Desktop/temp.pdf", width = 16, height = 9, units = "in")


# ---- final_set ----------------------------------------------------------

climate_predictors <- bind_rows(q_temp_df, q_precip_df, q_ind_df, q_spei_df)
names(qua) <- tolower(names(qua))

temp <- spread(climate_predictors, var, value)

climate_predictors <- temp %>%
  mutate(sumrow = rowSums(.[4:10])) %>%
  filter(!is.na(sumrow)) %>%
  select(-sumrow) %>%
  gather(var, value, -site, -year_of, -quarter)

# Change "precip" to "rainfall" to avoid name conflict with R's precip data set
climate_predictors$var <- revalue(climate_predictors$var, c(precip = "rainfall"))

save(lh, fert, qua, climate_predictors, sites,
     file = "~/GitHub/plhdbR/ClimatePred3.RData")

















# ---- final_set_old ------------------------------------------------------

temp1 <- select(bioclim_df, site, year_of,
                precip_annual = v12_annual_precip,
                precip_wettest_q = v16_precip_wettest_q,
                precip_driest_q = v17_precip_driest_q,
                precip_seasonality = v15_precip_seasonality,
                temp_mean_annual = v01_annual_mean_temp,
                tmax_warmest = v05_max_temp_warmest_m,
                tmin_coldest = v06_min_temp_coldest_m,
                temp_range_annual = v07_temp_annual_range)

temp2 <- indclim_df %>%
  select(site, year_of, matches("v0[1345]"))

names(temp2) <- str_replace(names(temp2), pattern = "v0[12345]_", "")

# temp2 <- indclim_df %>%
#   select(site, year_of, contains("v01")) %>%
#   rename(amo_mean = amo_v01_annual_mean,
#          dmi_mean = dmi_v01_annual_mean,
#          n34_mean = nino3.4_v01_annual_mean,
#          pdo_mean = pdo_v01_annual_mean,
#          nao_mean = nao_v01_annual_mean)

temp3 <- speiclim_df %>%
  select(site, year_of, matches("03_v0[1345]"))

names(temp3) <- str_replace(names(temp3), pattern = "v0[12345]_", "")

climate_predictors <- temp1 %>%
  left_join(temp2) %>%
  left_join(temp3)

save(lh, fert, climate_predictors, file = "~/GitHub/plhdbR/ClimatePred2.RData")
