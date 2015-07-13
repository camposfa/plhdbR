# Calculate hottest/coldest/wettest/driest months for each site
monthly_clim_means <- climates %>%
  ungroup() %>%
  group_by(site, month_of) %>%
  summarise_each(funs(mean), tmin_monthly, tmax_monthly, rain_monthly_mm, tavg_monthly)


mon_extremes <- function(df){

  temp <- data.frame(
    warmest_month = df[which(df$tmax_monthly ==
                               max(df$tmax_monthly)), ]$month_of,
    coldest_month =  df[which(df$tmin_monthly ==
                                min(df$tmin_monthly)), ]$month_of,
    wettest_month = df[which(df$rain_monthly_mm ==
                               max(df$rain_monthly_mm)), ]$month_of,
    driest_month = df[which(df$rain_monthly_mm ==
                              min(df$rain_monthly_mm)), ]$month_of
  )

  return(temp)
}

# Monthly extremes
monthly_extremes <- monthly_clim_means %>%
  ungroup() %>%
  group_by(site) %>%
  do(mon_extremes(.))


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
qua$site <- monthly_extremes$site

quarterly_extremes <- qua %>%
  mutate_each(funs(factor(month.abb[.], levels = month.abb)), -site) %>%
  select(site, 1:4)

names(quarterly_extremes)[-1] <- names(quarterly_extremes)[-1] %>% paste0("_quarter")
names(quarterly_extremes)[-1] <- tolower(names(quarterly_extremes)[-1])

clim_extremes <- inner_join(monthly_extremes, quarterly_extremes) %>%
  data.frame()

row.names(clim_extremes) <- clim_extremes$site
clim_extremes <- clim_extremes[, -1]



source('analysis_scripts/bioclim.R')

# Bioclim
bioclim_df <- climates %>%
  inner_join(select(ann_total, site, year_of, n_months)) %>%
  group_by(site, year_of) %>%
  select(month_of, tmin_monthly, tmax_monthly, rain_monthly_mm, tavg_monthly) %>%
  do(get_bioclim_annual(., clim_extremes))

bioclim_df$variable <- mapvalues(bioclim_df$variable,
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

temp <- spread(bioclim_df, variable, value)

ggplot() +
  geom_tile(data = temp,
            aes(x = site, y = year_of,
                fill = v19_precip_coldest_q)) +
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                       name = "Precip of Coldest Quarter",
                       trans = sqrt_trans()) +
  # facet_grid(. ~ site) +
  theme_bw() +
  labs(x = "Month", y = "Year", title = "Precip of Coldest Quarter\n") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(4, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1955, 2016), breaks = seq(1955, 2015, by = 5))




# ---- ind_clim -----------------------------------------------------------

indclim_df <- ind_df %>%
  mutate(year_of = year(date_of),
         month_of = month(date_of)) %>%
  group_by(index, year_of) %>%
  filter(year_of < 2015) %>%
  do(get_indclim(., tmin, tmax, prec, tavg))
