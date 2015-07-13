# # Calculate hottest/coldest/wettest/driest months for each site
# monthly_clim_means <- climates %>%
#   ungroup() %>%
#   group_by(site, month_of) %>%
#   summarise_each(funs(mean), rain_monthly_mm, tmin_monthly, tmax_monthly)
#
#
# mon_extremes <- function(df){
#
#   temp <- data.frame(
#     warmest_month = df[which(df$tmax_monthly ==
#                                max(df$tmax_monthly)), ]$month_of,
#     coldest_month =  df[which(df$tmin_monthly ==
#                                 min(df$tmin_monthly)), ]$month_of,
#     wettest_month = df[which(df$rain_monthly_mm ==
#                                max(df$rain_monthly_mm)), ]$month_of,
#     driest_month = df[which(df$rain_monthly_mm ==
#                               min(df$rain_monthly_mm)), ]$month_of
#   )
#
#   return(temp)
# }
#
# # Monthly extremes
# monthly_extremes <- monthly_clim_means %>%
#   ungroup() %>%
#   group_by(site) %>%
#   do(mon_extremes(.))
#
#
# # Quarterly
# prec <- monthly_clim_means %>%
#   select(-tmin_monthly, -tmax_monthly) %>%
#   spread(month_of, rain_monthly_mm) %>%
#   select(-site)
#
# tmax <- monthly_clim_means %>%
#   select(-rain_monthly_mm, -tmin_monthly) %>%
#   spread(month_of, tmax_monthly) %>%
#   select(-site)
#
# tmin <- monthly_clim_means %>%
#   select(-rain_monthly_mm, -tmax_monthly) %>%
#   spread(month_of, tmin_monthly) %>%
#   select(-site)
#
# # Calculate hottest/coldest/wettest/driest quarters for each site
# qua <- data.frame(extremes(tmin = tmin, tmax = tmax, prec = prec))
# qua$site <- monthly_extremes$site
#
# quarterly_extremes <- qua %>%
#   mutate_each(funs(month.abb[.]), -site) %>%
#   select(site, 1:4)
#
# names(quarterly_extremes)[-1] %<>% paste0("_quarter")
# names(quarterly_extremes)[-1] <- tolower(names(quarterly_extremes)[-1])
#
# clim_extremes <- inner_join(monthly_extremes, quarterly_extremes) %>%
#   gather(variable, month_of, -site) %>%
#   arrange(site, variable)

source('analysis_scripts/bioclim.R')

get_bioclim <- function(df){

  tmin <- df %>%
    ungroup() %>%
    select(-year_of, -rain_monthly_mm, -tmax_monthly) %>%
    spread(month_of, tmin_monthly) %>%
    select(-site)

  tmax <- df %>%
    ungroup() %>%
    select(-year_of, -rain_monthly_mm, -tmin_monthly) %>%
    spread(month_of, tmax_monthly) %>%
    select(-site)

  prec <- df %>%
    ungroup() %>%
    select(-year_of, -tmin_monthly, -tmax_monthly) %>%
    spread(month_of, rain_monthly_mm) %>%
    select(-site)

  b <- data.frame(suppressMessages(bioclim(tmin, tmax, prec, t.as.int = FALSE)))
  names(b)[1] <- "value"
  b$variable <- row.names(b)
  b <- b %>% select(variable, value)

  row.names(b) <- NULL

  return(b)

}

# Bioclim
temp <- climates %>%
  inner_join(select(ann_total, site, year_of, n_months)) %>%
  group_by(site, year_of) %>%
  select(month_of, rain_monthly_mm, tmin_monthly, tmax_monthly) %>%
  do(get_bioclim(.))

temp$variable <- mapvalues(temp$variable,
                           from = c("bioclim_1", "bioclim_2", "bioclim_3",
                                    "bioclim_4", "bioclim_5", "bioclim_6",
                                    "bioclim_7", "bioclim_8", "bioclim_9",
                                    "bioclim_10", "bioclim_11", "bioclim_12",
                                    "bioclim_13", "bioclim_14", "bioclim_15",
                                    "bioclim_16", "bioclim_17", "bioclim_18",
                                    "bioclim_19"),
                           to = c("01_annual_mean_temp", "02_mean_diurnal_range",
                                  "03_isothermality", "04_temp_seasonality",
                                  "05_max_temp_warmest_m", "06_min_temp_coldest_m",
                                  "07_temp_annual_range", "08_mean_temp_wettest_q",
                                  "09_mean_temp_driest_q", "10_mean_temp_warmest_q",
                                  "11_mean_temp_coldest_q", "12_annual_precip",
                                  "13_precip_wettest_m", "14_precip_driest_m",
                                  "15_precip_seasonality", "16_precip_wettest_q",
                                  "17_precip_driest_q", "18_precip_warmest_q",
                                  "19_precip_coldest_q"))

clim_extremes <- spread(temp, variable, value)

ggplot() +
  geom_tile(data = clim_extremes,
            aes(x = site, y = year_of,
                fill = bioclim_8)) +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"),
                       name = "Max Temp of Warmest Month") +
  # facet_grid(. ~ site) +
  theme_bw() +
  labs(x = "Month", y = "Year", title = "Max Temp of Warmest Month\n") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(4, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1955, 2016), breaks = seq(1955, 2015, by = 5))
