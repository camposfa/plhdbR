fc_tile_plot <- function(df, var) {

  p <- ggplot(df, aes_string(x = "factor(month_of)", y = "year_of", fill = var)) +
    geom_tile() +
    coord_equal() +
    facet_grid(. ~ site) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "bottom",
          strip.background = element_blank(),
          axis.line = element_blank(),
          strip.text = element_text(face = "bold", size = 11),
          legend.key.width = unit(2, "cm"),
          panel.margin = unit(1, "lines")) +
    scale_y_continuous(limits = c(1945, 2016), breaks = seq(1945, 2015, by = 5))

  return(p)
}


# ---- gpcc_rainfall_plot -------------------------------------------------

fc_tile_plot(precip_sites, "precip") +
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                       name = "Total Precipitation",
                       limits = c(0, 1000),
                       trans = "cubroot") +
  labs(x = "Month", y = "Year", title = "GPCC Rainfall Data\n")

# Plot of GPCC rainfall anomalies
fc_tile_plot(precip_combined, "precip_anom") +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       name = "Precipitation Anomaly (mm)",
                       limits = c(-613, 613),
                       trans = "sqrt_sign") +
  labs(x = "Month", y = "Year", title = "GPCC Rainfall Anomalies\n")


# ---- gpcc_long_term_mean ------------------------------------------------

# ftp://ftp.cdc.noaa.gov/Datasets/gpcc/full_v6/precip.mon.1981-2010.ltm.v6.nc
ltm <- nc_open("data/grids/gpcc/precip.mon.1981-2010.ltm.v6.nc")

precip_ltm <- extract_nc_values(ltm, sites = sites, x_var = "lon",
                                y_var = "lat", t_var = "time",
                                v_var = "precip", convert_0_to_360 = TRUE,
                                t_unit = "days",
                                t_origin = as.POSIXct("1800-01-03"))

precip_ltm <- select(precip_ltm, -date_of, -year_of, precip_ltm = v_var)
precip_ltm$month_of <- factor(precip_ltm$month_of, labels = month.abb)

precip_combined <- inner_join(precip_sites, precip_ltm)

precip_combined <- precip_combined %>%
  rename(ltm = precip_ltm) %>%
  mutate(precip_anom = precip - ltm)


# Plot of GPCC rainfall anomalies
fc_tile_plot(precip_combined, "precip_anom") +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       name = "Precipitation Anomaly (mm)",
                       limits = c(-613, 613),
                       trans = "sqrt_sign") +
  labs(x = "Month", y = "Year", title = "GPCC Rainfall Anomalies\n")

rm(list = c("ltm", "longitude", "latitude", "precip_ltm", "precip_combined"))


# ---- plot_berkeley_data -------------------------------------------------

# Temperature, all T variables

fc_tile_plot(at, "t_monthly") +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"),
                       name = "Temperature") +
  labs(x = "Month", y = "Year", title = "Berkeley Earth Temperature Data\n") +
  facet_grid(var ~ site) +
  coord_cartesian()

lim <- max(abs(at$t_anomaly))
fc_tile_plot(at, "t_anomaly") +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdYlBu")),
                       name = "Temperature Anomaly",
                       limits = c(-lim, lim)) +
  labs(x = "Month", y = "Year", title = "Berkeley Earth Temperature Anomalies\n") +
  facet_grid(var ~ site) +
  coord_cartesian()


# TMAX only

temp <- filter(at, var == "tavg")
lim <- max(abs(temp$t_anomaly))
# TMAX Anomalies
fc_tile_plot(temp, "t_anomaly") +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdYlBu")),
                       name = "Temperature Anomaly",
                       limits = c(-lim, lim)) +
  labs(x = "Month", y = "Year", title = "Berkeley Earth TMAX Anomalies\n")

fc_tile_plot(temp, "t_monthly") +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"),
                       guide = FALSE,
                       name = "Temperature") +
  labs(x = "Month", y = "Year", title = "Berkeley Earth TMAX\n")


# ---- site_plot ----------------------------------------------------------

temp <- filter(at, var == "tavg" & site == "ssr")
lim <- max(abs(temp$t_anomaly))

p1 <- ggplot(temp, aes(x = month_of, y = year_of, fill = t_anomaly)) +
  geom_tile() +
  coord_equal() +
  # facet_grid(. ~ site) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(0.25, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1979, 2016), breaks = seq(1945, 2015, by = 5)) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdYlBu")),
                       name = expression(paste(degree, C)),
                       limits = c(-lim, lim)) +
  labs(x = "Month", y = "Year", title = "Temperature Anomaly\n")

p2 <- ggplot(temp, aes(x = month_of, y = year_of, fill = t_monthly)) +
  geom_tile() +
  coord_equal() +
  # facet_grid(. ~ site) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(0.25, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1979, 2016), breaks = seq(1945, 2015, by = 5)) +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"),
                       name = expression(paste(degree, C))) +
  labs(x = "Month", y = "Year", title = "Average Temperature\n")

temp <- filter(rain_selected, site == "ssr")
lim <- max(abs(temp$rain_anomaly))

p3 <- ggplot(temp, aes(x = month_of, y = year_of, fill = rain_monthly_mm)) +
  geom_tile() +
  coord_equal() +
  # facet_grid(. ~ site) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(0.25, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1979, 2016), breaks = seq(1945, 2015, by = 5)) +
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                       trans = sqrt_trans(),
                       name = "mm") +
  labs(x = "Month", y = "Year", title = "Rainfall\n")

p4 <- ggplot(temp, aes(x = month_of, y = year_of, fill = rain_anomaly)) +
  geom_tile() +
  coord_equal() +
  # facet_grid(. ~ site) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(0.25, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1979, 2016), breaks = seq(1945, 2015, by = 5)) +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       name = "mm",
                       trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  labs(x = "Month", y = "Year", title = "Rainfall Anomaly\n")

temp <- ind_df %>%
  filter(index == "nino3.4") %>%
  mutate(month_of = month(date_of, label = TRUE),
         year_of = year(date_of))

lim <- max(abs(temp$value))

p5 <- ggplot(temp, aes(x = month_of, y = year_of, fill = value)) +
  geom_tile() +
  coord_equal() +
  # facet_grid(. ~ site) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(0.25, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1979, 2016), breaks = seq(1945, 2015, by = 5)) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "PiYG")),
                       name = "",
                       trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  labs(x = "Month", y = "Year", title = "ENSO Index\n")

cowplot::plot_grid(p2, p1, p3, p4, p5, nrow = 1, align = "hv")

# ---- plot_spei ----------------------------------------------------------

# Plot data
fc_tile_plot(spei, "spei_06") +
  scale_fill_gradientn(colours = brewer.pal(11, "PuOr"), name = "SPEI") +
  labs(x = "Month", y = "Year", title = "6-Month SPEI Drought Index\n")


# ---- rainfall_source_comparisons ----------------------------------------

# Station rain gauges vs. gpcc
source_comp %>%
  filter(!is.na(rain_gauge) & !is.na(gpcc)) %>%
  select(site, date_of, rain_gauge, gpcc) %>%
  tbl_df() %>%
  ggplot(aes(x = rain_gauge, y = gpcc, color = site)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  facet_wrap(~site, scales = "free") +
  coord_fixed() +
  labs(x = "Rain Gauge",
       y = "GPCC Weather Stations",
       title = "Station rain gauges vs. GPCC\n") +
  theme_bw()

# ggsave(filename = "Rain Gauge VS GPCC.pdf",
#        path = "plots/source_comparisons",
#        width = 12, height = 9, units = "in")

# Station rain gauges vs. trmm satellite
source_comp %>%
  filter(!is.na(rain_gauge) & !is.na(satellite_trmm)) %>%
  select(site, date_of, rain_gauge, satellite_trmm) %>%
  tbl_df() %>%
  ggplot(aes(x = rain_gauge, y = satellite_trmm, color = site)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  facet_wrap(~site, scales = "free") +
  coord_fixed() +
  labs(x = "Rain Gauge",
       y = "TRMM Satellite",
       title = "Station rain gauges vs. TRMM satellite\n") +
  theme_bw()

# ggsave(filename = "Rain Gauge VS TRMM Satellite.pdf",
#        path = "plots/source_comparisons",
#        width = 12, height = 9, units = "in")

# Station rain gauges vs. gpcp satellite
source_comp %>%
  filter(!is.na(rain_gauge) & !is.na(satellite_gpcp)) %>%
  select(site, date_of, rain_gauge, satellite_gpcp) %>%
  tbl_df() %>%
  ggplot(aes(x = rain_gauge, y = satellite_gpcp, color = site)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  facet_wrap(~site, scales = "free") +
  coord_fixed() +
  labs(x = "Rain Gauge",
       y = "GPCP Satellite",
       title = "Station rain gauges vs. GPCP satellite\n") +
  theme_bw()

# ggsave(filename = "Rain Gauge VS GPCP Satellite.pdf",
#        path = "plots/source_comparisons",
#        width = 12, height = 9, units = "in")

# gpcc vs. trmm satellite
source_comp %>%
  filter(!is.na(gpcc) & !is.na(satellite_trmm)) %>%
  select(site, date_of, gpcc, satellite_trmm) %>%
  tbl_df() %>%
  ggplot(aes(x = satellite_trmm, y = gpcc, color = site)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  facet_wrap(~site, scales = "free") +
  coord_fixed() +
  labs(x = "TRMM Satellite",
       y = "GPCC Weather Stations",
       title = "TRMM satellite vs. GPCC\n") +
  theme_bw()

# ggsave(filename = "TRMM Satellite VS GPCC.pdf",
#        path = "plots/source_comparisons",
#        width = 12, height = 9, units = "in")

# gpcc vs. gpcp satellite
source_comp %>%
  filter(!is.na(gpcc) & !is.na(satellite_gpcp)) %>%
  select(site, date_of, gpcc, satellite_gpcp) %>%
  tbl_df() %>%
  ggplot(aes(x = satellite_gpcp, y = gpcc, color = site)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  facet_wrap(~site, scales = "free") +
  coord_fixed() +
  labs(x = "GPCP Satellite",
       y = "GPCC Weather Stations",
       title = "GPCP satellite vs. GPCC\n") +
  theme_bw()

# ggsave(filename = "GPCP Satellite VS GPCC.pdf",
#        path = "plots/source_comparisons",
#        width = 12, height = 9, units = "in")

# gpcp satellite vs. trmm satellite
source_comp %>%
  filter(!is.na(satellite_gpcp) & !is.na(satellite_trmm)) %>%
  select(site, date_of, satellite_gpcp, satellite_trmm) %>%
  tbl_df() %>%
  ggplot(aes(x = satellite_trmm, y = satellite_gpcp, color = site)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  facet_wrap(~site, scales = "free") +
  labs(x = "TRMM Satellite",
       y = "GPCP Satellite",
       title = "TRMM satellite vs. GPCP satellite\n") +
  coord_fixed() +
  theme_bw()

# ggsave(filename = "TRMM Satellite VS GPCP Satellite.pdf",
#        path = "plots/source_comparisons",
#        width = 12, height = 9, units = "in")


# ---- plot_rainfall_summaries --------------------------------------------

# Accumulated monthly rainfall
fc_tile_plot(rain_selected, "rain_monthly_mm") +
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                       name = "Rainfall Total (mm)",
                       # guide = FALSE,
                       trans = "cubroot") +
  labs(x = "Month", y = "Year", title = "Total Monthly Rainfall\n")


# Data sources
fc_tile_plot(rain_selected, "data_source") +
  scale_fill_brewer(name = "Data Source",
                    # guide = FALSE,
                    palette = "Dark2") +
  labs(x = "Month", y = "Year", title = "Data Sources for Composite Rainfall Set\n") +
  theme(legend.key.width = unit(1, "cm"))

lim <- max(abs(rain_selected$rain_anomaly))
# Rainfall anomalies
fc_tile_plot(rain_selected, "rain_anomaly") +
  labs(x = "Month", y = "Year", title = "Rainfall Anomalies\n") +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       name = "Rainfall Anomaly (mm)",
                       trans = "sqrt_sign",
                       # guide = FALSE,
                       limits = c(-lim, lim))


# ---- anomaly_phase_analysis ---------------------------------------------

get_phase <- function(df){
  df$phase <- cut(df$value, include.lowest = TRUE,
                  breaks = c(min(df$value), -sd(df$value),
                             sd(df$value), max(df$value)),
                  labels = c("Negative Phase", "Neutral Phase",
                             "Positive Phase"))
  return(df)
}

ind_phases <- dlply(ind_df, .(index), function(x) get_phase(x))
ind_phases <- bind_rows(ind_phases)

monthly_anom <- climates %>%
  ungroup() %>%
  select(site, date_of, month_of, contains("anomaly"), contains("detrended")) %>%
  inner_join(ind_phases) %>%
  arrange(site, date_of, index, phase)

monthly_anom_summary <- monthly_anom %>%
  group_by(site, month_of, index, phase) %>%
  summarise(med_rain_anomaly = median(rain_anomaly, na.rm = TRUE),
            med_tmin_anomaly = median(tmin_anomaly, na.rm = TRUE),
            med_tavg_anomaly = median(tavg_anomaly, na.rm = TRUE),
            med_tmax_anomaly = median(tmax_anomaly, na.rm = TRUE),
            med_tmin_detrended = median(tmin_detrended, na.rm = TRUE),
            med_tavg_detrended = median(tavg_detrended, na.rm = TRUE),
            med_tmax_detrended = median(tmax_detrended, na.rm = TRUE),
            n = n())

for (i in 1:length(levels(monthly_anom$site))) {

  current_site <- levels(monthly_anom$site)[i]

  # TMIN
  lt <- min(filter(monthly_anom_summary, site == current_site)$med_tmin_detrended)
  ut <- max(filter(monthly_anom_summary, site == current_site)$med_tmin_detrended)
  lim <- max(abs(lt), abs(ut))

  p <- ggplot() +
    geom_bar(data = filter(monthly_anom_summary, site == current_site),
             aes(x = month_of, y = med_tmin_detrended,
                 fill = med_tmin_detrended),
             stat = "identity", position = "dodge",
             width = 0.8, size = 0.1, color = "black") +
    stat_summary(data = filter(monthly_anom, site == current_site),
                 aes(x = month_of, y = tmin_detrended),
                 fun.data = mean_cl_boot, geom = "errorbar",
                 width = 0.3, color = "black", alpha = 0.4) +
    geom_hline(yintercept = 0, size = 0.1) +
    facet_grid(index ~ phase) +
    labs(y = expression("Median TMIN Anomaly"), x = "Month") +
    theme_bw() +
    scale_fill_gradientn(colours = rev(brewer.pal(11, "RdYlBu")),
                         name = "Median Anomaly",
                         limits = c(-lim, lim)) +
    theme(strip.background = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(0.2, "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5))

  f <- paste(i, "_", current_site, ".pdf", sep = "")

  ggsave(filename = f, plot = p,
         path = "plots/phase_anomaly_plots/tmin_anomalies",
         width = 12, height = 12, units = "in")

  # TAVG
  lt <- min(filter(monthly_anom_summary, site == current_site)$med_tavg_detrended)
  ut <- max(filter(monthly_anom_summary, site == current_site)$med_tavg_detrended)
  lim <- max(abs(lt), abs(ut))

  p <- ggplot() +
    geom_bar(data = filter(monthly_anom_summary, site == current_site),
             aes(x = month_of, y = med_tavg_detrended,
                 fill = med_tavg_detrended),
             stat = "identity", position = "dodge",
             width = 0.8, size = 0.1, color = "black") +
    stat_summary(data = filter(monthly_anom, site == current_site),
                 aes(x = month_of, y = tavg_detrended),
                 fun.data = mean_cl_boot, geom = "errorbar",
                 width = 0.3, color = "black", alpha = 0.4) +
    geom_hline(yintercept = 0, size = 0.1) +
    facet_grid(index ~ phase) +
    labs(y = expression("Median TAVG Anomaly"), x = "Month") +
    theme_bw() +
    scale_fill_gradientn(colours = rev(brewer.pal(11, "RdYlBu")),
                         name = "Median Anomaly",
                         limits = c(-lim, lim)) +
    theme(strip.background = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(0.2, "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5))

  f <- paste(i, "_", current_site, ".pdf", sep = "")

  ggsave(filename = f, plot = p,
         path = "plots/phase_anomaly_plots/tavg_anomalies",
         width = 12, height = 12, units = "in")

  # TMAX
  lt <- min(filter(monthly_anom_summary, site == current_site)$med_tmax_detrended)
  ut <- max(filter(monthly_anom_summary, site == current_site)$med_tmax_detrended)
  lim <- max(abs(lt), abs(ut))

  p <- ggplot() +
    geom_bar(data = filter(monthly_anom_summary, site == current_site),
             aes(x = month_of, y = med_tmax_detrended,
                 fill = med_tmax_detrended),
             stat = "identity", position = "dodge",
             width = 0.8, size = 0.1, color = "black") +
    stat_summary(data = filter(monthly_anom, site == current_site),
                 aes(x = month_of, y = tmax_detrended),
                 fun.data = mean_cl_boot, geom = "errorbar",
                 width = 0.3, color = "black", alpha = 0.4) +
    geom_hline(yintercept = 0, size = 0.1) +
    facet_grid(index ~ phase) +
    labs(y = expression("Median TMAX Anomaly"), x = "Month") +
    theme_bw() +
    scale_fill_gradientn(colours = rev(brewer.pal(11, "RdYlBu")),
                         name = "Median Anomaly",
                         limits = c(-lim, lim)) +
    theme(strip.background = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(0.2, "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5))

  f <- paste(i, "_", current_site, ".pdf", sep = "")

  ggsave(filename = f, plot = p,
         path = "plots/phase_anomaly_plots/tmax_anomalies",
         width = 12, height = 12, units = "in")

  # RAIN
  p <- ggplot() +
    geom_bar(data = filter(monthly_anom_summary, site == current_site),
             aes(x = month_of, y = med_rain_anomaly,
                 fill = med_rain_anomaly),
             stat = "identity", position = "dodge",
             width = 0.8, size = 0.1, color = "black") +
    stat_summary(data = filter(monthly_anom, site == current_site),
                 aes(x = month_of, y = rain_anomaly),
                 fun.data = mean_cl_boot, geom = "errorbar",
                 width = 0.3, color = "black", alpha = 0.4) +
    stat_summary(data = filter(monthly_anom, site == current_site),
                 aes(x = month_of, y = rain_anomaly),
                 fun.y = median, geom = "point",
                 size = 2, color = "black", alpha = 0.4) +
    geom_hline(yintercept = 0, size = 0.1) +
    facet_grid(index ~ phase) +
    labs(y = expression("Median Rainfall Anomaly (mm)"), x = "Month") +
    theme_bw() +
    scale_fill_gradient2(low = "#8c510a", high = "#01665e", mid = "#f5f5f5",
                         trans = "sqrt_sign",
                         name = "Median Anomaly") +
    theme(strip.background = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(0.2, "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5))

  f <- paste(i, "_", current_site, ".pdf", sep = "")

  ggsave(filename = f, plot = p,
         path = "plots/phase_anomaly_plots/rain_anomalies",
         width = 12, height = 12, units = "in")

}


# ---- index_correlations -------------------------------------------------

phase_cor <- climates %>%
  select(site, year_of, date_of, month_of, contains("anomaly"), contains("detrended")) %>%
  inner_join(ind_df) %>%
  group_by(site, month_of, index) %>%
  arrange(year_of) %>%
  summarise(ind_rain_cor = cor(value, rain_anomaly, use = "na.or.complete"),
            ind_rain_p = cor.test(value, rain_anomaly)$p.value,
            ind_tmin_cor = cor(value, tmin_anomaly, use = "na.or.complete"),
            ind_tmin_p = cor.test(value, tmin_anomaly)$p.value,
            ind_tavg_cor = cor(value, tavg_anomaly, use = "na.or.complete"),
            ind_tavg_p = cor.test(value, tavg_anomaly)$p.value,
            ind_tmax_cor = cor(value, tmax_anomaly, use = "na.or.complete"),
            ind_tmax_p = cor.test(value, tmax_anomaly)$p.value,
            ind_tmin_d_cor = cor(value, tmin_detrended, use = "na.or.complete"),
            ind_tmin_d_p = cor.test(value, tmin_detrended)$p.value,
            ind_tavg_d_cor = cor(value, tavg_detrended, use = "na.or.complete"),
            ind_tavg_d_p = cor.test(value, tavg_detrended)$p.value,
            ind_tmax_d_cor = cor(value, tmax_detrended, use = "na.or.complete"),
            ind_tmax_d_p = cor.test(value, tmax_detrended)$p.value,
            n = n())

phase_cor <- phase_cor %>%
  mutate(new_rain_cor = ifelse(ind_rain_p >= .05, 0, ind_rain_cor),
         new_tmin_cor = ifelse(ind_tmin_p >= .05, 0, ind_tmin_cor),
         new_tavg_cor = ifelse(ind_tavg_p >= .05, 0, ind_tavg_cor),
         new_tmax_cor = ifelse(ind_tmax_p >= .05, 0, ind_tmax_cor),
         new_tmin_d_cor = ifelse(ind_tmin_d_p >= .05, 0, ind_tmin_d_cor),
         new_tavg_d_cor = ifelse(ind_tavg_d_p >= .05, 0, ind_tavg_d_cor),
         new_tmax_d_cor = ifelse(ind_tmax_d_p >= .05, 0, ind_tmax_d_cor))



# Rain
lim <-  max(c(abs(min(phase_cor$ind_rain_cor, na.rm = TRUE)),
              abs(max(phase_cor$ind_rain_cor, na.rm = TRUE))))

# All
ggplot(phase_cor, aes(x = index, y = month_of, fill = ind_rain_cor)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and monthly rainfall anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "all_rain.pdf", plot = last_plot(),
       path = "plots/index_anomaly_correlations",
       width = 12, height = 4.5, units = "in")

# p < 0.05
ggplot(phase_cor, aes(x = index, y = month_of, fill = new_rain_cor)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and monthly rainfall anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "sig_rain.pdf", plot = last_plot(),
       path = "plots/index_anomaly_correlations",
       width = 12, height = 4.5, units = "in")



# TMIN
lim <-  max(c(abs(min(phase_cor$ind_tmin_cor, na.rm = TRUE)),
              abs(max(phase_cor$ind_tmin_cor, na.rm = TRUE))))

# All
ggplot(phase_cor, aes(x = index, y = month_of, fill = ind_tmin_cor)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and TMIN anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "all_tmin.pdf", plot = last_plot(),
       path = "plots/index_anomaly_correlations",
       width = 12, height = 4.5, units = "in")

# < 0.05
ggplot(phase_cor, aes(x = index, y = month_of, fill = new_tmin_cor)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and TMIN anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "sig_tmin.pdf", plot = last_plot(),
       path = "plots/index_anomaly_correlations",
       width = 12, height = 4.5, units = "in")

# TAVG
lim <-  max(c(abs(min(phase_cor$ind_tavg_cor, na.rm = TRUE)),
              abs(max(phase_cor$ind_tavg_cor, na.rm = TRUE))))

# All
ggplot(phase_cor, aes(x = index, y = month_of, fill = ind_tavg_cor)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and tavg anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "all_tavg.pdf", plot = last_plot(),
       path = "plots/index_anomaly_correlations",
       width = 12, height = 4.5, units = "in")

# < 0.05
ggplot(phase_cor, aes(x = index, y = month_of, fill = new_tavg_cor)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and tavg anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "sig_tavg.pdf", plot = last_plot(),
       path = "plots/index_anomaly_correlations",
       width = 12, height = 4.5, units = "in")



# TMAX
lim <-  max(c(abs(min(phase_cor$ind_tmax_cor, na.rm = TRUE)),
              abs(max(phase_cor$ind_tmax_cor, na.rm = TRUE))))

# All
ggplot(phase_cor, aes(x = index, y = month_of, fill = ind_tmax_cor)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and tmax anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "all_tmax.pdf", plot = last_plot(),
       path = "plots/index_anomaly_correlations",
       width = 12, height = 4.5, units = "in")

# < 0.05
ggplot(phase_cor, aes(x = index, y = month_of, fill = new_tmax_cor)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and tmax anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "sig_tmax.pdf", plot = last_plot(),
       path = "plots/index_anomaly_correlations",
       width = 12, height = 4.5, units = "in")


# TMIN detrended
lim <-  max(c(abs(min(phase_cor$ind_tmin_d_cor, na.rm = TRUE)),
              abs(max(phase_cor$ind_tmin_d_cor, na.rm = TRUE))))

# All
ggplot(phase_cor, aes(x = index, y = month_of, fill = ind_tmin_d_cor)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and detrended TMIN anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "all_tmin_detrended.pdf", plot = last_plot(),
       path = "plots/index_anomaly_correlations",
       width = 12, height = 4.5, units = "in")

# < 0.05
ggplot(phase_cor, aes(x = index, y = month_of, fill = new_tmin_d_cor)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and detrended TMIN anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "sig_tmin_detrended.pdf", plot = last_plot(),
       path = "plots/index_anomaly_correlations",
       width = 12, height = 4.5, units = "in")

# TAVG detrended
lim <-  max(c(abs(min(phase_cor$ind_tavg_d_cor, na.rm = TRUE)),
              abs(max(phase_cor$ind_tavg_d_cor, na.rm = TRUE))))

# All
ggplot(phase_cor, aes(x = index, y = month_of, fill = ind_tavg_d_cor)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and detrended tavg anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "all_tavg_detrended.pdf", plot = last_plot(),
       path = "plots/index_anomaly_correlations",
       width = 12, height = 4.5, units = "in")

# < 0.05
ggplot(phase_cor, aes(x = index, y = month_of, fill = new_tavg_d_cor)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and detrended tavg anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "sig_tavg_detrended.pdf", plot = last_plot(),
       path = "plots/index_anomaly_correlations",
       width = 12, height = 4.5, units = "in")



# TMAX detrended
lim <-  max(c(abs(min(phase_cor$ind_tmax_d_cor, na.rm = TRUE)),
              abs(max(phase_cor$ind_tmax_d_cor, na.rm = TRUE))))

# All
ggplot(phase_cor, aes(x = index, y = month_of, fill = ind_tmax_d_cor)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and detrended tmax anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "all_tmax_detrended.pdf", plot = last_plot(),
       path = "plots/index_anomaly_correlations",
       width = 12, height = 4.5, units = "in")

# < 0.05
ggplot(phase_cor, aes(x = index, y = month_of, fill = new_tmax_d_cor)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and detrended tmax anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "sig_tmax_detrended.pdf", plot = last_plot(),
       path = "plots/index_anomaly_correlations",
       width = 12, height = 4.5, units = "in")





temp <- phase_cor %>%
  filter(index == "nino3.4") %>%
  select(site, month_of, index, matches("new"), -matches("tmax"),
         -matches("tmin"), -new_tavg_cor) %>%
  gather(var, value, -site, -month_of, -index)

temp <- phase_cor %>%
  filter(index == "nino3.4" & site == "ssr") %>%
  select(site, month_of, index, ind_tavg_d_cor, ind_rain_cor) %>%
  gather(var, value, -site, -month_of, -index)

old_vars <- levels(factor(temp$var))

temp$var <- mapvalues(temp$var, from = old_vars, to = c("Rainfall", "Mean Temperature"))
temp$site <- revalue(temp$site, site_map)

lim <-  max(c(abs(min(temp$value, na.rm = TRUE)),
              abs(max(temp$value, na.rm = TRUE))))

ggplot(temp, aes(y = var, x = month_of, fill = value)) +
  geom_tile(size = 0.1, color = "black") +
  coord_equal() +
  facet_wrap(~ site, ncol = 4) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_fc() +
  labs(y = "", x = "") +
  theme(strip.background = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))


# ---- var_pairwise_plots -------------------------------------------------

temp <- climates_combined %>%
  filter(site == "ssr") %>%
  select(site, year_of, month_of, rain_monthly_mm, rain_anomaly, tavg_detrended, nino3.4)

lim <-  max(abs(temp$tavg_detrended), na.rm = TRUE)

p1 <- ggplot(temp, aes(x = nino3.4, y = tavg_detrended, color = tavg_detrended)) +
  geom_point() +
  facet_wrap(~month_of, ncol = 3) +
  scale_color_gradientn(colours = rev(brewer.pal(11, "RdYlBu")),
                        limits = c(-lim, lim),
                        trans = sqrt_sign_trans(),
                        guide = FALSE) +
  theme_dark() +
  stat_smooth(method = "lm", se = FALSE, color = "white", size = 0.75) +
  labs(x = "ENSO Conditions", y = "Temperature Anomaly", title = "ENSO vs Temperature")

lim <-  max(abs(temp$rain_anomaly), na.rm = TRUE)

p2 <- ggplot(temp, aes(x = nino3.4, y = rain_anomaly, color = rain_anomaly)) +
  geom_point() +
  facet_wrap(~month_of, ncol = 3) +
  scale_color_gradientn(colours = brewer.pal(11, "BrBG"),
                        limits = c(-lim, lim),
                        trans = sqrt_sign_trans(),
                        guide = FALSE) +
  theme_dark() +
  stat_smooth(method = "lm", se = FALSE, color = "white", size = 0.75) +
  labs(x = "ENSO Conditions", y = "Rain Anomaly", title = "ENSO vs Rainfall")

cowplot::plot_grid(p1, p2, nrow = 1, align = "hv", scale = 0.95)
