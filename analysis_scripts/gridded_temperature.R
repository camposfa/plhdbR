library(plyr)
library(ncdf4)
library(lubridate)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(grid)


# ---- mean_temp ----------------------------------------------------------

# GHCN_CAMS Gridded 2m Temperature (Land)
# 0.5 degree latitude x 0.5 degree longitude global grid
# ftp://ftp.cdc.noaa.gov/Datasets/ghcncams/air.mon.mean.nc

t <- nc_open("data/grids/GHCN_CAMS/air.mon.mean.nc")

lon <- ncvar_get(t, varid = "lon")
lat <- ncvar_get(t, varid = "lat")
time <- ncvar_get(t, varid = "time")
at <- ncvar_get(t, varid = "air")

inds_lon <- (1:dim(lon))
inds_lat <- (1:dim(lat))
inds_time <- (1:dim(time))

site_coords <- read.csv("data/site_coords.csv")
names(site_coords)[2] <- "long_name"
site_list <- c("rppn-fma", "amboseli", "kakamega", "gombe", "karisoke", "beza", "ssr")
site_coords$site <- site_list
site_coords$site <- factor(site_coords$site, levels = site_list)
site_coords <- mutate(site_coords, lat_ind = 0, lon_ind = 0)

# Convert longitude to degrees east (0 to 360)
site_coords[site_coords$Long < 0, ]$Long <- 360 + site_coords[site_coords$Long < 0, ]$Long

for(i in 1:nrow(site_coords)){
  site_coords[i, ]$lat_ind <- which.min(abs(site_coords[i, ]$Lat - lat))
  site_coords[i, ]$lon_ind <- which.min(abs(site_coords[i, ]$Long - lon))
}

at_sites_f <- list()

# Full data set (in "t")
for(i in 1:nrow(site_coords)){
  temp <- ncvar_get(t, varid = "air",
                    start = c(site_coords[i, ]$lon_ind,
                              site_coords[i, ]$lat_ind,
                              1),
                    count = c(1, 1, -1))
  at_sites_f[[i]] <- tbl_df(data.frame(hrs = time,
                                       air_temp = temp,
                                       site = site_coords[i, ]$site))
}

at_sites_f <- bind_rows(at_sites_f)

at <- at_sites_f %>%
  mutate(date_of = ymd_hms("1900-01-01 00:00:00") + hours(hrs),
         year_of = year(date_of),
         month_of = month(date_of),
         air_temp = as.numeric(air_temp) - 273.15) %>%
  select(site, date_of, year_of, month_of, air_temp)

at$month_of <- factor(at$month_of, labels = month.abb)

# Circular
ggplot(at, aes(x = factor(month_of), y = year_of, fill = air_temp)) +
  geom_tile() +
  facet_wrap(~site, ncol = 2) +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd")) +
  theme_classic() +
  scale_y_continuous(limits = c(1920, 2016)) +
  coord_polar()

# Rectangular
ggplot(at, aes(x = factor(month_of), y = year_of, fill = air_temp)) +
  geom_tile() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"), name = "Mean Temperature") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(2, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(breaks = seq(1945, 2015, by = 5)) +
  labs(x = "Month", y = "Year")


# ---- berkeley_tavg_anom -------------------------------------------------

# Average Temperature Anomaly (relative to 1951-1980 mean)
# 1 degree latitude x 1 degree longitude global grid
# http://berkeleyearth.lbl.gov/auto/Global/Gridded/Land_and_Ocean_LatLong1.nc
# t <- nc_open("data/grids/Berkeley/Land_and_Ocean_LatLong1.nc")
ta <- nc_open("data/grids/Berkeley/Complete_TAVG_LatLong1.nc")
tx <- nc_open("data/grids/Berkeley/Complete_TMAX_LatLong1.nc")
tn <- nc_open("data/grids/Berkeley/Complete_TMIN_LatLong1.nc")

lon <- ncvar_get(ta, varid = "longitude")
lat <- ncvar_get(ta, varid = "latitude")
time_a <- ncvar_get(ta, varid = "time")
time_x <- ncvar_get(tx, varid = "time")
time_n <- ncvar_get(tn, varid = "time")
# at <- ncvar_get(t, varid = "temperature")

inds_lon <- (1:dim(lon))
inds_lat <- (1:dim(lat))

site_coords <- read.csv("data/site_coords.csv")
names(site_coords)[2] <- "long_name"
site_list <- c("rppn-fma", "amboseli", "kakamega", "gombe", "karisoke", "beza", "ssr")
site_coords$site <- site_list
site_coords$site <- factor(site_coords$site, levels = site_list)
site_coords <- mutate(site_coords, lat_ind = 0, lon_ind = 0)

# Convert longitude to degrees east (0 to 360)
# site_coords[site_coords$Long < 0, ]$Long <- 360 + site_coords[site_coords$Long < 0, ]$Long

for(i in 1:nrow(site_coords)){
  site_coords[i, ]$lat_ind <- which.min(abs(site_coords[i, ]$Lat - lat))
  site_coords[i, ]$lon_ind <- which.min(abs(site_coords[i, ]$Lon - lon))
}

at_sites_f <- list()

# Full data set (in "t")
for(i in 1:nrow(site_coords)){
  ta_anom <- ncvar_get(ta, varid = "temperature",
                       start = c(site_coords[i, ]$lon_ind,
                                 site_coords[i, ]$lat_ind,
                                 1),
                       count = c(1, 1, -1))

  ta_avg <- data.frame(month_of = 1:12,
                       t_monthly = ncvar_get(ta, varid = "climatology",
                                         start = c(site_coords[i, ]$lon_ind,
                                                   site_coords[i, ]$lat_ind,
                                                   1),
                                         count = c(1, 1, -1)))

  ta_df <- tbl_df(data.frame(date_of = date_decimal(time_a),
                             t_anom = ta_anom,
                             site = site_coords[i, ]$site))

  ta_df$month_of <- month(ta_df$date_of)
  ta_df <- suppressMessages(inner_join(ta_df, ta_avg))
  ta_df$var <- "t_avg"


  tx_anom <- ncvar_get(tx, varid = "temperature",
                       start = c(site_coords[i, ]$lon_ind,
                                 site_coords[i, ]$lat_ind,
                                 1),
                       count = c(1, 1, -1))

  tx_avg <- data.frame(month_of = 1:12,
                       t_monthly = ncvar_get(tx, varid = "climatology",
                                         start = c(site_coords[i, ]$lon_ind,
                                                   site_coords[i, ]$lat_ind,
                                                   1),
                                         count = c(1, 1, -1)))

  tx_df <- tbl_df(data.frame(date_of = date_decimal(time_x),
                             t_anom = tx_anom,
                             site = site_coords[i, ]$site))

  tx_df$month_of <- month(tx_df$date_of)
  tx_df <- suppressMessages(inner_join(tx_df, tx_avg))
  tx_df$var <- "t_max"


  tn_anom <- ncvar_get(tn, varid = "temperature",
                       start = c(site_coords[i, ]$lon_ind,
                                 site_coords[i, ]$lat_ind,
                                 1),
                       count = c(1, 1, -1))

  tn_avg <- data.frame(month_of = 1:12,
                       t_monthly = ncvar_get(tn, varid = "climatology",
                                         start = c(site_coords[i, ]$lon_ind,
                                                   site_coords[i, ]$lat_ind,
                                                   1),
                                         count = c(1, 1, -1)))

  tn_df <- tbl_df(data.frame(date_of = date_decimal(time_n),
                             t_anom = tn_anom,
                             site = site_coords[i, ]$site))

  tn_df$month_of <- month(tn_df$date_of)
  tn_df <- suppressMessages(inner_join(tn_df, tn_avg))
  tn_df$var <- "t_min"

  temp <- bind_rows(ta_df, tx_df, tn_df)

  at_sites_f[[i]] <- temp
}

at_sites_f <- bind_rows(at_sites_f)

at <- at_sites_f %>%
  mutate(year_of = year(date_of),
         t_anomaly = as.numeric(t_anom),
         t_monthly = t_monthly + t_anom,
         var = factor(var, levels = c("t_max", "t_avg", "t_min"))) %>%
  select(site, date_of, year_of, month_of, t_anomaly, t_monthly, var)

at$month_of <- factor(at$month_of, labels = month.abb)


# ---- plot_all_variables -------------------------------------------------

ggplot(at, aes(x = month_of, y = year_of, fill = t_anomaly)) +
  geom_tile() +
  facet_grid(var ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdYlBu")),
                       name = "Temperature Anomaly",
                       limits = c(-3.25, 3.25)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(2, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1945, 2016), breaks = seq(1945, 2015, by = 5)) +
  labs(x = "Month", y = "Year")

ggplot(at, aes(x = month_of, y = year_of, fill = t_monthly)) +
  geom_tile() +
  facet_grid(var ~ site) +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"),
                       name = "Temperature") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(2, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1945, 2016), breaks = seq(1945, 2015, by = 5)) +
  labs(x = "Month", y = "Year")


# --- t_avg_only ----------------------------------------------------------

temp <- filter(at, var == "t_avg")

ggplot(temp, aes(x = month_of, y = year_of, fill = t_anomaly)) +
  geom_tile() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdYlBu")),
                       name = "Temperature Anomaly",
                       limits = c(-2.74, 2.74)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(2, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1945, 2016), breaks = seq(1945, 2015, by = 5)) +
  labs(x = "Month", y = "Year")

ggplot(temp, aes(x = month_of, y = year_of, fill = t_monthly)) +
  geom_tile() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"),
                       name = "Temperature") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(2, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1945, 2016), breaks = seq(1945, 2015, by = 5)) +
  labs(x = "Month", y = "Year")
