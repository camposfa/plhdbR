Sys.setenv(TZ = 'UTC')
list.of.packages <- list("plyr", "reshape2", "ncdf4", "lubridate", "ggplot2",
                         "RColorBrewer", "grid", "stringr", "scales", "tidyr",
                         "grid", "dplyr", "plhdbR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(unlist(new.packages))
lapply(list.of.packages, require, character.only = T)

load(".RData")
`%ni%` = Negate(`%in%`)


# ---- scale_transform_functions ------------------------------------------

cubroot_trans <-  function(){
  trans_new('cubroot', transform= function(x) x^(1/3), inverse = function(x) x^3 )
}

sqrt_sign_trans <-  function(){
  trans_new('sqrt_sign', transform= function(x) sign(x) * sqrt(abs(x)), inverse = function(x) sign(x) * x^2)
}


# ---- prep ---------------------------------------------------------------

f <- "data/biography_2015_03_17.csv"
lh <- read_bio_table(f)


f <- "data/fertility_2015_03_17.csv"
fert <- read_fert_table(f)

# Fix a probable error
lh[lh$Animal.Id == "247" & lh$Study.Id == "beza", ]$Entry.Date <- ymd("1984-07-15")

study_durations <- lh %>%
  group_by(Study.Id) %>%
  summarise(min_entry = min(Entry.Date),
            max_depart = max(Depart.Date),
            n_rec = n()) %>%
  mutate(dur = as.duration(max_depart - min_entry))



# ==== GRIDDED_RAINFALL_DATA ==============================================

# gridded_gpcc ------------------------------------------------------------

# Downloaded from http://www.esrl.noaa.gov/psd/data/gridded/data.gpcc.html

# GPCC total monthly precipitation
# 0.5 degree latitude x 0.5 degree longitude global grid
# ftp://ftp.cdc.noaa.gov/Datasets/gpcc/full_v6/precip.mon.total.v6.nc
t <- nc_open("data/grids/gpcc/precip.mon.total.v6.nc")

longitude <- ncvar_get(t, varid = "lon")
latitude <- ncvar_get(t, varid = "lat")

inds_lon <- (1:dim(longitude))
inds_lat <- (1:dim(latitude))

site_coords <- read.csv("data/site_coords.csv")
names(site_coords)[2] <- "long_name"
site_list <- c("rppn-fma", "amboseli", "kakamega", "gombe", "karisoke", "beza", "ssr")
site_coords$site <- site_list
site_coords$site <- factor(site_coords$site, levels = site_list)
site_coords <- mutate(site_coords, lat_ind = 0, lon_ind = 0)

# Convert longitude to degrees east (0 to 360)
site_coords[site_coords$Long < 0, ]$Long <- 360 + site_coords[site_coords$Long < 0, ]$Long

for(i in 1:nrow(site_coords)){
  site_coords[i, ]$lat_ind <- which.min(abs(site_coords[i, ]$Lat - latitude))
  site_coords[i, ]$lon_ind <- which.min(abs(site_coords[i, ]$Lon - longitude))
}

precip_sites_f <- list()

# Full data set (in "t")
for(i in 1:nrow(site_coords)){
  temp <- ncvar_get(t, varid = "precip",
                    start = c(site_coords[i, ]$lon_ind,
                              site_coords[i, ]$lat_ind,
                              1),
                    count = c(1, 1, -1))
  precip_sites_f[[i]] <- tbl_df(data.frame(days = time,
                                           precip = temp,
                                           site = site_coords[i, ]$site))
}

precip_sites_f <- bind_rows(precip_sites_f)

precip_sites_f <- precip_sites_f %>%
  mutate(date_of = ymd("1800-01-01") + days(days),
         year_of = year(date_of),
         month_of = month(date_of),
         precip = as.numeric(precip)) %>%
  select(site, date_of, year_of, month_of, precip)


# ---- gpcc_monitoring_data -----------------------------------------------

# Monitoring data set
# GPCC total monthly precipitation montoring data set
# 1 degree latitude x 1 degree longitude global grid
# ftp://ftp.cdc.noaa.gov/Datasets/gpcc/monitor/precip.monitor.mon.total.1x1.v4.nc
m <- nc_open("data/grids/gpcc/precip.monitor.mon.total.1x1.v4.nc")

longitude <- ncvar_get(m, varid = "lon")
latitude <- ncvar_get(m, varid = "lat")

inds_lon <- (1:dim(longitude))
inds_lat <- (1:dim(latitude))

site_coords <- read.csv("data/site_coords.csv")
names(site_coords)[2] <- "long_name"
site_list <- c("rppn-fma", "amboseli", "kakamega", "gombe", "karisoke", "beza", "ssr")
site_coords$site <- site_list
site_coords$site <- factor(site_coords$site, levels = site_list)
site_coords <- mutate(site_coords, lat_ind = 0, lon_ind = 0)

# Convert longitude to degrees east (0 to 360)
site_coords[site_coords$Long < 0, ]$Long <- 360 + site_coords[site_coords$Long < 0, ]$Long

precip_sites_m <- list()

for(i in 1:nrow(site_coords)){
  site_coords[i, ]$lat_ind <- which.min(abs(site_coords[i, ]$Lat - latitude))
  site_coords[i, ]$lon_ind <- which.min(abs(site_coords[i, ]$Lon - longitude))
}

precip_sites_m <- list()

for(i in 1:nrow(site_coords)){
  temp <- ncvar_get(m, varid = "precip",
                    start = c(site_coords[i, ]$lon_ind,
                              site_coords[i, ]$lat_ind,
                              1),
                    count = c(1, 1, -1))
  precip_sites_m[[i]] <- tbl_df(data.frame(days = time,
                                           precip = temp,
                                           site = site_coords[i, ]$site))
}

precip_sites_m <- bind_rows(precip_sites_m)

precip_sites_m <- precip_sites_m %>%
  mutate(date_of = ymd("1800-01-01") + days(days),
         year_of = year(date_of),
         month_of = month(date_of),
         precip = as.numeric(precip)) %>%
  filter(year_of > 2010) %>%
  select(site, date_of, year_of, month_of, precip)

precip_sites <- bind_rows(precip_sites_m, precip_sites_f)

precip_sites <- precip_sites %>%
  arrange(site, date_of)

precip_sites$month_of <- factor(precip_sites$month_of, labels = month.abb)

ggplot(precip_sites, aes(x = factor(month_of), y = year_of, fill = precip)) +
  geom_tile() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                       name = "Total Precipitation",
                       limits = c(0, 1000),
                       trans = "cubroot") +
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


# ---- gcc_long_term_mean -------------------------------------------------

# ftp://ftp.cdc.noaa.gov/Datasets/gpcc/full_v6/precip.mon.1981-2010.ltm.v6.nc
ltm <- nc_open("data/grids/gpcc/precip.mon.1981-2010.ltm.v6.nc")

longitude <- ncvar_get(ltm, varid = "lon")
latitude <- ncvar_get(ltm, varid = "lat")

inds_lon <- (1:dim(longitude))
inds_lat <- (1:dim(latitude))

site_coords <- read.csv("data/site_coords.csv")
names(site_coords)[2] <- "long_name"
site_list <- c("rppn-fma", "amboseli", "kakamega", "gombe", "karisoke", "beza", "ssr")
site_coords$site <- site_list
site_coords$site <- factor(site_coords$site, levels = site_list)
site_coords <- mutate(site_coords, lat_ind = 0, lon_ind = 0)

# Convert longitude to degrees east (0 to 360)
site_coords[site_coords$Long < 0, ]$Long <- 360 + site_coords[site_coords$Long < 0, ]$Long

for(i in 1:nrow(site_coords)){
  site_coords[i, ]$lat_ind <- which.min(abs(site_coords[i, ]$Lat - latitude))
  site_coords[i, ]$lon_ind <- which.min(abs(site_coords[i, ]$Lon - longitude))
}

precip_ltm <- list()

for(i in 1:nrow(site_coords)){
  temp <- ncvar_get(ltm, varid = "precip",
                    start = c(site_coords[i, ]$lon_ind,
                              site_coords[i, ]$lat_ind,
                              1),
                    count = c(1, 1, -1))
  precip_ltm[[i]] <- tbl_df(data.frame(month_of = 1:12,
                                       precip_ltm = temp,
                                       site = site_coords[i, ]$site))
}

precip_ltm <- bind_rows(precip_ltm)

precip_ltm <- precip_ltm %>%
  mutate(precip_ltm = as.numeric(precip_ltm)) %>%
  select(site, month_of, precip_ltm)

precip_ltm$month_of <- factor(precip_ltm$month_of, labels = month.abb)

precip_combined <- inner_join(precip_sites, precip_ltm,
                              by = c("site" = "site", "month_of" = "month_of"))

precip_combined <- precip_combined %>%
  rename(ltm = precip_ltm) %>%
  mutate(precip_anom = precip - ltm)


# Plot of GPCC rainfall anomalies

ggplot(precip_combined, aes(x = factor(month_of), y = year_of, fill = precip_anom)) +
  geom_tile() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       name = "Precipitation Anomaly (mm)",
                       limits = c(-613, 613),
                       trans = "sqrt_sign") +
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



# ==== GRIDDED_TEMPERATURE_DATA ===========================================

# ghcn_cams_tavg ----------------------------------------------------------

# GHCN_CAMS Gridded 2m Temperature (Land)
# 0.5 degree latitude x 0.5 degree longitude global grid
# ftp://ftp.cdc.noaa.gov/Datasets/ghcncams/air.mon.mean.nc

t <- nc_open("data/grids/GHCN_CAMS/air.mon.mean.nc")

lon <- ncvar_get(t, varid = "lon")
lat <- ncvar_get(t, varid = "lat")

inds_lon <- (1:dim(lon))
inds_lat <- (1:dim(lat))

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

# Plot data
ggplot(at, aes(x = factor(month_of), y = year_of, fill = air_temp)) +
  geom_tile() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"), name = "Temperature") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(4, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(breaks = seq(1945, 2015, by = 5)) +
  labs(x = "Month", y = "Year", title = "GHCN CAMS Temperature Data\n")


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

inds_lon <- (1:dim(lon))
inds_lat <- (1:dim(lat))

site_coords <- read.csv("data/site_coords.csv")
names(site_coords)[2] <- "long_name"
site_list <- c("rppn-fma", "amboseli", "kakamega", "gombe", "karisoke", "beza", "ssr")
site_coords$site <- site_list
site_coords$site <- factor(site_coords$site, levels = site_list)
site_coords <- mutate(site_coords, lat_ind = 0, lon_ind = 0)

for(i in 1:nrow(site_coords)){
  site_coords[i, ]$lat_ind <- which.min(abs(site_coords[i, ]$Lat - lat))
  site_coords[i, ]$lon_ind <- which.min(abs(site_coords[i, ]$Lon - lon))
}

at_sites_f <- list()

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


# ---- plot_berkeley_data -------------------------------------------------

# All variables
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
        legend.key.width = unit(4, "cm"),
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
        legend.key.width = unit(4, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1945, 2016), breaks = seq(1945, 2015, by = 5)) +
  labs(x = "Month", y = "Year")


# TAVG only

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
        legend.key.width = unit(4, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1945, 2016), breaks = seq(1945, 2015, by = 5)) +
  labs(x = "Month", y = "Year")

ggplot(temp, aes(x = month_of, y = year_of, fill = t_monthly)) +
  geom_tile() +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"),
                       # guide = FALSE,
                       name = "Temperature") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(4, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1945, 2016), breaks = seq(1945, 2015, by = 5)) +
  labs(x = "Month", y = "Year", title = "Berkeley Earth Temperature Data\n")



# ==== RAIN_GAUGE_DATA====================================================

# ---- read_rain_data -----------------------------------------------------

amboseli <- tbl_df(read.csv("data/rain_csv/amboseli.csv"))
names(amboseli) <- c("date_time", "date_of", "rainfall")
amboseli$rainfall <- str_replace_all(amboseli$rainfall, fixed("\\N"), fixed(""))
amboseli$rainfall <- as.numeric(amboseli$rainfall)
amboseli <- amboseli %>%
  mutate(date_of = parse_date_time(date_of, "%d/%m/%Y"),
         year_of = year(date_of),
         type = "daily",
         site = "amboseli") %>%
  select(year_of, rainfall, date_of, type, site)

beza <- tbl_df(read.csv("data/rain_csv/beza.csv"))
names(beza) <- c("year_of", "rainfall")
beza <- beza %>%
  mutate(date_of = ymd(paste(year_of, "-12-31", sep = "")),
         type = "yearly",
         site = "beza")

gombe <- tbl_df(read.csv("data/rain_csv/gombe.csv"))
names(gombe) <- c("year_of", "rainfall")
gombe <- gombe %>%
  filter(!is.na(rainfall)) %>%
  mutate(date_of = ymd(paste(year_of, "-12-31", sep = "")),
         type = "yearly",
         site = "gombe")

kakamega <- read.csv("data/rain_csv/kakamega.csv")
kakamega <- tbl_df(melt(kakamega))
kakamega$variable <- as.integer(substr(kakamega$variable,
                                       start = 2, stop = 5))
names(kakamega) <- c("month_of", "year_of", "rainfall")
kakamega <- kakamega %>%
  mutate(date_of = parse_date_time(paste("01", month_of, year_of, sep = "-"),
                                   "%d-%b-%y") + months(1) - days(1),
         type = "monthly",
         site = "kakamega") %>%
  arrange(date_of) %>%
  select(year_of, rainfall, date_of, type, site)

karisoke <- read.csv("data/rain_csv/karisoke.csv")
karisoke <- tbl_df(melt(karisoke))
karisoke$variable <- as.integer(substr(karisoke$variable,
                                       start = 2,
                                       stop = nchar(as.character(karisoke$variable))))
names(karisoke) <- c("month_of", "day_of", "rainfall")
karisoke <- karisoke %>%
  filter(!is.na(rainfall)) %>%
  mutate(date_of = ymd(paste(month_of, day_of, sep = "-")),
         type = "daily",
         year_of = year(date_of),
         site = "karisoke") %>%
  arrange(date_of) %>%
  select(year_of, rainfall, date_of, type, site)

karisoke_monthly <- karisoke %>%
  mutate(month_of = month(date_of)) %>%
  group_by(year_of, month_of, type, site) %>%
  summarise(rainfall = sum(rainfall),
            n = n()) %>%
  ungroup() %>%
  mutate(date_of = ymd(paste(year_of, month_of, "01", sep = "-")) + months(1) - days(1),
         # date_of = paste("01", month_of, year_of, sep = "-"),
         type = "monthly") %>%
  select(year_of, rainfall, date_of, type, site)

karisoke_air <- read.csv("data/rain_csv/karisoke_airstrip.csv")
karisoke_air <- tbl_df(melt(karisoke_air, id.vars = "Year"))
names(karisoke_air) <- c("year_of", "month_of", "rainfall")
karisoke_air <- karisoke_air %>%
  filter(!is.na(rainfall) & year_of >= 1968) %>%
  mutate(date_of = parse_date_time(paste("01", month_of, year_of, sep = "-"),
                                   "%d-%b-%y") + months(1) - days(1),
         type = "monthly",
         site = "karisoke_airstrip") %>%
  arrange(date_of) %>%
  select(year_of, rainfall, date_of, type, site)

rppn <- read.csv("data/rain_csv/rppn.csv")
rppn <- melt(rppn, id.vars = "Year")
names(rppn) <- c("year_of", "month_of", "rainfall")
rppn <- rppn %>%
  filter(!is.na(rainfall)) %>%
  mutate(date_of = parse_date_time(paste("01", month_of, year_of, sep = "-"),
                                   "%d-%b-%y") + months(1) - days(1),
         type = "monthly",
         site = "rppn-fma") %>%
  arrange(date_of) %>%
  select(year_of, rainfall, date_of, type, site)


# ---- load_pace_data -----------------------------------------------------

# Use PACE DB for most recent data from SSR
# Run the following two lines in terminal:
system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')

pace_db <- src_mysql(group = "PACE", user = "camposf", password = NULL, dbname = "monkey")

# Pull latest weather
# Probably a good idea to check data to make sure it's still valid
ssr <- collect(tbl(pace_db, "tblWeather"))
ssr <- select(ssr, DateOf, Rainfall)
names(ssr) <- c("date_of", "rainfall")

ssr <- ssr %>%
  filter(!is.na(rainfall)) %>%
  mutate(type = "daily",
         year_of = year(date_of),
         date_of = ymd(date_of),
         site = "ssr") %>%
  arrange(date_of) %>%
  select(year_of, rainfall, date_of, type, site)

# If no access to PACE DB, use the csv file:
# ssr <- read.csv("data/rain_csv/ssr.csv") %>%
#   tbl_df() %>%
#   select(-X) %>%
#   mutate(date_of = ymd(date_of))


# ---- gpcp_satellite_station_data ----------------------------------------

d <- "data/satellite_data/GPCP/"
f <- list.files(d)
f <- f[str_detect(f, "csv")]
s <- list()

# Get site names from files
sites <- unlist(str_split(f, fixed(".")))[seq(1, by = 2, length = length(f))]

# Read in csv files for each site
for(i in 1:length(f))
{
  s[[i]] <- tbl_df(read.csv(paste(d, f[i], sep = "")))
  s[[i]]$site <- sites[i]
}

sat_gpcp <- s %>%
  bind_rows() %>%
  mutate(date_of = parse_date_time(paste(YEAR, DOY, sep = "-"), "%y-%j")) %>%
  filter(RAIN != "-") %>%
  select(date_of, RAIN, site)

names(sat_gpcp)[2] <- "rainfall"

sat_gpcp <- sat_gpcp %>%
  filter(!is.na(rainfall)) %>%
  group_by(site) %>%
  mutate(year_of = year(date_of),
         type = "daily",
         rainfall = as.numeric(as.character(rainfall))) %>%
  arrange(date_of) %>%
  select(year_of, rainfall, date_of, type, site)


# ---- trmm_satellite_data ------------------------------------------------

d <- "data/satellite_data/TRMM_daily/"
f <- list.files(d)
f <- f[str_detect(f, "txt")]
s <- list()

# Get site names from files
sites <- unlist(str_split(f, fixed(".")))[seq(1, by = 2, length = length(f))]

# Read in files for each site
for(i in 1:length(f))
{
  s[[i]] <- tbl_df(read.table(paste(d, f[i], sep = ""), skip = 4, header = TRUE))
  s[[i]]$site <- sites[i]
}

sat_trmm <- s %>%
  bind_rows() %>%
  mutate(date_of = parse_date_time(Time.year.month.day., "%y:%m:%d")) %>%
  filter(AccRain != "-9999") %>%
  select(date_of, AccRain, site)

names(sat_trmm)[2] <- c("rainfall")

sat_trmm <- sat_trmm %>%
  filter(!is.na(rainfall)) %>%
  group_by(site) %>%
  mutate(year_of = year(date_of),
         type = "daily",
         rainfall = as.numeric(as.character(rainfall))) %>%
  arrange(date_of) %>%
  select(year_of, rainfall, date_of, type, site)


# ---- combine_site_data --------------------------------------------------

rain_sites <- bind_rows(amboseli, kakamega, karisoke,
                        karisoke_air, rppn, ssr)
rain_sites$data_source <- "rain_gauge"
rain_sites[rain_sites$site == "karisoke_airstrip", ]$data_source <- "nearby_station"
rain_sites[rain_sites$site == "karisoke_airstrip", ]$site <- "karisoke"

rain_sat_gpcp <- bind_rows(sat_gpcp)
rain_sat_gpcp$data_source <- "satellite_gpcp"

rain_sat_trmm <- bind_rows(sat_trmm)
rain_sat_trmm$data_source <- "satellite_trmm"

# Get precip_sites from gridded_rainfall.R
rain_gpcc <- precip_sites %>%
  rename(rainfall = precip) %>%
  select(year_of, rainfall, date_of, site)

rain_gpcc$type <- "monthly"
rain_gpcc$data_source <- "gpcc"

rain <- bind_rows(rain_sites, rain_sat_gpcp, rain_sat_trmm, rain_gpcc)

rain$rainfall <- round(rain$rainfall, digits = 2)
names(rain)[4] <- "recording_interval"

rain_monthly <- rain %>%
  filter(!is.na(rainfall)) %>%
  mutate(month_of = month(date_of)) %>%
  group_by(site, year_of, month_of, recording_interval, data_source) %>%
  summarise(rain_monthly_mm = sum(rainfall, na.rm = TRUE),
            n_measurements = n()) %>%
  arrange(site, year_of, month_of) %>%
  select(site, year_of, month_of, recording_interval, n_measurements, rain_monthly_mm, data_source)

rain_monthly$is_complete <- TRUE
rain_monthly[rain_monthly$recording_interval == "daily" & rain_monthly$n_measurements < 20, ]$is_complete <- FALSE

rain_monthly$site <- factor(rain_monthly$site,
                            levels = c("rppn-fma", "amboseli", "kakamega",
                                       "gombe", "karisoke", "karisoke_airstrip",
                                       "beza", "ssr"))

rain_monthly <- inner_join(rain_monthly, study_durations, by = c("site" = "Study.Id"))


# ---- select_final_rainfall_data -----------------------------------------

rain_monthly <- rain_monthly %>%
  mutate(date_of = ymd(paste(year_of, month_of, "01", sep = "-")),
         priority = ifelse(data_source == "rain_gauge", 1,
                           ifelse(data_source == "nearby_station", 2,
                                  ifelse(data_source == "satellite_trmm", 3,
                                         ifelse(data_source == "satellite_gpcp", 5,
                                                4))))) %>%
  filter(date_of > min_entry - years(5))

rain_selected <- rain_monthly %>%
  ungroup() %>%
  filter(is_complete == TRUE) %>%
  group_by(site, year_of, month_of) %>%
  arrange(priority) %>%
  summarise(rain_monthly_mm = first(rain_monthly_mm),
            data_source = first(data_source),
            recording_interval = first(recording_interval),
            n_measurements = first(n_measurements),
            date_of = min(date_of))

rain_ltm <- rain_selected %>%
  ungroup() %>%
  group_by(site, month_of) %>%
  summarise(rain_ltm = mean(rain_monthly_mm))

rain_selected <- inner_join(rain_selected, rain_ltm) %>%
  mutate(rain_anomaly = rain_monthly_mm - rain_ltm)


# ---- plot_rainfall_summaries --------------------------------------------

rain_selected$site <- factor(rain_selected$site,
                             levels = c("rppn-fma", "amboseli", "kakamega",
                                        "gombe", "karisoke",
                                        "beza", "ssr"))

study_durations$site <- study_durations$Study.Id

rain_selected$month_of <- factor(rain_selected$month_of, labels = month.abb)

rain_selected$data_source <- factor(rain_selected$data_source,
                                    levels = c("rain_gauge", "nearby_station",
                                               "satellite_trmm", "satellite_gpcp",
                                               "gpcc"))

# Accumulated monthly rainfall
ggplot() +
  geom_tile(data = rain_selected,
            aes(x = month_of, y = year_of,
                fill = rain_monthly_mm)) +
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                       name = "Rainfall Total (mm)",
                       guide = FALSE,
                       trans = "cubroot") +
  facet_grid(. ~ site) +
  theme_bw() +
  labs(x = "Month", y = "Year") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(4, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1955, 2016), breaks = seq(1955, 2015, by = 5))

# Data sources
ggplot() +
  geom_tile(data = rain_selected,
            aes(x = month_of, y = year_of,
                fill = data_source),
            colour = "gray70") +
  facet_grid(. ~ site) +
  theme_bw() +
  labs(x = "Month", y = "Year") +
  scale_fill_brewer(name = "Data Source",
                    guide = FALSE,
                    palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(2, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1955, 2016), breaks = seq(1955, 2015, by = 5))

# Rainfall anomalies
ggplot() +
  geom_tile(data = rain_selected,
            aes(x = month_of, y = year_of,
                fill = rain_anomaly),
            colour = "gray70") +
  facet_grid(. ~ site) +
  theme_bw() +
  labs(x = "Month", y = "Year") +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       name = "Rainfall Anomaly (mm)",
                       trans = "sqrt_sign",
                       # guide = FALSE,
                       limits = c(-1308, 1308)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(4, "cm"),
        panel.margin = unit(1, "lines")) +
  scale_y_continuous(limits = c(1955, 2016), breaks = seq(1955, 2015, by = 5))


# ---- write_rainfall_selected_data ---------------------------------------

# Write to csv file for later convenience
# write.csv(rain_selected, "data/rain_csv/rain_selected.csv", row.names = FALSE)


# ---- rainfall_source_comparisons ----------------------------------------

rain_monthly$site <- factor(rain_monthly$site,
                            levels = c("rppn-fma", "amboseli", "kakamega",
                                       "gombe", "karisoke", "karisoke_airstrip",
                                       "beza", "ssr"))

source_comp <- rain_monthly %>%
  ungroup %>%
  dcast(site + date_of ~ data_source, value.var = "rain_monthly_mm")

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



# ==== OSCILLATION_ANALYSIS ===============================================

# ---- load_indices -------------------------------------------------------

ind <- load_climate_index(c("pdo", "dmi", "oni", "amo", "nao", "sam", "ao"))

ind_df <- bind_rows(ind)

ind_df <- filter(ind_df, date_of > ymd("1945-01-01"))

ind_df$index <- factor(ind_df$index, levels = c("amo", "nao", "oni", "pdo",
                                                "dmi", "sam", "ao"))

ind_neg <- ind_df
ind_neg[ind_neg$value > 0, ]$value <- 0

ind_pos <- ind_df
ind_pos[ind_pos$value < 0, ]$value <- 0

# Plot climate oscillators
ggplot() +
  geom_area(data = ind_neg,
            aes(x = date_of, y = value), fill = "#009ACD") +
  geom_area(data = ind_pos,
            aes(x = date_of, y = value), fill = "#FF8100") +
  facet_grid(index ~ ., scales = "free_y") +
  geom_hline(yintercept = 0) +
  scale_x_datetime(labels = date_format("%Y"),
                   limits = c(ymd("1944-01-01"), ymd("2016-01-01")),
                   minor_breaks = date_breaks("1 year"),
                   breaks = date_breaks("5 years")) +

  theme_bw() +
  theme(strip.background = element_blank()) +
  labs(x = "Year", y = "Value")


# ---- rainfall_stl -------------------------------------------------------

# rain_selected from "standardize_rainfall.R"
rain_site <- dlply(rain_selected, .(site))

rain_sites_ts <- list()

for(j in 1:length(rain_site)){
  rain_temp <- rain_site[[j]]
  temp <- zoo(rain_temp$rain_anomaly, rain_temp$date_of, frequency = 12)
  rain_ts <- ts(coredata(temp), freq = frequency(temp),
                start = c(year(start(temp)), month(start(temp))),
                end = c(year(end(temp)), month(end(temp))))
  stl_dates <- floor_date(ymd(rownames(data.frame(temp))),
                          unit = "month") + days(15)
  rain_stl <- data.frame(stl_dates)

  stl_res <- list()

  for(i in 4:37){
    temp <- data.frame(stl(rain_ts,
                           s.window = "periodic",
                           t.window = i,
                           t.degree = 0)$time.series)
    temp <- cbind(rain_stl, temp) %>%
      gather(component, value, -stl_dates) %>%
      rename(date_of = stl_dates)
    temp$n_months <- i
    temp$site <- rain_site[[j]]$site[1]

    stl_res[[i]] <- temp
  }

  rain_sites_ts[[j]] <- bind_rows(stl_res)
}

rain_sites_df <- bind_rows(rain_sites_ts)

rain_trend <- filter(rain_sites_df, component == "trend")

ggplot(rain_trend, aes(x = date_of, y = value,
                       color = n_months, group = n_months)) +
  geom_line() +
  scale_color_gradientn(colours = rev(heat_grad),
                        name = "Sliding window \nlength (months)") +
  theme_bw() +
  scale_x_datetime(breaks = date_breaks(width = "2 years"),
                   labels = date_format("%Y")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(2, "cm"),
        panel.margin = unit(1, "lines")) +
  labs(x = "Date", y = "Rainfall trend") +
  facet_grid(site ~ ., scales = "free_y")

rain_trend_18 <- filter(rain_trend, n_months == 18)

ggplot(rain_trend_18,
       aes(x = date_of, y = value,
           color = n_months, group = n_months)) +
  geom_line(color = "#00AAFF", size = 0.8) +
  theme_bw() +
  scale_x_datetime(breaks = date_breaks(width = "2 years"),
                   labels = date_format("%Y")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.key.width = unit(2, "cm"),
        panel.margin = unit(1, "lines")) +
  labs(x = "Date", y = "Rainfall trend (18-month smooth)") +
  facet_grid(site ~ ., scales = "free_y")


# ---- temperature_stl ----------------------------------------------------

tavg_site <- dlply(filter(at, var == "t_avg"), .(site))

tavg_sites_ts <- list()

for(j in 1:length(tavg_site)){
  tavg_temp <- tavg_site[[j]]
  tavg_temp <- tavg_temp %>%
    filter(year_of >= 1945) %>%
    mutate(date_of = as.Date(paste(year_of, month(date_of), "16", sep = "-")))

  temp <- zoo(tavg_temp$t_anomaly, tavg_temp$date_of, frequency = 12)
  tavg_ts <- ts(coredata(temp), freq = frequency(temp),
                start = c(year(start(temp)), month(start(temp))),
                end = c(year(end(temp)), month(end(temp))))
  stl_dates <- floor_date(ymd(rownames(data.frame(temp))),
                          unit = "month") + days(15)
  tavg_stl <- data.frame(stl_dates)

  stl_res <- list()

  for(i in 4:37){
    temp <- data.frame(stl(tavg_ts,
                           s.window = "periodic",
                           t.window = i,
                           t.degree = 0)$time.series)
    temp <- cbind(tavg_stl, temp) %>%
      gather(component, value, -stl_dates) %>%
      rename(date_of = stl_dates)
    temp$n_months <- i
    temp$site <- tavg_site[[j]]$site[1]

    stl_res[[i]] <- temp
  }

  tavg_sites_ts[[j]] <- bind_rows(stl_res)
}

tavg_sites_df <- bind_rows(tavg_sites_ts)


# ---- index_stl ----------------------------------------------------------

indices_ts <- list()

for(j in 1:length(ind)){
  ind_temp <- ind[[j]]
  temp <- zoo(ind_temp$value, ind_temp$date_of, frequency = 12)
  ind_ts <- ts(coredata(temp), freq = frequency(temp),
               start = c(year(start(temp)), month(start(temp))),
               end = c(year(end(temp)), month(end(temp))))
  stl_dates <- floor_date(ymd(rownames(data.frame(temp))),
                          unit = "month") + days(15)
  ind_stl <- data.frame(stl_dates)

  stl_res <- list()

  for(i in 4:37){
    temp <- data.frame(stl(ind_ts,
                           s.window = "periodic",
                           t.window = i,
                           t.degree = 0)$time.series)
    temp$actual <- ind_ts
    temp <- cbind(ind_stl, temp) %>%
      gather(component, value, -stl_dates) %>%
      rename(date_of = stl_dates)
    temp$n_months <- i
    temp$index <- ind[[j]]$index[1]

    stl_res[[i]] <- temp
  }

  indices_ts[[j]] <- bind_rows(stl_res)
}

indices_df <- bind_rows(indices_ts)


# ---- combine_rain_temp_data ---------------------------------------------

temp1 <- rain_selected %>%
  select(site:rain_monthly_mm, date_of, rain_anomaly)

temp2 <- at %>%
  filter(var == "t_avg") %>%
  select(-date_of)

temp3 <- temp2 %>%
  gather(measurement, value, t_anomaly:t_monthly) %>%
  unite(variable, var, measurement) %>%
  rename(var = variable)

temp3$var <- str_replace(temp3$var, "_t_", "_")

temp3 <- temp3 %>% spread(var, value)

temp4 <- rain_sites_df %>%
  filter(n_months == 18 & component == "remainder") %>%
  rename(rain_detrended = value) %>%
  select(site, date_of, rain_detrended)

temp5 <- tavg_sites_df %>%
  filter(n_months == 18 & component == "remainder") %>%
  rename(tavg_detrended = value) %>%
  select(site, date_of, tavg_detrended)

climates <- temp1 %>%
  mutate(date_of = ymd(paste(year_of, month_of, "16", sep = "-"))) %>%
  inner_join(temp4) %>%
  inner_join(temp3) %>%
  inner_join(temp5) %>%
  select(site, date_of, year_of, month_of, rain_monthly_mm,
         rain_anomaly:tavg_detrended)

climates_tidy <- gather(climates, var, value, rain_monthly_mm:tavg_detrended)

rm(temp1)
rm(temp2)
rm(temp3)
rm(temp4)
rm(temp5)


# ---- anomaly_index_ccf --------------------------------------------------

clim_site <- dlply(filter(climates_tidy, var == "rain_anomaly"), .(site))

clim_anom <- list()

for(i in 1:length(clim_site)){

  clim_anom_test <- list()
  clim_set <- tbl_df(clim_site[[i]])
  current_site <- as.character(clim_set[1, ]$site)

  for(j in 1:length(ind)){

    current_ind <- as.character(ind[[j]]$index[1])

    ind_set <- indices_df %>%
      filter(index == current_ind & n_months == 4 & component == "trend")

    ind_ts <- zoo(ind_set$value, ind_set$date_of, frequency = 12)
    clim_ts <- zoo(clim_set$value, clim_set$date_of, frequency = 12)

    # Start and end dates of period in which the time series overlap
    s <- c(year(max(clim_set[1, ]$date_of, ind_set[1, ]$date_of)),
           month(max(clim_set[1, ]$date_of, ind_set[1, ]$date_of)))
    e <- c(year(min(clim_set[nrow(clim_set), ]$date_of,
                    ind_set[nrow(ind_set), ]$date_of)),
           month(min(clim_set[nrow(clim_set), ]$date_of,
                     ind_set[nrow(ind_set), ]$date_of)))

    ind_ts <- ts(coredata(ind_ts),
                 freq = frequency(ind_ts),
                 start = s,
                 end = e)

    clim_ts <- ts(coredata(clim_ts),
                  freq = frequency(clim_ts),
                  start = s,
                  end = e)

    temp <- ccf(ind_ts, clim_ts, plot = FALSE, lag.max = 24)
    temp <- data.frame(site = current_site,
                       index = current_ind,
                       acf = temp$acf,
                       lag = temp$lag,
                       ci = acf_ci(ccf(ind_ts, clim_ts, plot = FALSE,
                                       lag.max = 24)))

    clim_anom_test[[j]] <- temp
  }
  clim_anom[[i]] <- bind_rows(clim_anom_test)
}

clim_anom <- bind_rows(clim_anom)

clim_anom$index <- factor(clim_anom$index, levels = levels(ind_df$index))
clim_anom$site <- factor(clim_anom$site, levels = levels(rain_selected$site))


for(i in 1:length(levels(rain_anom$site))){

  current_site <- levels(rain_anom$site)[i]

  p <- ggplot(filter(clim_anom, site == current_site)) +
    geom_segment(aes(x = lag, y = acf,
                     xend = lag, yend = 0),
                 size = 1) +
    geom_hline(aes(yintercept = ci), lty = 2, color = "blue") +
    geom_hline(aes(yintercept = -ci), lty = 2, color = "blue") +
    geom_ribbon(aes(x = lag, ymin = -ci, ymax = ci),
                fill = "blue", alpha = 0.1) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    theme_bw() +
    scale_x_continuous(breaks = seq(-2, 2, length.out = 17),
                       labels = seq(-24, 24, by = 3)) +
    labs(x = "Lag (months)", y = "Cross-correlation") +
    facet_grid(index ~ .)

  f <- paste(i, "_", current_site, ".pdf", sep = "")

  ggsave(filename = f, plot = p, path = "plots/ccf_plots/rain_anomalies",
         width = 8, height = 12, units = "in")
}


# ---- anomaly_phase_analysis ---------------------------------------------

get_phase <- function(df){
  df$phase <- cut(df$value, include.lowest = TRUE,
                  breaks = c(min(df$value), -sd(df$value),
                             sd(df$value), max(df$value)),
                  labels = c("Negative Phase", "Neutral Phase",
                             "Positive Phase"))
  return(df)
}

temp <- dlply(ind_df, .(index), function(x) get_phase(x))
temp <- bind_rows(temp)

monthly_anom <- climates %>%
  ungroup() %>%
  select(site, date_of, month_of, rain_anomaly, t_avg_anomaly, t_avg_monthly) %>%
  inner_join(temp) %>%
  group_by(site, month_of) %>%
  mutate(new_tavg_med = median(t_avg_monthly),
         new_tavg_anom = t_avg_monthly - new_tavg_med) %>%
  ungroup() %>%
  group_by(site, month_of, index, phase) %>%
  summarise(mean_rain_anomaly = mean(rain_anomaly, na.rm = TRUE),
            med_rain_anomaly = median(rain_anomaly, na.rm = TRUE),
            mean_tavg_anomaly = mean(t_avg_anomaly, na.rm = TRUE),
            med_tavg_anomaly = median(t_avg_anomaly, na.rm = TRUE),
            new_med_tavg_anomaly = median(new_tavg_anom, na.rm = TRUE),
            n = n())

temp3 <- climates %>%
  ungroup() %>%
  select(site, date_of, month_of, rain_anomaly, t_avg_anomaly, t_avg_monthly) %>%
  inner_join(temp) %>%
  group_by(site, month_of) %>%
  mutate(new_tavg_med = median(t_avg_monthly),
         new_tavg_anom = t_avg_monthly - new_tavg_med)

for(i in 1:length(levels(monthly_anom$site))){

  current_site <- levels(monthly_anom$site)[i]

  t_min <- min(filter(monthly_anom, site == current_site)$new_med_tavg_anomaly)
  t_max <- max(filter(monthly_anom, site == current_site)$new_med_tavg_anomaly)
  lim <- max(abs(t_min), abs(t_max))

  # TAVG
  p <- ggplot() +
    geom_bar(data = filter(monthly_anom, site == current_site),
             aes(x = month_of, y = new_med_tavg_anomaly,
                 fill = new_med_tavg_anomaly),
             stat = "identity", position = "dodge",
             width = 0.8, size = 0.1, color = "black") +
    stat_summary(data = filter(temp3, site == current_site),
                 aes(x = month_of, y = new_tavg_anom),
                 fun.data = mean_cl_boot, geom = "errorbar",
                 width = 0.3, color = "black", alpha= 0) +
    stat_summary(data = filter(temp3, site == current_site),
                 aes(x = month_of, y = new_tavg_anom),
                 fun.y = median, geom = "point",
                 size = 2, color = "black", alpha= 0) +
    geom_hline(yintercept = 0, size = 0.1) +
    facet_grid(index ~ phase) +
    labs(y = expression("Median Temperature Anomaly"), x = "Month") +
    theme_bw() +
    scale_fill_gradientn(colours = rev(brewer.pal(11, "RdYlBu")),
                         name = "Median Anomaly",
                         limits = c(-lim, lim)) +
    theme(strip.background = element_blank(),
          legend.position = "bottom",
          legend.key.width=unit(2, "cm"),
          legend.key.height=unit(0.2, "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5))

  f <- paste(i, "_", current_site, ".pdf", sep = "")

  ggsave(filename = f, plot = p,
         path = "plots/phase_anomaly_plots/tavg_anomalies2",
         width = 12, height = 12, units = "in")


  # RAIN
  p <- ggplot() +
    geom_bar(data = filter(monthly_anom, site == current_site),
             aes(x = month_of, y = med_rain_anomaly,
                 fill = med_rain_anomaly),
             stat = "identity", position = "dodge",
             width = 0.8, size = 0.1, color = "black") +
    stat_summary(data = filter(temp3, site == current_site),
                 aes(x = month_of, y = rain_anomaly),
                 fun.data = mean_cl_boot, geom = "errorbar",
                 width = 0.3, color = "black", alpha= 0) +
    stat_summary(data = filter(temp3, site == current_site),
                 aes(x = month_of, y = rain_anomaly),
                 fun.y = median, geom = "point",
                 size = 2, color = "black", alpha= 0) +
    geom_hline(yintercept = 0, size = 0.1) +
    facet_grid(index ~ phase) +
    labs(y = expression("Median Rainfall Anomaly (mm)"), x = "Month") +
    theme_bw() +
    scale_fill_gradient2(low = "#8c510a", high = "#01665e", mid = "#f5f5f5",
                         trans = "sqrt_sign",
                         name = "Median Anomaly") +
    theme(strip.background = element_blank(),
          legend.position = "bottom",
          legend.key.width=unit(2, "cm"),
          legend.key.height=unit(0.2, "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5))

  f <- paste(i, "_", current_site, ".pdf", sep = "")

  ggsave(filename = f, plot = p,
         path = "plots/phase_anomaly_plots/rain_anomalies2",
         width = 12, height = 12, units = "in")

}


# ---- index_correlations -------------------------------------------------

phase_anom <- monthly_anom %>%
  ungroup() %>%
  select(1:4, med_rain_anomaly) %>%
  spread(phase, med_rain_anomaly) %>%
  mutate(diff = `Positive Phase` - `Negative Phase`)

lim <-  max(c(abs(min(phase_anom$diff, na.rm = TRUE)),
              abs(max(phase_anom$diff, na.rm = TRUE))))

temp <- climates %>%
  group_by(site, month_of) %>%
  summarise(new_tavg_med = median(t_avg_monthly))

phase_cor <- climates %>%
  select(site, date_of, month_of, rain_anomaly, t_avg_anomaly, t_avg_monthly) %>%
  inner_join(ind_df) %>%
  inner_join(temp) %>%
  mutate(new_tavg_anom = t_avg_monthly - new_tavg_med) %>%
  group_by(site, month_of, index) %>%
  arrange(year_of) %>%
  summarise(ind_rain_cor = cor(value, rain_anomaly),
            ind_rain_p = cor.test(value, rain_anomaly)$p.value,
            ind_tavg_cor = cor(value, new_tavg_anom),
            ind_tavg_p = cor.test(value, new_tavg_anom)$p.value,
            n = n())

phase_cor <- phase_cor %>%
  mutate(new_rain_cor = ifelse(ind_rain_p >= .05, 0, ind_rain_cor),
         new_tavg_cor = ifelse(ind_tavg_p >= .05, 0, ind_tavg_cor))

lim <-  max(c(abs(min(phase_cor$ind_rain_cor, na.rm = TRUE)),
              abs(max(phase_cor$ind_rain_cor, na.rm = TRUE))))

ggplot(phase_cor, aes(x = index, y = month_of, fill = ind_rain_cor)) +
  geom_tile(size = 0.1, color = "black") +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and monthly rainfall anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(2, "cm"),
        legend.key.height=unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(phase_cor, aes(x = index, y = month_of, fill = new_rain_cor)) +
  geom_tile(size = 0.1, color = "black") +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and monthly rainfall anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(2, "cm"),
        legend.key.height=unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))


lim <-  max(c(abs(min(phase_cor$ind_tavg_cor, na.rm = TRUE)),
              abs(max(phase_cor$ind_tavg_cor, na.rm = TRUE))))

ggplot(phase_cor, aes(x = index, y = month_of, fill = ind_tavg_cor)) +
  geom_tile(size = 0.1, color = "black") +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and monthly temperature anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(2, "cm"),
        legend.key.height=unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(phase_cor, aes(x = index, y = month_of, fill = new_tavg_cor)) +
  geom_tile(size = 0.1, color = "black") +
  facet_grid(. ~ site) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-lim, lim)) +
  theme_bw() +
  labs(x = "Climate Oscillation Index", y = "Month",
       title = "Correlation between climate indices and monthly temperature anomalies") +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(2, "cm"),
        legend.key.height=unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))