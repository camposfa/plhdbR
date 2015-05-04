Sys.setenv(TZ = 'UTC')
list.of.packages <- list("plyr", "ncdf4", "lubridate", "ggplot2",
                         "RColorBrewer", "grid", "scales", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(unlist(new.packages))
lapply(list.of.packages, require, character.only = T)

# ---- new_format ---------------------------------------------------------

# Downloaded from http://www.esrl.noaa.gov/psd/data/gridded/data.gpcc.html

# GPCC total monthly precipitation
# 0.5 degree latitude x 0.5 degree longitude global grid
# ftp://ftp.cdc.noaa.gov/Datasets/gpcc/full_v6/precip.mon.total.v6.nc
t <- nc_open("data/grids/gpcc/precip.mon.total.v6.nc")

longitude <- ncvar_get(t, varid = "lon")
latitude <- ncvar_get(t, varid = "lat")
time <- ncvar_get(t, varid = "time")
precip <- ncvar_get(t, varid = "precip")

inds_lon <- (1:dim(longitude))
inds_lat <- (1:dim(latitude))
inds_time <- (1:dim(time))

site_coords <- read.csv("site_coords.csv")
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



# Monitoring data set (in "m")
# GPCC total monthly precipitation montoring data set
# 1 degree latitude x 1 degree longitude global grid
# ftp://ftp.cdc.noaa.gov/Datasets/gpcc/monitor/precip.monitor.mon.total.1x1.v4.nc
m <- nc_open("data/grids/gpcc/precip.monitor.mon.total.1x1.v4.nc")

longitude <- ncvar_get(m, varid = "lon")
latitude <- ncvar_get(m, varid = "lat")
time <- ncvar_get(m, varid = "time")
precip <- ncvar_get(m, varid = "precip")

inds_lon <- (1:dim(longitude))
inds_lat <- (1:dim(latitude))
inds_time <- (1:dim(time))

site_coords <- read.csv("site_coords.csv")
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

cubroot_trans <-  function(){
  trans_new('cubroot', transform= function(x) x^(1/3), inverse = function(x) x^3 )
}

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

rm(t)
rm(m)
gc()



# ---- long-term mean -----------------------------------------------------

# ftp://ftp.cdc.noaa.gov/Datasets/gpcc/full_v6/precip.mon.1981-2010.ltm.v6.nc
ltm <- nc_open("data/grids/gpcc/precip.mon.1981-2010.ltm.v6.nc")

longitude <- ncvar_get(ltm, varid = "lon")
latitude <- ncvar_get(ltm, varid = "lat")
time <- ncvar_get(ltm, varid = "time")
precip <- ncvar_get(ltm, varid = "precip")

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

sqrt_sign_trans <-  function(){
  trans_new('sqrt_sign', transform= function(x) sign(x) * sqrt(abs(x)), inverse = function(x) sign(x) * x^2)
}

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