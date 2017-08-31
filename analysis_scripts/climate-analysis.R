if (!("plhdbR" %in% installed.packages()[,"Package"]))
  devtools::install_github("camposfa/plhdbR")

if (!("ClimGrid" %in% installed.packages()[,"Package"]))
  devtools::install_github("camposfa/ClimGrid")

Sys.setenv(TZ = 'UTC')
list.of.packages <- list("plhdbR", "plyr", "Hmisc", "reshape2", "ncdf4",
                         "lubridate", "RColorBrewer", "grid",
                         "stringr", "scales", "grid", "zoo", "viridis",
                         "MuMIn", "vegan", "lme4", "broom", "climwin",
                         "purrr", "ClimGrid", "sjPlot", "tidyverse", "forcats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(unlist(new.packages))
lapply(list.of.packages, require, character.only = T)

`%ni%` = Negate(`%in%`)

# If reloading completed workspace
# load("ClimatePrep.RData")


# ---- prep ---------------------------------------------------------------

f <- "data/biography_2015_06_17.csv"
lh <- read_bio_table(f)


f <- "data/fertility_2015_06_17.csv"
fert <- read_fert_table(f)

# Duplicate entries for Karisoke "SUS"
lh <- lh %>%
  filter(!(Animal.Id == "SUS" & (year(Birth.Date) == 2014 | year(Entry.Date) == 2014)))

# Date error for "TEKINF"
lh[lh$Animal.Id == "TEKINF", ]$Entry.Date <- ymd("2014-12-23")

study_durations <- lh %>%
  group_by(Study.Id) %>%
  dplyr::summarise(min_entry = min(Entry.Date),
            max_depart = max(Depart.Date),
            n_rec = n()) %>%
  mutate(dur = as.duration(max_depart - min_entry))

site_coords <- read.csv("data/site_coords.csv")
names(site_coords)[2] <- "long_name"
site_list <- c("rppn-fma", "amboseli", "kakamega", "gombe", "karisoke", "beza", "ssr")
site_coords$site <- site_list
site_list <- c("beza", "ssr", "rppn-fma", "kakamega", "amboseli", "gombe", "karisoke")
site_coords$site <- factor(site_coords$site, levels = site_list)
sites <- dplyr::select(site_coords, site, lat = Lat, lon = Long)
rm(site_coords)


# ---- weaning ------------------------------------------------------------

gest <- tbl_df(read.csv("data/IBI_descriptive_stats.csv")) %>%
  select(Study, Species, Gestation)

offspring <- lh %>%
  dplyr::filter(!is.na(Mom.Id)) %>%
  dplyr::select(Study.Id, Offspring.Id = Animal.Id, Mom.Id, Birth.Date, Depart.Date,
                Depart.Type) %>%
  dplyr::arrange(Study.Id, Mom.Id, Birth.Date) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Age.Depart = (Depart.Date - Birth.Date) / lubridate::dyears(1))
  # dplyr::left_join(weaning_ages, by = c("Study.Id" = "site")) %>%
  # dplyr::rowwise() %>%
  # dplyr::mutate(Resume.Date = as.Date(Birth.Date + lubridate::dyears(min(Age.Depart, weaning_age))))

ibi <- offspring %>%
  filter(!is.na(Birth.Date)) %>%
  ungroup() %>%
  arrange(Study.Id, Mom.Id, Birth.Date) %>%
  # group_by(Study.Id, Mom.Id, Birth.Date) %>% # Remove most twins
  # top_n(1, Depart.Date) %>% # Remove most twins
  # ungroup %>%
  group_by(Study.Id, Mom.Id) %>%
  mutate(prev_off = lag(Offspring.Id),
         prev_off_dod = lag(Depart.Date),
         prev_off_dob = lag(Birth.Date),
         prev_death_age = difftime(prev_off_dod, prev_off_dob,
                                   units = "days") / dyears(1),
         ibi_days = difftime(Birth.Date, prev_off_dob, units = "days"),
         ibi_years = ibi_days / dyears(1),
         is_successful = Birth.Date <= prev_off_dod)

# Remove some special cases, twins and probable data entry errors
ibi_suc <- ibi %>%
  filter(is_successful & Offspring.Id %ni% c("356-1", "BUK") & ibi_days != 0)

ibi_summary <- ibi_suc %>%
  ungroup %>%
  group_by(Study.Id) %>%
  summarise(med_suc = median(ibi_years),
            min_suc = min(ibi_years))

ibi_summary$gest <- gest$Gestation

weaning <- ibi_summary %>%
  mutate(weaning_age = med_suc - gest,
         min_weaning_age = min_suc - gest) %>%
  select(site = Study.Id, weaning_age)


# ==== GRIDDED_RAINFALL_DATA ==============================================

# gridded_gpcc ------------------------------------------------------------

# Downloaded from http://www.esrl.noaa.gov/psd/data/gridded/data.gpcc.html

# GPCC total monthly precipitation (1901 to 2013)
# 0.5 degree latitude x 0.5 degree longitude global grid
# ftp://ftp.cdc.noaa.gov/Datasets/gpcc/full_v6/precip.mon.total.v6.nc
# download.file("ftp://ftp.cdc.noaa.gov/Datasets/gpcc/full_v7/precip.mon.total.v7.nc",
#                destfile = "data/grids/gpcc/precip.mon.total.v7.nc")

t <- nc_open("data/grids/gpcc/precip.mon.total.v7.nc")

precip_sites_f <- extract_nc_values(t, sites = sites, x_var = "lon",
                                    y_var = "lat", t_var = "time",
                                    v_var = "precip", convert_0_to_360 = TRUE,
                                    t_unit = "days",
                                    t_origin = as.POSIXct("1800-01-01"))


# ---- gpcc_monitoring_data -----------------------------------------------

# Monitoring data set (use 2013 to present)
# GPCC total monthly precipitation montoring data set
# 1 degree latitude x 1 degree longitude global grid
# ftp://ftp.cdc.noaa.gov/Datasets/gpcc/monitor/precip.monitor.mon.total.1x1.v4.nc

# Get most recent file
# download.file("ftp://ftp.cdc.noaa.gov/Datasets/gpcc/monitor/precip.monitor.mon.total.1x1.v4.nc",
#               destfile = "data/grids/gpcc/precip.monitor.mon.total.1x1.v4.nc")

m <- nc_open("data/grids/gpcc/precip.monitor.mon.total.1x1.v4.nc")

precip_sites_m <- extract_nc_values(m, sites = sites, x_var = "lon",
                                    y_var = "lat", t_var = "time",
                                    v_var = "precip", convert_0_to_360 = TRUE,
                                    t_unit = "days",
                                    t_origin = as.POSIXct("1800-01-01"))

# Get data from after 2010 only
precip_sites_m <- filter(precip_sites_m, year_of > 2013)


# ---- combine_gpcc_data --------------------------------------------------

precip_sites <- bind_rows(precip_sites_m, precip_sites_f)

precip_sites <- precip_sites %>%
  arrange(site, date_of) %>%
  rename(precip = v_var)

precip_sites$month_of <- factor(precip_sites$month_of, labels = month.abb)

rm(list = c("t", "m", "precip_sites_m", "precip_sites_f"))


# ---- berkeley_tavg_anom -------------------------------------------------

# Temperature Anomaly (relative to 1951-1980 mean)
# 1 degree latitude x 1 degree longitude global grid

# Get most recent files
# download.file("http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_LatLong1.nc",
#               destfile = "data/grids/Berkeley/Complete_TAVG_LatLong1.nc")
# download.file("http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_LatLong1.nc",
#               destfile = "data/grids/Berkeley/Complete_TMAX_LatLong1.nc")
# download.file("http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_LatLong1.nc",
#               destfile = "data/grids/Berkeley/Complete_TMIN_LatLong1.nc")

# http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_LatLong1.nc
ta <- nc_open("data/grids/Berkeley/Complete_TAVG_LatLong1.nc")
# http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_LatLong1.nc
tx <- nc_open("data/grids/Berkeley/Complete_TMAX_LatLong1.nc")
# http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_LatLong1.nc
tn <- nc_open("data/grids/Berkeley/Complete_TMIN_LatLong1.nc")

ta_anom <- extract_nc_values(ta, sites = sites, x_var = "longitude",
                             y_var = "latitude", t_var = "time",
                             v_var = "temperature", convert_0_to_360 = FALSE,
                             t_unit = "date_decimal",
                             t_origin = NULL)
ta_ltm <- extract_nc_values(ta, sites = sites, x_var = "longitude",
                             y_var = "latitude", t_var = "time",
                             v_var = "climatology", convert_0_to_360 = FALSE,
                             t_unit = "monthly",
                             t_origin = NULL)

tx_anom <- extract_nc_values(tx, sites = sites, x_var = "longitude",
                             y_var = "latitude", t_var = "time",
                             v_var = "temperature", convert_0_to_360 = FALSE,
                             t_unit = "date_decimal",
                             t_origin = NULL)
tx_ltm <- extract_nc_values(tx, sites = sites, x_var = "longitude",
                             y_var = "latitude", t_var = "time",
                             v_var = "climatology", convert_0_to_360 = FALSE,
                             t_unit = "monthly",
                             t_origin = NULL)

tn_anom <- extract_nc_values(tn, sites = sites, x_var = "longitude",
                             y_var = "latitude", t_var = "time",
                             v_var = "temperature", convert_0_to_360 = FALSE,
                             t_unit = "date_decimal",
                             t_origin = NULL)
tn_ltm <- extract_nc_values(tn, sites = sites, x_var = "longitude",
                             y_var = "latitude", t_var = "time",
                             v_var = "climatology", convert_0_to_360 = FALSE,
                             t_unit = "monthly",
                             t_origin = NULL)

ta_anom <- rename(ta_anom, t_anom = v_var)
ta_ltm <- rename(ta_ltm, t_monthly = v_var, month_of = t_step)
tx_anom <- rename(tx_anom, t_anom = v_var)
tx_ltm <- rename(tx_ltm, t_monthly = v_var, month_of = t_step)
tn_anom <- rename(tn_anom, t_anom = v_var)
tn_ltm <- rename(tn_ltm, t_monthly = v_var, month_of = t_step)

tavg <- inner_join(ta_anom, ta_ltm)
tmax <- inner_join(tx_anom, tx_ltm)
tmin <- inner_join(tn_anom, tn_ltm)

tavg$var <- "tavg"
tmax$var <- "tmax"
tmin$var <- "tmin"

at <- bind_rows(tavg, tmax, tmin)

at <- at %>%
  mutate(year_of = year(date_of),
         t_monthly = t_monthly + t_anom,
         t_anomaly = as.numeric(t_anom),
         var = factor(var, levels = c("tmax", "tavg", "tmin"))) %>%
  select(site, date_of, year_of, month_of, t_monthly, t_anomaly, var)

at$month_of <- factor(at$month_of, labels = month.abb)

rm(list = c("ta", "tx", "tn", "ta_anom", "ta_ltm", "tavg", "tx_anom",
            "tx_ltm", "tmax", "tn_anom", "tn_ltm", "tmin"))


# ==== GRIDDED_DROUGHT_INDEX_DATA =========================================

# ---- Standardized Precipitation Evapotranspiration Index (SPEI) ---------

# Global SPEI database
# 0.5 degree latitude x 0.5 degree longitude global grid
# http://sac.csic.es/spei/database.html

# Use 1, 3, 6, and 12 month drought condition data
f <- c("01", "03", "06", "12")

spei <- NULL

for (j in 1:length(f)) {

  t <- nc_open(paste("data/grids/SPEI/SPEI_", f[j], ".nc", sep = ""))

  spei[[j]] <- extract_nc_values(t, sites = sites, x_var = "lon",
                            y_var = "lat", t_var = "time",
                            v_var = "spei", convert_0_to_360 = FALSE,
                            t_unit = "days",
                            t_origin = ymd_hms("1900-01-01 00:00:00"))

  spei[[j]]$period <- j
  names(spei[[j]])[5] <- "spei"

}

spei <- bind_rows(spei)
spei <- spread(spei, period, spei)
names(spei)[5:(5 + length(f) - 1)] <- paste("spei", f, sep = "_")

spei_nulls <- filter(spei, (is.na(spei_01) | is.na(spei_03) | is.na(spei_06) |
                       is.na(spei_12)) & year_of > 1901)

spei <- anti_join(spei, spei_nulls) %>%
  arrange(site, date_of)


# Monitoring data set
spei_m <- NULL

for (j in 1:length(f)) {

  t <- nc_open(paste("data/grids/SPEI/monitor/SPEI_", f[j], "_m.nc", sep = ""))

  spei_m[[j]] <- extract_nc_values(t, sites = sites, x_var = "lon",
                                 y_var = "lat", t_var = "time",
                                 v_var = "spei", convert_0_to_360 = FALSE,
                                 t_unit = "days",
                                 t_origin = ymd_hms("1900-01-01 00:00:00"))

  spei_m[[j]]$period <- j
  names(spei_m[[j]])[5] <- "spei"

}

spei_m <- bind_rows(spei_m)
spei_m <- spread(spei_m, period, spei)
names(spei_m)[5:(5 + length(f) - 1)] <- paste("spei", f, sep = "_")
spei_m$date_of <- ymd(paste(spei_m$year_of, spei_m$month_of, "16", sep = "-"), tz = "UTC")

spei_m_replace <- semi_join(spei_m, select(spei_nulls, site, year_of, month_of))

spei_m <- filter(spei_m, year(date_of) > 2013)

spei <- bind_rows(spei, spei_m, spei_m_replace) %>%
  arrange(site, date_of)

spei$month_of <- factor(spei$month_of, labels = month.abb)

rm(t, spei_m_replace, spei_nulls)


# ==== RAIN_GAUGE_DATA=====================================================

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
  mutate(date_of = ymd(paste(year_of, "-12-31", sep = ""), tz = "UTC"),
         type = "yearly",
         site = "beza")

gombe <- tbl_df(read.csv("data/rain_csv/gombe.csv"))
names(gombe) <- c("year_of", "rainfall")
gombe <- gombe %>%
  filter(!is.na(rainfall)) %>%
  mutate(date_of = ymd(paste(year_of, "-12-31", sep = ""), tz = "UTC"),
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
  mutate(date_of = ymd(paste(month_of, day_of, sep = "-"), tz = "UTC"),
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
  mutate(date_of = ymd(paste(year_of, month_of, "01", sep = "-"), tz = "UTC") + months(1) - days(1),
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

# If no access to PACE DB, use the csv file:
ssr <- read.csv("data/rain_csv/ssr.csv") %>%
  tbl_df() %>%
  select(-X) %>%
  mutate(date_of = ymd(date_of, tz = "UTC"))


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
         date_of = ymd(date_of, tz = "UTC"),
         site = "ssr") %>%
  arrange(date_of) %>%
  select(year_of, rainfall, date_of, type, site)


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
for (i in 1:length(f)) {
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

rm(list = c("d", "s", "f"))


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
                            levels = site_list)

rain_monthly <- inner_join(rain_monthly, study_durations, by = c("site" = "Study.Id"))


# ---- select_final_rainfall_data -----------------------------------------

# Assign data source priority and remove years before min_entry - 5 years
rain_monthly <- rain_monthly %>%
  mutate(date_of = ymd(paste(year_of, month_of, "01", sep = "-"), tz = "UTC"),
         priority = ifelse(data_source == "rain_gauge", 1,
                           ifelse(data_source == "nearby_station", 2,
                                  ifelse(data_source == "satellite_trmm", 3,
                                         ifelse(data_source == "satellite_gpcp", 5,
                                                4))))) %>%
  filter(date_of > min_entry - years(5))

rain_monthly$site <- factor(rain_monthly$site, levels = site_list)

# Grouping by site, year, and month, take data source with smallest priority number
rain_selected <- rain_monthly %>%
  ungroup() %>%
  filter(is_complete == TRUE) %>%
  group_by(site, year_of, month_of) %>%
  arrange(priority) %>%
  summarise(rain_monthly_mm = first(rain_monthly_mm),
            data_source = first(data_source),
            recording_interval = first(recording_interval),
            n_measurements = first(n_measurements),
            date_of = min(date_of),
            min_entry = min(min_entry))


# ---- rainfall_source_comparisons ----------------------------------------

# Convert from long to wide format
source_comp <- rain_monthly %>%
  filter(is_complete == TRUE) %>%
  ungroup %>%
  dcast(site + date_of ~ data_source, value.var = "rain_monthly_mm") %>%
  tbl_df()


# ---- predict_rain_gauge_data --------------------------------------------

# Model rain gauge value on missing days by regressing GPCC data
mod_gpcc <- source_comp %>%
  filter((!is.na(rain_gauge) | !is.na(nearby_station)) & !is.na(gpcc)) %>%
  group_by(site, date_of) %>%
  mutate(month_of = month(date_of),
         new_gauge = ifelse(!is.na(rain_gauge), rain_gauge, nearby_station)) %>%
  select(site, date_of, month_of, nearby_station, rain_gauge, new_gauge, gpcc) %>%
  ungroup() %>%
  group_by(site, month_of) %>%
  do(mod = lm(new_gauge ~ gpcc, data = .))

# Predict rain gauge data for days with GPCC data but no (or incomplete) rain gauge data
site_set <- levels(factor(mod_gpcc$site))

temp <- rain_selected %>%
  filter(as.character(site) %in% site_set & data_source == "gpcc") %>%
  rename(gpcc = rain_monthly_mm)

p <- list()
count <- 1
# Predict rain gauge data for days with GPCC data but no rain gauge or TRMM data
for (i in 1:length(site_set)) {

  current_site <- levels(factor(mod_gpcc$site))[i]

  for (j in 1:12) {

    df <- filter(temp, site == current_site & month_of == j)

    df$rain_predicted <- predict(mod_gpcc[mod_gpcc$site == current_site & mod_gpcc$month_of == j, ]$mod[[1]], newdata = df)

    p[[count]] <- df

    count <- count + 1
  }
}

p <- bind_rows(p)

p <- select(p, site, year_of, month_of, rain_predicted)

rain_selected <- left_join(rain_selected, p)

rain_selected$data_source <- as.character(rain_selected$data_source)
rain_selected[which(!is.na(rain_selected$rain_predicted)), ]$rain_monthly_mm <- rain_selected[which(!is.na(rain_selected$rain_predicted)), ]$rain_predicted
rain_selected[which(!is.na(rain_selected$rain_predicted)), ]$data_source <- "predicted_from_gpcc"

rain_selected[which(rain_selected$rain_monthly_mm < 0), ]$rain_monthly_mm <- 0

rm(list = c("mod_gpcc", "p", "temp"))


# ---- long_term_means ----------------------------------------------------

# Calculate long term mean using time period used for rain_selected.
# Not using same period as for temperature (1951 to 1980) because that would be
# mostly GPCC data, which consistently overestimates rainfall for some sites
# like Amboseli
rain_ltm <- rain_selected %>%
  ungroup() %>%
  group_by(site, month_of) %>%
  summarise(rain_ltm = mean(rain_monthly_mm))

rain_selected <- inner_join(rain_selected, rain_ltm) %>%
  mutate(rain_anomaly = rain_monthly_mm - rain_ltm)

rain_selected$site <- factor(rain_selected$site, levels = site_list)

study_durations$site <- study_durations$Study.Id

rain_selected$month_of <- factor(rain_selected$month_of, labels = month.abb)

rain_selected$data_source <- factor(rain_selected$data_source,
                                    levels = c("rain_gauge", "nearby_station",
                                               "satellite_trmm", "satellite_gpcp",
                                               "gpcc", "predicted_from_gpcc"))


# ---- write_rainfall_selected_data ---------------------------------------

# Write to csv file for later convenience
write.csv(rain_selected, "data/rain_csv/rain_selected.csv", row.names = FALSE)



# ==== OSCILLATION_ANALYSIS ===============================================

# ---- load_indices -------------------------------------------------------

ind <- ClimGrid::load_climate_index(c("pdo", "dmi", "mei", "soi", "nino3.4",
                                      "amo", "nao", "sam", "ao"))

ind <- ClimGrid::load_climate_index(c("dmi", "nino3.4"))

ind_df <- bind_rows(ind)

ind_df <- filter(ind_df, date_of > ymd("1945-01-01", tz = "UTC"))

ind_df$index <- factor(ind_df$index,
                       levels = c("pdo", "dmi", "mei", "soi", "nino3.4", "amo",
                                  "nao", "sam", "ao"))

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
                   limits = c(ymd("1944-01-01", tz = "UTC"), ymd("2016-01-01", tz = "UTC")),
                   minor_breaks = date_breaks("1 year"),
                   breaks = date_breaks("5 years")) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  labs(x = "Year", y = "Value", title = "Climate Oscillations\n")

rm(list = c("ind_pos", "ind_neg"))


# ---- temperature_stl ----------------------------------------------------

current_t <- list()

for(i in 1:length(levels(at$var))){

  current_var <- levels(at$var)[i]

  t_site <- dlply(filter(at, var == current_var), .(site))

  t_sites_ts <- list()

  for(j in 1:length(t_site)){
    t_temp <- t_site[[j]]
    t_temp <- t_temp %>%
      filter(year_of >= 1945) %>%
      mutate(date_of = as.Date(paste(year_of, month(date_of), "16", sep = "-")))

    temp <- zoo(t_temp$t_anomaly, t_temp$date_of, frequency = 12)
    t_ts <- ts(coredata(temp), freq = frequency(temp),
                  start = c(year(start(temp)), month(start(temp))),
                  end = c(year(end(temp)), month(end(temp))))
    stl_dates <- floor_date(ymd(rownames(data.frame(temp))),
                            unit = "month") + days(15)
    t_stl <- data.frame(stl_dates)

    # Need to specify the smoothing window for extracting the temperature trend
    # For now, use 20-year smooth (might want to adjust later)
    # Do this by setting t.window in the stl function to # of months + 1 (i.e., 241)
    temp <- data.frame(stl(t_ts,
                           s.window = "periodic",
                           t.window = 241,
                           t.degree = 0,
                           na.action = na.approx)$time.series)
    temp <- cbind(t_stl, temp) %>%
      gather(component, value, -stl_dates) %>%
      rename(date_of = stl_dates)
    temp$n_months <- 240
    temp$site <- t_site[[j]]$site[1]
    temp$variable <- current_var

    t_sites_ts[[j]] <- temp
  }

  current_t[[i]] <- bind_rows(t_sites_ts)
}

t_sites_df <- bind_rows(current_t)

t_sites_df$variable <- factor(t_sites_df$variable,
                              levels = c("tmin", "tavg", "tmax"))

rm(list = c("t_site", "t_sites_ts", "t_temp", "t_ts", "stl_dates",
            "t_stl"))


# ---- combine_rain_temp_data ---------------------------------------------

temp1 <- rain_selected %>%
  select(site:month_of, date_of, rain_monthly_mm, rain_anomaly, data_source) %>%
  rename(rain_data_source = data_source) %>%
  select(-date_of)

temp2 <- at %>%
  select(-date_of) %>%
  gather(measurement, value, t_monthly:t_anomaly) %>%
  unite(variable, var, measurement) %>%
  rename(var = variable)

temp2$var <- str_replace(temp2$var, "_t_", "_")

temp2 <- temp2 %>% spread(var, value)

temp3 <- t_sites_df %>%
  tbl_df() %>%
  filter(component == "remainder" | component == "seasonal") %>%
  unite(v, variable, component) %>%
  spread(v, value) %>%
  mutate(month_of = month(date_of, label = TRUE, abbr = TRUE),
         year_of = year(date_of),
         tmin_detrended = tmin_seasonal + tmin_remainder,
         tavg_detrended = tavg_seasonal + tavg_remainder,
         tmax_detrended = tmax_seasonal + tmax_remainder) %>%
  select(site, year_of, month_of, dplyr::contains("detrended"))

temp4 <- spei %>%
  select(-date_of)

climates <- temp1 %>%
  left_join(temp2) %>%
  left_join(temp3) %>%
  left_join(temp4) %>%
  select(site, year_of, month_of, dplyr::contains("rain"),
         dplyr::contains("tmin"), dplyr::contains("tavg"),
         dplyr::contains("tmax"), dplyr::contains("spei")) %>%
  ungroup() %>%
  mutate(date_of = ymd(paste(year_of, month_of, "16", sep = "-"), tz = "UTC")) %>%
  arrange(site, date_of)

climates$site <- factor(climates$site, levels = site_list)

climates_tidy <- climates %>%
  select(-rain_data_source) %>%
  gather(var, value, 4:ncol(.))

rm(temp1)
rm(temp2)
rm(temp3)
rm(temp4)


# ---- put_data_together --------------------------------------------------

ind_wide <- ind_df %>%
  spread(index, value)

climates_combined <- climates %>%
  full_join(ind_wide) %>%
  filter(!is.na(site)) %>%
  arrange(site, date_of)


# ---- save_r_data --------------------------------------------------------

save.image("~/GitHub/plhdbR/ClimatePrep.RData")
save(lh, fert, climates, climates_combined, ind_df, site_list, sites, weaning,
     file = "~/GitHub/plhdbR/ClimatePred1.RData")
