
# ---- load_packages ------------------------------------------------------

Sys.setenv(TZ = 'UTC')
list.of.packages <- list("plyr", "reshape2", "ggplot2", "lubridate",
                         "RColorBrewer", "dplyr", "magrittr",
                         "stringr", "scales", "grid", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(unlist(new.packages))
lapply(list.of.packages, require, character.only = T)


study_durations <- lh %>%
  group_by(Study.Id) %>%
  summarise(min_entry = min(Entry.Date),
            max_depart = max(Depart.Date),
            n_rec = n()) %>%
  mutate(dur = as.duration(max_depart - min_entry))

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

# Filter and merge two sources for karisoke data
`%ni%` = Negate(`%in%`)

# # Exclude these years for field station data
# drop_years_k1 <- c(1981, 1986, 1987, 1991)
# karisoke <- filter(karisoke, year_of %ni% drop_years_k1)
#
# # Exclude years for airstrip data
# drop_years_k2 <- c(1982:1985, 1988:1990, 1992, 2002, 2004, 2005)
# karisoke_air <- filter(karisoke_air, year_of %ni% drop_years_k2)

# karisoke <- bind_rows(karisoke, karisoke_air)
# karisoke$site <- "karisoke"
#
# karisoke <- arrange(karisoke, date_of)

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
# ssh -L 3307:localhost:3306 camposf@pacelab.ucalgary.ca
# enter password: Axhs79G1
system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')

# pace_db <- src_mysql(dbname = "monkey", host = "127.0.0.1", port = 3307,
#                      user = "camposf", password = "Axhs79G1")

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


# ---- satellite_data -----------------------------------------------------

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



# ---- TRMM_data ----------------------------------------------------------

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


# ---- plot_summaries -----------------------------------------------------

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

quaroot_sign_trans <-  function(){
  trans_new('quaroot_sign', transform = function(x) sign(x) * abs(x)^(1/4),
            inverse = function(x) sign(x) * x^4)
}

sqrt_sign_trans <-  function(){
  trans_new('sqrt_sign', transform = function(x) sign(x) * sqrt(abs(x)),
            inverse = function(x) sign(x) * x^2)
}

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


# ---- write_data ---------------------------------------------------------

# Write to csv file for later convenience
write.csv(rain_selected, "data/rain_csv/rain_selected.csv", row.names = FALSE)


# ---- source_comparisons -------------------------------------------------

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

ggsave(filename = "Rain Gauge VS GPCC.pdf", path = "plots/source_comparisons",
       width = 12, height = 9, units = "in")

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

ggsave(filename = "Rain Gauge VS TRMM Satellite.pdf", path = "plots/source_comparisons",
       width = 12, height = 9, units = "in")

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

ggsave(filename = "Rain Gauge VS GPCP Satellite.pdf", path = "plots/source_comparisons",
       width = 12, height = 9, units = "in")

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

ggsave(filename = "TRMM Satellite VS GPCC.pdf", path = "plots/source_comparisons",
       width = 12, height = 9, units = "in")

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

ggsave(filename = "GPCP Satellite VS GPCC.pdf", path = "plots/source_comparisons",
       width = 12, height = 9, units = "in")

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

ggsave(filename = "TRMM Satellite VS GPCP Satellite.pdf", path = "plots/source_comparisons",
       width = 12, height = 9, units = "in")


