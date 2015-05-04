# ---- prepare_workspace --------------------------------------------------

Sys.setenv(TZ = 'UTC')
list.of.packages <- c("zoo", "colorspace", "plhdbR", "ggplot2", "scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(unlist(new.packages))
lapply(list.of.packages, require, character.only = T)

heat_grad <- heat_hcl(12, c = c(80, 30), l = c(30, 90), power = c(1/5, 2))

load(".RData")
`%ni%` = Negate(`%in%`)

load_plhdb_packages()


# ---- load_indices -------------------------------------------------------

ind <- load_climate_index(c("pdo", "dmi", "oni", "amo", "nao"))

ind_df <- bind_rows(ind)

ind_df <- filter(ind_df, date_of > ymd("1945-01-01"))

ind_df$index <- factor(ind_df$index, levels = c("amo", "nao", "oni", "pdo", "dmi"))

ind_neg <- ind_df
ind_neg[ind_neg$value > 0, ]$value <- 0

ind_pos <- ind_df
ind_pos[ind_pos$value < 0, ]$value <- 0

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

ggplot(filter(indices_df, n_months == 4), aes(x = as.Date(date_of), y = value)) +
  geom_line() +
  facet_grid(component ~ index, scales = "free") +
  theme_bw() +
  scale_x_date(limits = c(as.Date("1945-01-01"), as.Date("2016-01-01")))


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
  select(site, date_of, year_of, month_of, rain_monthly_mm, rain_anomaly:tavg_detrended)


climates_tidy <- gather(climates, var, value, rain_monthly_mm:tavg_detrended)


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
    e <- c(year(min(clim_set[nrow(clim_set), ]$date_of, ind_set[nrow(ind_set), ]$date_of)),
           month(min(clim_set[nrow(clim_set), ]$date_of, ind_set[nrow(ind_set), ]$date_of)))

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
                       ci = acf_ci(ccf(ind_ts, clim_ts, plot = FALSE, lag.max = 24)))

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

