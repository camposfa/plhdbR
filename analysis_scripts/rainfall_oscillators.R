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
  temp <- zoo(rain_temp$rain_monthly_mm, rain_temp$date_of, frequency = 12)
  rain_ts <- ts(coredata(temp), freq = frequency(temp),
                start = c(year(start(temp)), month(start(temp))),
                end = c(year(end(temp)), month(end(temp))))
  stl_dates <- floor_date(ymd(rownames(data.frame(temp))),
                          unit = "month") + days(15)
  rain_stl <- data.frame(stl_dates)

  for(i in 4:37){
    temp <- stl(rain_ts, s.window = "periodic",
                t.window = i, t.degree = 0)
    rain_stl <- cbind(rain_stl, data.frame(temp$time.series)$trend)
    names(rain_stl)[i - 2] <- paste(i, "months", sep = "_")
  }

  rain_stl <- gather(rain_stl, window, value, -stl_dates)

  rain_stl <- mutate(rain_stl,
                     n_months = as.numeric(as.character(str_extract(rain_stl$window,
                                                                    pattern = "[0-9]+"))))

  rain_stl$site <- rain_site[[j]]$site[1]

  rain_sites_ts[[j]] <- rain_stl
}

rain_sites_df <- bind_rows(rain_sites_ts)

ggplot(rain_sites_df, aes(x = stl_dates, y = value,
                          color = n_months, group = window)) +
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

rain_sites_df_18 <- filter(rain_sites_df, n_months == 18)

ggplot(rain_sites_df_18,
       aes(x = stl_dates, y = value,
           color = n_months, group = window)) +
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


# ---- combine_rain_temp_data ---------------------------------------------

# rain_selected from "standardize_rainfall.R"
# at from "gridded_temperature.R"

temp1 <- rain_selected %>%
  select(site:rain_monthly_mm, date_of, rain_anomaly)
  # gather(var, value, rain_monthly_mm, rain_anomaly)

temp2 <- at %>%
  select(-date_of)

temp3 <- temp2 %>%
  gather(measurement, value, t_anomaly:t_monthly) %>%
  unite(variable, var, measurement) %>%
  rename(var = variable)

temp3$var <- str_replace(temp3$var, "_t_", "_")

temp3 <- temp3 %>% spread(var, value)

temp4 <- rain_sites_df_18 %>%
  rename(date_of = stl_dates,
         rain_trend = value) %>%
  select(site, date_of, rain_trend)

climates <- temp1 %>%
  mutate(date_of = ymd(paste(year_of, month_of, "16", sep = "-"))) %>%
  inner_join(temp4) %>%
  inner_join(temp3) %>%
  select(site, date_of, year_of, month_of, rain_monthly_mm, rain_anomaly:t_min_monthly)


climates_tidy <- gather(climates, var, value, rain_monthly_mm:t_min_monthly)


# ---- anomaly_index_ccf --------------------------------------------------

rain_site <- dlply(filter(climates_tidy, var == "rain_anomaly"), .(site))

rain_anom <- list()

for(i in 1:length(rain_site)){

  rain_anom_test <- list()
  rain_set <- tbl_df(rain_site[[i]])
  current_site <- as.character(rain_set[1, ]$site)

  for(j in 1:length(ind)){

    ind_set <- ind[[j]]

    current_ind <- as.character(ind_set[1, ]$index)

    ind_ts <- zoo(ind_set$value, ind_set$date_of, frequency = 12)
    rain_ts <- zoo(rain_set$value, rain_set$date_of, frequency = 12)

    # Start and end dates of period in which the time series overlap
    s <- c(year(max(rain_set[1, ]$date_of, ind_set[1, ]$date_of)),
           month(max(rain_set[1, ]$date_of, ind_set[1, ]$date_of)))
    e <- c(year(min(rain_set[nrow(rain_set), ]$date_of, ind_set[nrow(ind_set), ]$date_of)),
           month(min(rain_set[nrow(rain_set), ]$date_of, ind_set[nrow(ind_set), ]$date_of)))

    ind_ts <- ts(coredata(ind_ts),
                 freq = frequency(ind_ts),
                 start = s,
                 end = e)

    rain_ts <- ts(coredata(rain_ts),
                 freq = frequency(rain_ts),
                 start = s,
                 end = e)

    acf_df <- NULL
    reps <- 10000

    for(k in 1:reps){
      test_rain <- ts(coredata(sample(rain_ts,
                                      size = length(rain_ts))),
                      frequency = 12,
                      start = s,
                      end = e)
      acf_df <- rbind(acf_df, ccf(ind_ts, test_rain, plot = FALSE,
                                  lag.max = 24)$acf)
    }

    acf_df <- data.frame(acf_df)
    acf_df <- gather(acf_df, lag, value)

    lags <- ccf(ind_ts, test_rain, plot = FALSE, lag.max = 24)$lag

    acf_df$lag <- rep(lags, times = 1, each = reps)

    temp <- acf_df %>%
      group_by(lag) %>%
      summarise(upper = quantile(value, probs = 0.995),
                lower = quantile(value, probs = 0.005))

    actual_acf <- data.frame(actual = ccf(ind_ts, rain_ts,
                                          plot = FALSE, lag.max = 24)$acf,
                             lag = lags,
                             site = current_site,
                             index = current_ind)

    temp <- tbl_df(bind_cols(temp, select(actual_acf, -lag)))
    temp <- select(temp, site, index, lag, actual, lower, upper)

    rain_anom_test[[j]] <- temp
  }
  rain_anom[[i]] <- bind_rows(rain_anom_test)
}

rain_anom <- bind_rows(rain_anom)

rain_anom$index <- factor(rain_anom$index, levels = levels(ind_df$index))
rain_anom$site <- factor(rain_anom$site, levels = levels(rain_selected$site))

# ggplot(rain_anom, aes(x = lag)) +
#   geom_segment(aes(y = actual,
#                    xend = lag, yend = 0),
#                size = 1) +
#   geom_line(aes(y = upper), lty = 2, color = "red") +
#   geom_line(aes(y = lower), lty = 2, color = "red") +
#   geom_ribbon(aes(ymin = lower, ymax = upper),
#               fill = "red", alpha = 0.1) +
#   geom_hline(yintercept = 0) +
#   theme_bw() +
#   scale_x_continuous(breaks = seq(-2, 2, length.out = 17),
#                      labels = seq(-24, 24, by = 3)) +
#   labs(x = "Lag (months)", y = "Cross-correlation") +
#   geom_vline(xintercept = 0, lty = 3) +
#   facet_grid(site ~ index)

for(i in 1:length(levels(rain_anom$site))){

  current_site <- levels(rain_anom$site)[i]

  p <- ggplot(filter(rain_anom, site == current_site), aes(x = lag)) +
    geom_segment(aes(y = actual,
                     xend = lag, yend = 0),
                 size = 1) +
    geom_line(aes(y = upper), lty = 2, color = "blue") +
    geom_line(aes(y = lower), lty = 2, color = "blue") +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                fill = "blue", alpha = 0.1) +
    geom_hline(yintercept = 0) +
    theme_bw() +
    scale_x_continuous(breaks = seq(-2, 2, length.out = 17),
                       labels = seq(-24, 24, by = 3)) +
    labs(x = "Lag (months)", y = "Cross-correlation") +
    geom_vline(xintercept = 0, lty = 3) +
    facet_grid(index ~ .)

  f <- paste(i, "_", current_site, ".pdf", sep = "")

  ggsave(filename = f, plot = p, path = "plots/ccf_plots/rain_anomalies",
         width = 8, height = 12, units = "in")
}




# ggplot(temp, aes(x = lag)) +
#   geom_segment(aes(y = actual,
#                    xend = lag, yend = 0),
#                size = 1) +
#   geom_line(aes(y = upper), lty = 2, color = "red") +
#   geom_line(aes(y = lower), lty = 2, color = "red") +
#   geom_ribbon(aes(ymin = lower, ymax = upper),
#               fill = "red", alpha = 0.1) +
#   geom_hline(yintercept = 0) +
#   theme_classic() +
#   scale_x_continuous(breaks = seq(-2, 2, length.out = 17),
#                      labels = seq(-24, 24, by = 3)) +
#   labs(x = "Lag (months)", y = "Cross-correlation") +
#   geom_vline(xintercept = 0, lty = 3)
