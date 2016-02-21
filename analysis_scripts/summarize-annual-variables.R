# Function to assign monthly data to different kinds of "years"
# Calendar years, birth-years, rainfall years
assign_group_year <- function(df, group_dates) {

  temp <- df %>%
    inner_join(group_dates, by = "site") %>%
    mutate(month_num = month(date_of),
           group_year = ifelse(month_num >= month_start,
                               year_of, year_of - 1)) %>%
    select(-month_start, -month_num)

  return(temp)
}

birth_years <- data_frame(site = study_durations$Study.Id,
                          month_start = c(2, 5, 8, 12, 2, 4, 9))

climates_combined <- assign_group_year(climates_combined, birth_years)
climates <- assign_group_year(climates, birth_years)

ann_mean <- climates_combined %>%
  ungroup() %>%
  select(-date_of, -month_of, -year_of, -rain_data_source) %>%
  group_by(site, group_year) %>%
  summarise_each(funs(mean(., na.rm = TRUE), n())) %>%
  rename(n_months = rain_monthly_mm_n) %>%
  select(-ends_with("_n")) %>%
  filter(n_months == 12)

ann_div <- climates %>%
  group_by(site, group_year) %>%
  mutate(rain_adj = ifelse(rain_monthly_mm == 0, .001, rain_monthly_mm)) %>%
  summarise(n_months = n(),
            shannon_rain = diversity(rain_adj, index = "shannon"),
            simpson_rain = diversity(rain_adj, index = "simpson"),
            invsimpson_rain = diversity(rain_adj, index = "invsimpson"),
            cov_rain = sd(rain_monthly_mm, na.rm = TRUE) /
              mean(rain_monthly_mm, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(n_months == 12)

ann_total <- climates %>%
  group_by(site, group_year) %>%
  summarise(rain_total_mm = sum(rain_monthly_mm),
            n_months = n()) %>%
  filter(n_months == 12)

ann_extremes <- climates %>%
  group_by(site, group_year) %>%
  summarise(coldest_tmin_anomaly = min(tmin_anomaly),
            hottest_tmax_anomaly = max(tmax_anomaly),
            wettest_anomaly = max(rain_anomaly),
            driest_anomaly = min(rain_anomaly),
            n_months = n()) %>%
  filter(n_months == 12)


# Use Shannon diversity index and combine with other climate variables
climate_predictors <- ann_mean %>%
  left_join(select(ann_total, -n_months)) %>%
  left_join(select(ann_extremes, -n_months)) %>%
  left_join(select(ann_div, site, group_year, shannon_rain))
