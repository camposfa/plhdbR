sliding_win_models2 <- function(temp_c, temp_b) {
  temp <- slidingwin(xvar = list(index_nino3.4 = temp_c$nino3.4,
                                 dmi = temp_c$dmi,
                                 rainfall = temp_c$rain_monthly_mm,
                                 mean_temp = temp_c$tavg_anomaly),
                     cdate = temp_c$chr_date,
                     bdate = temp_b$chr_date,
                     baseline = glmer(fate ~ 1 + (1 | year_of),
                                      data = temp_b,
                                      family = binomial),
                     cinterval = "month",
                     range = c(24, 18),
                     type = "absolute",
                     refday = c(01, 01),
                     stat = c("mean"),
                     func = c("lin"))
  return(temp)
}

# Run all the models
# WARNING: TAKES A LONG TIME!!!!
fert_set_blue <- fert_set %>%
  filter(site == "kakamega") %>%
  select(site, age_class, fertility, climate, trials) %>%
  mutate(models = purrr::map2(climate, trials, sliding_win_models2))

fert_set_blue  <- fert_set_blue %>%
  mutate(datasets = purrr::map(models, ~bind_datasets(.)))



# ---- autowin ------------------------------------------------------------

autowin_all <- function(temp_r, temp_c, temp_b) {

  r1 <- autowin_models(temp_r[[1]], temp_c, temp_b, "nino3.4")
  r2 <- autowin_models(temp_r[[2]], temp_c, temp_b, "dmi")
  r3 <- autowin_models(temp_r[[3]], temp_c, temp_b, "rain_monthly_mm")
  r4 <- autowin_models(temp_r[[4]], temp_c, temp_b, "tavg_anomaly")

  res <- bind_rows(r1, r2, r3, r4)
  return(res)
}

autowin_models <- function(temp_r, temp_c, temp_b, v1) {

  aw <- autowin(reference = temp_r,
                xvar = temp_c[, v1],
                cdate = temp_c$chr_date,
                bdate = temp_b$chr_date,
                func = "lin",
                baseline = glmer(fate ~ 1 + (1 | year_of),
                                 data = temp_b,
                                 family = binomial),
                cinterval = "month",
                range = c(24, 0),
                type = "absolute",
                refday = c(01, 01),
                stat = "mean")

  aw$v1 <- v1

  return(aw)
}

aw <- fert_set_blue %>%
  filter(age_class == "adult") %>%
  mutate(autowin = purrr::pmap(list(models, climate, trials), autowin_all)) %>%
  select(site, age_class, autowin) %>%
  unnest()

aw$v1 <- revalue(aw$v1, c("rain_monthly_mm" = "Rainfall",
                          "tavg_anomaly" = "Temperature",
                          "nino3.4" = "ENSO Index",
                          "dmi" = "IOD Index"))

aw$v1 <- factor(aw$v1,
                levels = c("Rainfall", "Temperature", "ENSO Index", "IOD Index"))

aw %>%
  group_by(v1) %>%
  top_n(1, wt = cor) %>%
  View()

plotcor_fc(aw, type = "A") +
  theme_gcb_x2() +
  facet_grid(. ~ v1) +
  theme(legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.25, "cm"),
        plot.title = element_text(hjust = 0),
        plot.caption = element_text(size = 16, hjust = 0.5)) +
  labs(title = "Blue Monkey",
       subtitle = "Climate variable autocorrelation",
       caption = "Pearson correlation coefficient")

aw %>%
  mutate(abs_cor = abs(cor)) %>%
  filter(site == "kakamega" & WindowOpen %in% 3:7 & WindowClose %in% 3:7) %>%
  group_by(v1) %>%
  arrange(v1, abs_cor) %>%
  top_n(1, abs_cor) %>%
  select(-c(6:11))

# Period 2
cw %>%
  mutate(abs_cor = abs(cor)) %>%
  filter(site == "kakamega" & WindowOpen %in% 3:7 & WindowClose %in% 3:7) %>%
  group_by(v1, v2) %>%
  arrange(v1, v2, abs_cor) %>%
  top_n(1, abs_cor) %>%
  select(-c(6:10))

# Period 1
cw %>%
  mutate(abs_cor = abs(cor)) %>%
  filter(site == "kakamega" & WindowOpen %in% 18:24 & WindowClose %in% 18:24) %>%
  group_by(v1, v2) %>%
  arrange(v1, v2, abs_cor) %>%
  top_n(1, abs_cor) %>%
  select(-c(6:10))


# ---- fertility_autocorr -------------------------------------------------

temp <- fert_set %>%
  select(site, age_class, null_model) %>%
  mutate(fitted = purrr::map(null_model, ~ broom::augment(.) %>%
                               tbl_df()))

temp2 <- temp %>%
  select(site, age_class, fitted) %>%
  unnest() %>%
  group_by(site, age_class, year_of) %>%
  summarise(fert = mean(.mu))

temp2$year_of <- as.numeric(temp2$year_of)

temp2 <- temp2 %>%
  filter(!(site == "kakamega" & year_of < 1997))

temp3 <- temp2 %>%
  group_by(site) %>%
  do(acf = acf(.$fert, lag.max = 15, plot = FALSE)$acf,
     lag = acf(.$fert, lag.max = 15, plot = FALSE)$lag)

temp3 <- unnest(temp3)

temp3 <- temp3 %>%
  group_by(site) %>%
  summarise(n = n()) %>%
  inner_join(temp3) %>%
  mutate(lower = -1 * qnorm((1 + 0.95) / 2) / sqrt(n),
         upper = 1 * qnorm((1 + 0.95) / 2) / sqrt(n),
         sig = acf > upper | acf < lower)

ggplot(temp3, aes(x = lag)) +
  geom_segment(aes(y = acf, xend = lag, yend = 0, color = sig)) +
  geom_point(aes(y = acf, fill = sig), shape = 21, color = "white") +
  geom_text(data = filter(temp3, sig == TRUE),
            aes(x = lag, y = acf + (0.15 * sign(acf)),
                label = lag, color = sig), size = 3) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "gray70", alpha = 0.25) +
  facet_wrap(~site, labeller = global_labeller, ncol = 2) +
  theme_gcb_x2() +
  labs(x = "Lag (years)", y = "Autocorrelation") +
  coord_cartesian(xlim = c(0, 25), ylim = c(-1, 1)) +
  # scale_x_continuous(breaks = 0:15) +
  scale_fill_manual(values = c("black", "firebrick"), guide = FALSE) +
  scale_color_manual(values = c("black", "firebrick"), guide = FALSE)

# Example plot
forecast::Acf(filter(temp2, site == "rppn-fma")$fert, lag.max = 100)
acf(filter(temp2, site == "rppn-fma")$fert, lag.max = 100)


set.seed(2014)
iter <- 10000
my_n <- nrow(temp2)
my_lags <- 15

temp3 <- temp2 %>%
  group_by(site) %>%
  do(acf = acf(.$fert, lag.max = my_lags, plot = FALSE)$acf,
     lag = acf(.$fert, lag.max = my_lags, plot = FALSE)$lag) %>%
  unnest()

# Rep data frame
temp4 <- temp2[rep(seq_len(my_n), iter), ]
temp4$iter <- rep(1:iter, each = my_n)

temp4 <- temp4 %>%
  group_by(site, iter) %>%
  mutate(fert = sample(fert)) %>%
  do(acf = acf(.$fert, lag.max = my_lags, plot = FALSE)$acf,
     lag = acf(.$fert, lag.max = my_lags, plot = FALSE)$lag)

temp4 <- unnest(temp4)

temp5 <- temp4 %>%
  group_by(site, lag) %>%
  summarise(upper = quantile(acf, probs = 0.995),
            lower = quantile(acf, probs = 0.005)) %>%
  inner_join(temp3)

temp5 <- temp5 %>%
  group_by(site, lag) %>%
  mutate(sig = acf > upper | acf < lower)

ggplot(filter(temp5, lag > 0), aes(x = lag)) +
  geom_segment(aes(y = acf, xend = lag, yend = 0, color = sig)) +
  geom_point(aes(y = acf, fill = sig), shape = 21, color = "white") +
  geom_text(data = filter(temp5, sig == TRUE & lag > 0),
            aes(x = lag, y = acf + (0.15 * sign(acf)),
                label = lag, color = sig), size = 3) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "gray70", alpha = 0.25) +
  facet_wrap(~site, labeller = global_labeller, ncol = 2) +
  theme_gcb_x2() +
  labs(x = "Lag (years)", y = "Autocorrelation") +
  coord_cartesian(xlim = c(1, my_lags), ylim = c(-1, 1)) +
  scale_fill_manual(values = c("black", "firebrick"), guide = FALSE) +
  scale_color_manual(values = c("black", "firebrick"), guide = FALSE)



# ---- survival_autocorrelation -------------------------------------------

temp <- surv_set %>%
  select(site, age_class, null_model) %>%
  mutate(fitted = purrr::map(null_model, ~ broom::augment(.) %>%
                               tbl_df()))

temp2 <- temp %>%
  select(site, age_class, fitted) %>%
  unnest() %>%
  group_by(site, age_class, year_of) %>%
  summarise(surv = mean(.mu))

temp2$year_of <- as.numeric(temp2$year_of)

temp2 <- temp2 %>%
  filter(!(site == "kakamega" & year_of < 1997))

temp3 <- temp2 %>%
  group_by(site,age_class) %>%
  do(acf = acf(.$surv, lag.max = 15, plot = FALSE)$acf,
     lag = acf(.$surv, lag.max = 15, plot = FALSE)$lag)

temp3 <- unnest(temp3)

temp3 <- temp3 %>%
  group_by(site, age_class) %>%
  summarise(n = n()) %>%
  inner_join(temp3) %>%
  mutate(lower = -1.96 / sqrt(n),
         upper = 1.96 / sqrt(n),
         sig = acf > upper | acf < lower)

ggplot(temp3, aes(x = lag)) +
  geom_segment(aes(y = acf, xend = lag, yend = 0, color = sig)) +
  geom_point(aes(y = acf, fill = sig), shape = 21, color = "white") +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "gray70", alpha = 0.25) +
  geom_hline(aes(yintercept = lower), lty = 2, color = "gray70") +
  geom_hline(aes(yintercept = upper), lty = 2, color = "gray70") +
  facet_grid(site ~ age_class, labeller = global_labeller) +
  theme_gcb() +
  labs(x = "Lag (years)", y = "Autocorrelation") +
  scale_x_continuous(breaks = 0:15) +
  scale_fill_manual(values = c("black", "firebrick"), guide = FALSE) +
  scale_color_manual(values = c("black", "firebrick"), guide = FALSE)

# ---- old ----------------------------------------------------------------







# Randomize
rand_win_models <- function(temp_c, temp_b) {
  temp <- randwin(repeats = 10,
                  xvar = list(index_nino3.4 = temp_c$nino3.4,
                              dmi =temp_c$dmi,
                              rainfall = temp_c$rain_monthly_mm,
                              mean_temp = temp_c$tavg_anomaly),
                  cdate = temp_c$chr_date,
                  bdate = temp_b$chr_date,
                  baseline = glmer(fate ~ 1 + (1 | year_of),
                                   data = temp_b,
                                   family = binomial),
                  cinterval = "month",
                  range = c(24, 0),
                  type = "absolute",
                  refday = c(01, 01),
                  stat = c("mean"),
                  func = c("lin"))
  return(temp)
}

# Extremely long time!!!!
surv_set_blue <- surv_set_blue %>%
  mutate(randsets = purrr::map2(climate, trials, rand_win_models))

# temp_c <- surv_set_blue[1, ]$climate[[1]]
# temp_b <- surv_set_blue[1, ]$trials[[1]]
# temp1 <- rand_win_models(temp_c, temp_b)

# Extract p values from randsets
bind_randsets <- function(lst) {
  n_var <- nrow(lst$combos)
  ds <- list(n_var)
  for (i in 1:n_var) {
    df <- lst[[i]]
    df$var <- lst$combos$climate[i]
    ds[[i]] <- tbl_df(df)
  }
  res <- bind_rows(ds)
}

summarise_randsets <- function(rand_set, bio_set) {
  rand_set <- rand_set[[1]]
  bio_set <- bio_set[[1]]
  df <- rand_set$combos
  n_var <- nrow(df)
  pc <- numeric(n_var)
  for (i in 1:n_var) {
    ss <- bio_set[[i]]$Dataset$sample.size[1]
    pc[i] <- pvalue(datasetrand = rand_set[[i]], dataset = bio_set[[i]]$Dataset,
                    metric = "C", sample.size = ss)
  }
  df$pc <- pc
  return(df)
}

pcs <- list()
for (i in 1:nrow(surv_set_blue)) {
  pcs[[i]] <- summarise_randsets(surv_set_blue[i, ]$randsets,
                                 surv_set_blue[i, ]$models)
}

surv_set_blue$pcs <- pcs

surv_set_blue$site <- factor(surv_set_blue$site, levels = site_list)

# Order results by p value
p_surv_table_blue <- unnest(select(surv_set_blue, site, age_class, pcs)) %>%
  arrange(pc)