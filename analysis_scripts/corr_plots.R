temp <- clim_nest %>% unnest()

phase_cor <- temp %>%
  select(site, year_of, site, month_of, rainfall = rain_monthly_mm,
         mean_temp = tavg_anomaly, dmi, index_nino3.4 = nino3.4) %>%
  gather(var, value, index_nino3.4, dmi) %>%
  group_by(site, month_of, var) %>%
  arrange(year_of) %>%
  summarise(ind_rain_cor = cor(value, rainfall, use = "na.or.complete", method = "spearman"),
            ind_rain_p = cor.test(value, rainfall)$p.value,
            ind_tavg_cor = cor(value, mean_temp, use = "na.or.complete", method = "spearman"),
            ind_tavg_p = cor.test(value, mean_temp)$p.value,
            n = n())

ppcor_df <- function(df) {

  df_r <- select(df, rainfall, mean_temp, dmi, index_nino3.4)
  est <- ppcor::spcor(df_r, method = "spearman")$estimate
  vars <- rownames(est)
  pvals <- ppcor::spcor(df_r, method = "spearman")$p.value

  res1 <- as.data.frame(est)
  res1$x <- vars
  res1 <- res1 %>%
    filter(x %in% c("mean_temp", "rainfall")) %>%
    select(x, dmi) %>%
    spread(x, dmi) %>%
    rename(ind_rain_cor = rainfall, ind_tavg_cor = mean_temp)
  res1$var <- "dmi_spc"

  res2 <- as.data.frame(est)
  res2$x <- vars
  res2 <- res2 %>%
    filter(x %in% c("mean_temp", "rainfall")) %>%
    select(x, index_nino3.4) %>%
    spread(x, index_nino3.4) %>%
    rename(ind_rain_cor = rainfall, ind_tavg_cor = mean_temp)
  res2$var <- "enso_spc"

  res1 <- bind_rows(res1, res2)

  res3 <- as.data.frame(pvals)
  res3$x <- vars
  res3 <- res3 %>%
    filter(x %in% c("mean_temp", "rainfall")) %>%
    select(x, dmi) %>%
    spread(x, dmi) %>%
    rename(ind_rain_p = rainfall, ind_tavg_p = mean_temp)
  res3$var <- "dmi_spc"

  res4 <- as.data.frame(pvals)
  res4$x <- vars
  res4 <- res4 %>%
    filter(x %in% c("mean_temp", "rainfall")) %>%
    select(x, index_nino3.4) %>%
    spread(x, index_nino3.4) %>%
    rename(ind_rain_p = rainfall, ind_tavg_p = mean_temp)
  res4$var <- "enso_spc"

  res3 <- bind_rows(res3, res4)

  res <- res1 %>%
    inner_join(res3, by = "var") %>%
    select(var, ind_rain_cor, ind_rain_p, ind_tavg_cor, ind_tavg_p)

  return(res)
}

spcorr <- temp %>%
  filter(!is.na(tavg_monthly)) %>%
  select(site, year_of, site, month_of, rainfall = rain_anomaly,
         mean_temp = tavg_anomaly, dmi, index_nino3.4 = nino3.4) %>%
  arrange(site, year_of) %>%
  group_by(site, month_of) %>%
  do(ppcor_df(.))

phase_cor$cor <- "Correlation"
spcorr$cor <- "Semi-partial Correlation"

# phase_cor <- bind_rows(phase_cor, spcorr)

phase_cor$var <- plyr::revalue(phase_cor$var, var_map)
phase_cor <- phase_cor %>%
  mutate(rain_cor = ifelse(ind_rain_p >= .05, NA, round(ind_rain_cor, 2)),
         tavg_cor = ifelse(ind_tavg_p >= .05, NA, round(ind_tavg_cor, 2)))

spcorr$var <- plyr::revalue(spcorr$var, var_map)
spcorr <- spcorr %>%
  mutate(rain_cor = ifelse(ind_rain_p >= .05, NA, round(ind_rain_cor, 2)),
         tavg_cor = ifelse(ind_tavg_p >= .05, NA, round(ind_tavg_cor, 2)))

# Rain
rain_lim <-  max(abs(phase_cor$rain_cor), na.rm = TRUE)

p1 <- ggplot(phase_cor, aes(x = month_of, y = var, fill = rain_cor)) +
  geom_tile(size = 0.1, color = "black") +
  geom_text(aes(label = rain_cor), size = 2.5, color = "white") +
  facet_grid(site ~ ., labeller = global_labeller) +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       name = "Correlation Coefficient",
                       limits = c(-rain_lim, rain_lim), na.value = "#F5F5F5") +
  theme_gcb() +
  labs(x = "Month", y = "Climate Index") +
  theme(legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  coord_equal()

rain_lim <-  max(abs(phase_cor$rain_cor), na.rm = TRUE)

p1 <- ggplot(phase_cor, aes(x = month_of, y = var, fill = rain_cor)) +
  geom_tile(size = 0.1, color = "black") +
  geom_text(aes(label = rain_cor), size = 2.5, color = "white") +
  facet_grid(site ~ ., labeller = global_labeller) +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       name = "Correlation Coefficient",
                       limits = c(-rain_lim, rain_lim), na.value = "#F5F5F5") +
  theme_gcb() +
  labs(x = "Month", y = "Climate Index") +
  theme(legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  coord_equal()

tavg_lim <-  max(abs(phase_cor$tavg_cor), na.rm = TRUE)

ggplot(phase_cor, aes(x = month_of, y = var, fill = tavg_cor)) +
  geom_tile(size = 0.1, color = "black") +
  geom_text(aes(label = tavg_cor), size = 2.5, color = "white") +
  coord_equal() +
  facet_grid(site ~ ., labeller = global_labeller) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                       name = "Correlation Coefficient",
                       limits = c(-tavg_lim, tavg_lim), na.value = "#F7F7F7") +
  theme_gcb() +
  labs(x = "Month", y = "Climate Index") +
  theme(legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
