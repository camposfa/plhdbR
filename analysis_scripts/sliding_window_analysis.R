rm(list = ls())
`%ni%` = Negate(`%in%`)

# If running the script from scratch
# load("ClimatePred1.RData")

# load("SlidingWin.RData")

load(".RData")

# If script already run and resuming workspace
source("~/Dropbox/R/themes_fc.R")
source("~/Dropbox/R/delta_plotting.R")
source("analysis_scripts/ggcorr.R")
source("analysis_scripts/variable_maps.R")

census_frame <- tibble(site = site_list, rep(1, 7))

surv <- stage_specific_survival(lh, weaning_ages = weaning,
                                census_start_month = census_frame[, 2][[1]])

surv <- surv %>%
  filter(year_of < 2015) %>%
  mutate(failures = trials - successes) %>%
  dplyr::rename(site = Study.Id)

surv <- complete(surv, nesting(site, year_of), age_class,
              fill = list(n_animals = 0, individual_years = 0, s = 0,
                          deaths = 0, trials = 0, successes = 0,
                          failures = 0))

surv_nest <- surv %>%
  group_by(site, age_class) %>%
  nest(.key = survival)

clim_nest <- climates_combined %>%
  select(-dplyr::contains("spei"), -rain_data_source, -date_of) %>%
  group_by(site) %>%
  nest(.key = climate)

surv_set <- inner_join(surv_nest, clim_nest)

# Function to convert survival data frame to "trials" (success failure)
get_trials <- function(df) {

  fates <- list()

  for (i in 1:nrow(df)) {
    fate <- c(rep(1L, df[i, ]$successes),
              rep(0L, df[i, ]$trials - df[i, ]$successes))

    f <- data_frame(fate = fate)

    if (nrow(f) > 0) {
      f$year_of <- df[i, ]$year_of

      fates[[i]] <- f
    }

  }

  fates <- dplyr::bind_rows(fates)
  fates$year_of <- factor(fates$year_of)

  return(fates)
}

# Convert survival data to binary "trials" (success/failure) by applying
# the get_trials function
surv_set <- surv_set %>%
  mutate(trials = purrr::map(survival, ~ get_trials(.)))

# Format annual census dates (Jan 01) from years only
# Output as dd/mm/yyyy as required by the climwin package
# Note that 1 is added to year_of for census because year_of refers to trials
# that occurred "during" that year, and are therefore "observed" on Jan. 1 of
# year_of + 1
format_dates_annual <- function(df) {
  df <- df %>%
    mutate(date_of = ymd(paste(year_of, "01", "01", sep = "-")) + lubridate::years(1),
           chr_date = strftime(as.Date(date_of), format = "%d/%m/%Y"),
           year_of = as.numeric(as.character(year_of)))

  return(df)
}

# Format month/year dates as dd/mm/yyyy as required by the climwin package
format_dates_monthly <- function(df) {
  df <- df %>%
    mutate(date_of = ymd(paste(year_of, month_of, "01", sep = "-")),
           chr_date = strftime(as.Date(date_of), format = "%d/%m/%Y"),
           year_of = as.numeric(as.character(year_of)))

  return(df)
}

# Scale each climate variable separately to make betas comparable
scale_clim <- function(df) {
  df <- df %>%
    gather(var, value, -year_of, -month_of, -date_of, -chr_date) %>%
    group_by(var) %>%
    mutate(value = scale(value)) %>%
    spread(var, value)

  return(df)
}

# Apply functions to format dates and scale climate variables
# Separately for each species and age class
surv_set <- surv_set %>%
  mutate(trials = purrr::map(trials, ~format_dates_annual(.)),
         climate = purrr::map(climate, ~format_dates_monthly(.)),
         climate = purrr::map(climate, ~scale_clim(.)))


# ---- surv_models --------------------------------------------------------

# Function to combine climwin output datasets
bind_datasets <- function(lst) {
  n_var <- nrow(lst$combos)
  ds <- list(n_var)
  for (i in 1:n_var) {
    df <- lst[[i]]$Dataset
    df$var <- lst$combos$climate[i]
    ds[[i]] <- tbl_df(df)
  }
  res <- bind_rows(ds)
}

# Function for running models
# temp_c is the data frame containing climate variables
# temp_b is the data frame containing the demographic data
sliding_win_models <- function(temp_c, temp_b) {
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
                     range = c(24, 0),
                     type = "absolute",
                     refday = c(01, 01),
                     stat = c("mean"),
                     func = c("lin"))
  return(temp)
}

# Run all the models
# WARNING: TAKES A LONG TIME!!!!
surv_set <- surv_set %>%
  mutate(models = purrr::map2(climate, trials, sliding_win_models))

surv_set <- surv_set %>%
  mutate(datasets = purrr::map(models, ~bind_datasets(.)))

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
surv_set <- surv_set %>%
  mutate(randsets = purrr::map2(climate, trials, rand_win_models))

# temp_c <- surv_set[1, ]$climate[[1]]
# temp_b <- surv_set[1, ]$trials[[1]]
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
for (i in 1:nrow(surv_set)) {
  pcs[[i]] <- summarise_randsets(surv_set[i, ]$randsets,
                                 surv_set[i, ]$models)
}

surv_set$pcs <- pcs

surv_set$site <- factor(surv_set$site, levels = site_list)

# Order results by p value
p_surv_table <- unnest(select(surv_set, site, age_class, pcs)) %>%
  arrange(pc)

save.image("SlidingWin.RData")


# ---- surv_plots ---------------------------------------------------------

p_surv_delta <- list()
p_surv_beta <- list()

for (i in seq_along(site_list)) {

  # Plots
  # Plot delta AIC
  temp1 <- surv_set %>%
    select(site, age_class, datasets) %>%
    filter(site == site_list[i]) %>%
    unnest() %>%
    # filter(var != "dmi") %>%
    mutate(deltaAICc_orig = deltaAICc)

  temp2 <- surv_set %>%
    select(site, age_class, pcs) %>%
    filter(site == site_list[i]) %>%
    unnest() %>%
    # filter(climate != "dmi") %>%
    rename(var = climate)

  temp1 <- inner_join(temp1, temp2)

  if (length(which(temp1$deltaAICc < -7) > 0)) {
    temp1[which(temp1$deltaAICc < -7), ]$deltaAICc <- -7
  }
  if (length(which(temp1$deltaAICc > 7) > 0)) {
    temp1[which(temp1$deltaAICc > 7), ]$deltaAICc <- 7
  }

  if (length(which(temp1$ModelBeta < -10) > 0)) {
    temp1[which(temp1$ModelBeta < -10), ]$ModelBeta <- -10
  }
  if (length(which(temp1$ModelBeta > 10) > 0)) {
    temp1[which(temp1$ModelBeta > 10), ]$ModelBeta <- 10
  }

  ann_text <- distinct(temp1, site, age_class, var, pc)
  ann_text$sig <- ifelse(ann_text$pc < 0.5, "sig", "ns")
  ann_text$sig <- factor(ann_text$sig, levels = c("ns", "sig"))

  p_temp <- ggplot(data = temp1, aes(x = WindowClose, y = WindowOpen)) +
    geom_tile(aes(fill = deltaAICc), color = "white", size = 0.1) +
    theme_gcb_x2() +
    facet_grid(age_class ~ var, labeller = global_labeller) +
    theme(legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.25, "cm"),
          plot.title = element_text(hjust = 0),
          legend.position = "bottom",
          plot.caption = element_text(size = 16, hjust = 0.5)) +
    labs(title = site_map[site_list[i]],
         subtitle = expression(paste(Delta, "AICc for models of survival")),
         caption = expression(paste(Delta, "AICc (compared to null model)")),
         x = "Window close (months)",
         y = "Window open (months)") +
    scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                         limits = c(-7, 7), name = "",
                         breaks = seq(-6, 6, by = 3)) +
    coord_fixed() +
    scale_x_continuous(breaks = seq(0, 24, by = 6)) +
    scale_y_continuous(breaks = seq(0, 24, by = 6)) +
    geom_text(data = ann_text, x = 17, y = 6,
              aes(label = paste0("p[c] == ", formatC(round(pc, 2), digits = 2, format = "f")),
                  color = sig),
              parse = TRUE) +
    scale_color_manual(guide = FALSE, values = c("black", "red"), drop = FALSE)


  p_surv_delta[[i]] <- p_temp

  lim <- max(abs(temp1$ModelBeta))
  p_temp <- plotbetas_fc(temp1, col = brewer.pal(11, "PiYG")) +
    facet_grid(age_class ~ var, labeller = global_labeller) +
    theme_gcb_x2() +
    theme(legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.25, "cm"),
          plot.title = element_text(hjust = 0),
          plot.caption = element_text(size = 16, hjust = 0.5)) +
    coord_fixed() +
    scale_fill_gradientn(colours = brewer.pal(11, "PiYG"),
                         trans = sqrt_sign_trans(),
                         limits = c(-10, 10), name = "") +
    labs(title = site_map[site_list[i]],
         subtitle = expression(paste("Coefficient estimates (",
                                     beta, ") for models of survival")),
         caption = expression(paste("Model", ~~beta)))

  p_surv_beta[[i]] <- p_temp

}

for (i in seq_along(p_surv_delta)) {
  p <- cowplot::plot_grid(p_surv_delta[[i]], p_surv_beta[[i]],
                     nrow = 1, scale = 0.95, labels = c("(a)", "(b)"))

  ggsave(path = "plots/vital_plots",
         filename = paste0("FigS", i + 4, "_survival_",
                           str_replace_all(tolower(site_map[site_list[i]]), " ", ""),
                           ".pdf"),
         plot = p, width = 13, height = 7, units = "in")
}






# ---- fertility ----------------------------------------------------------

f <- stage_specific_fertility(lh, fert, adult_definition = "minimum",
                              weaning_ages = weaning)

f <- f %>%
  filter(year_of < 2015) %>%
  mutate(failures = trials - successes) %>%
  dplyr::rename(site = Study.Id) %>%
  filter(age_class == "adult")

f <- complete(f, nesting(site, year_of),
              fill = list(n_animals = 0, female_years = 0, f = 0,
                          trials = 0, successes = 0,
                          failures = 0))

fert_nest <- f %>%
  group_by(site, age_class) %>%
  nest(.key = fertility)

fert_set <- inner_join(fert_nest, clim_nest)

# Convert fertility data to binary "trials" (success/failure) by applying
# the get_trials function
fert_set <- fert_set %>%
  mutate(trials = purrr::map(fertility, ~ get_trials(.)))

# Apply functions to format dates and scale climate variables
# Separately for each species
fert_set <- fert_set %>%
  mutate(trials = purrr::map(trials, ~format_dates_annual(.)),
         climate = purrr::map(climate, ~format_dates_monthly(.)),
         climate = purrr::map(climate, ~scale_clim(.)))


# ---- fert_models --------------------------------------------------------

# Run all the models
# WARNING: LONG TIME!!!!
fert_set <- fert_set %>%
  mutate(models = purrr::map2(climate, trials, sliding_win_models))

fert_set <- fert_set %>%
  mutate(datasets = purrr::map(models, ~bind_datasets(.)))

# Extremely long time!!!!
fert_set <- fert_set %>%
  mutate(randsets = purrr::map2(climate, trials, rand_win_models))

fpcs <- list()
for (i in 1:nrow(fert_set)) {
  fpcs[[i]] <- summarise_randsets(fert_set[i, ]$randsets,
                                 fert_set[i, ]$models)
}

fert_set$pcs <- fpcs

fert_set$site <- factor(fert_set$site, levels = site_list)

# Order results by p value
p_fert_table <- unnest(select(fert_set, site, age_class, pcs)) %>%
  arrange(pc)

save.image("SlidingWin.RData")


# ---- fert_plots_separate ------------------------------------------------

p_fert_delta <- list()
p_fert_beta <- list()

for (i in seq_along(site_list)) {

  # Plots
  # Plot delta AIC
  temp1 <- fert_set %>%
    select(site, datasets) %>%
    filter(site == site_list[i]) %>%
    unnest() %>%
    # filter(var != "dmi") %>%
    mutate(deltaAICc_orig = deltaAICc)

  temp2 <- fert_set %>%
    select(site, pcs) %>%
    filter(site == site_list[i]) %>%
    unnest() %>%
    # filter(climate != "dmi") %>%
    rename(var = climate)

  temp1 <- inner_join(temp1, temp2)

  if (length(which(temp1$deltaAICc < -7) > 0)) {
    temp1[which(temp1$deltaAICc < -7), ]$deltaAICc <- -7
  }
  if (length(which(temp1$deltaAICc > 7) > 0)) {
    temp1[which(temp1$deltaAICc > 7), ]$deltaAICc <- 7
  }

  if (length(which(temp1$ModelBeta < -10) > 0)) {
    temp1[which(temp1$ModelBeta < -10), ]$ModelBeta <- -10
  }
  if (length(which(temp1$ModelBeta > 10) > 0)) {
    temp1[which(temp1$ModelBeta > 10), ]$ModelBeta <- 10
  }

  ann_text <- distinct(temp1, site, var, pc)
  ann_text$sig <- ifelse(ann_text$pc < 0.5, "sig", "ns")
  ann_text$sig <- factor(ann_text$sig, levels = c("ns", "sig"))

  p_temp <- ggplot(data = temp1, aes(x = WindowClose, y = WindowOpen)) +
    geom_tile(aes(fill = deltaAICc), color = "white", size = 0.1) +
    theme_gcb_x2() +
    facet_grid(. ~ var, labeller = global_labeller) +
    theme(legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.25, "cm"),
          plot.title = element_text(hjust = 0),
          legend.position = "bottom",
          plot.caption = element_text(size = 16, hjust = 0.5)) +
    labs(title = site_map[site_list[i]],
         subtitle = expression(paste(Delta, "AICc for models of fertility")),
         caption = expression(paste(Delta, "AICc (compared to null model)")),
         x = "Window close (months)",
         y = "Window open (months)") +
    scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                         limits = c(-7, 7), name = "",
                         breaks = seq(-6, 6, by = 3)) +
    coord_fixed() +
    scale_x_continuous(breaks = seq(0, 24, by = 6)) +
    scale_y_continuous(breaks = seq(0, 24, by = 6)) +
    geom_text(data = ann_text, x = 17, y = 6,
              aes(label = paste0("p[c] == ", formatC(round(pc, 2), digits = 2, format = "f")),
                  color = sig),
              parse = TRUE, size = 3) +
    scale_color_manual(guide = FALSE, values = c("black", "red"), drop = FALSE)


  p_fert_delta[[i]] <- p_temp

  lim <- max(abs(temp1$ModelBeta))
  p_temp <- plotbetas_fc(temp1, col = brewer.pal(11, "PiYG")) +
    facet_grid(. ~ var, labeller = global_labeller) +
    theme_gcb_x2() +
    theme(legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.25, "cm"),
          plot.title = element_text(hjust = 0),
          plot.caption = element_text(size = 16, hjust = 0.5)) +
    coord_fixed() +
    scale_fill_gradientn(colours = brewer.pal(11, "PiYG"),
                         trans = sqrt_sign_trans(),
                         limits = c(-10, 10), name = "") +
    labs(title = site_map[site_list[i]],
         subtitle = expression(paste("Coefficient estimates (",
                                     beta, ") for models of fertility")),
         caption = expression(paste("Model", ~~beta)))

  p_fert_beta[[i]] <- p_temp

}

# for (i in seq_along(p_fert_delta)) {
#   p <- cowplot::plot_grid(p_fert_delta[[i]], p_fert_beta[[i]],
#                           nrow = 1, scale = 0.95, labels = c("(a)", "(b)"))
#
#   ggsave(path = "plots/vital_plots",
#          filename = paste0("0", i + 7, "_", site_map[site_list[i]], "_fertility.pdf"),
#          plot = p, width = 13, height = 5, units = "in")
# }

for (i in seq_along(p_fert_delta)) {
  p <- cowplot::plot_grid(p_fert_delta[[i]], p_fert_beta[[i]],
                          nrow = 1, scale = 0.95, labels = c("(a)", "(b)"))

  ggsave(path = "plots/vital_plots",
         filename = paste0("FigS", i + 13, "_fertility_",
                           str_replace_all(tolower(site_map[site_list[i]]), " ", ""),
                           ".pdf"),
         plot = p, width = 13, height = 5, units = "in")
}

save.image("SlidingWin.RData")

# Plots
# Plot delta AIC
temp1 <- fert_set %>%
  select(site, datasets) %>%
  unnest() %>%
  mutate(deltaAICc_orig = deltaAICc)

temp2 <- fert_set %>%
  select(site, pcs) %>%
  unnest() %>%
  rename(var = climate)

temp1 <- inner_join(temp1, temp2)

temp1[temp1$deltaAICc < -7, ]$deltaAICc <- -7
temp1[temp1$deltaAICc > 7, ]$deltaAICc <- 7

plotdelta_fc(dataset = temp1, col = brewer.pal(11, "RdGy")) +
  facet_grid(var ~ site, labeller = global_labeller) +
  theme_gcb_x2() +
  theme(legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.25, "cm"))

plotweights(temp1) +
  facet_grid(var ~ site, labeller = global_labeller) +
  theme_gcb_x2() +
  theme(legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.25, "cm")) +
  coord_fixed()

lim <- max(abs(temp1$ModelBeta))
plotbetas_fc(temp1) +
  facet_grid(var ~ site, labeller = global_labeller) +
  theme_gcb_x2() +
  theme(legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.25, "cm")) +
  scale_fill_gradientn(colours = brewer.pal(11, "PiYG"),
                       trans = sqrt_sign_trans(),
                       limits = c(-lim, lim)) +
  coord_fixed()


# ---- fert_plots_combined ------------------------------------------------

# Plots
# Plot delta AIC
temp1 <- fert_set %>%
  select(site, datasets) %>%
  unnest() %>%
  # filter(var != "dmi") %>%
  mutate(deltaAICc_orig = deltaAICc)

temp2 <- fert_set %>%
  select(site, pcs) %>%
  unnest() %>%
  # filter(climate != "dmi") %>%
  rename(var = climate)

temp1 <- inner_join(temp1, temp2)

if (length(which(temp1$deltaAICc < -7) > 0)) {
  temp1[which(temp1$deltaAICc < -7), ]$deltaAICc <- -7
}
if (length(which(temp1$deltaAICc > 7) > 0)) {
  temp1[which(temp1$deltaAICc > 7), ]$deltaAICc <- 7
}

if (length(which(temp1$ModelBeta < -10) > 0)) {
  temp1[which(temp1$ModelBeta < -10), ]$ModelBeta <- -10
}
if (length(which(temp1$ModelBeta > 10) > 0)) {
  temp1[which(temp1$ModelBeta > 10), ]$ModelBeta <- 10
}

ann_text <- distinct(temp1, site, var, pc)
ann_text$sig <- ifelse(ann_text$pc < 0.5, "sig", "ns")
ann_text$sig <- factor(ann_text$sig, levels = c("ns", "sig"))

ggplot(data = temp1, aes(x = WindowClose, y = WindowOpen)) +
  geom_tile(aes(fill = deltaAICc), color = "white", size = 0.1) +
  theme_gcb_x2() +
  facet_grid(var ~ site, labeller = global_labeller) +
  theme(legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.25, "cm"),
        plot.title = element_text(hjust = 0),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5)) +
  labs(x = "Window close (months before census)",
       y = "Window open (months before census)",
       caption = expression(paste(Delta, "AICc (compared to null model)"))) +
  scale_fill_gradientn(colours = brewer.pal(11, "RdGy"),
                       limits = c(-7, 7), breaks = seq(-6, 6, by = 3),
                       name = "") +
  coord_fixed() +
  scale_x_continuous(breaks = seq(0, 24, by = 6)) +
  scale_y_continuous(breaks = seq(0, 24, by = 6)) +
  geom_text(data = ann_text, x = 17, y = 6,
            aes(label = paste0("p[c] == ", formatC(round(pc, 2), digits = 2, format = "f")),
                color = sig), parse = TRUE) +
  scale_color_manual(guide = FALSE, values = c("black", "red"), drop = FALSE)

ggsave("~/Desktop/Fig3_fertility_aic_plots.pdf",
       width = 11, height = 7.5, units = "in")


# ---- crosswin -----------------------------------------------------------

crosswin_all <- function(temp_c, temp_b) {
  r1 <- crosswin_models(temp_c, temp_b, "rain_monthly_mm", "tavg_anomaly")
  r2 <- crosswin_models(temp_c, temp_b, "rain_monthly_mm", "nino3.4")
  r3 <- crosswin_models(temp_c, temp_b, "rain_monthly_mm", "dmi")
  r4 <- crosswin_models(temp_c, temp_b, "tavg_anomaly", "nino3.4")
  r5 <- crosswin_models(temp_c, temp_b, "tavg_anomaly", "dmi")
  r6 <- crosswin_models(temp_c, temp_b, "nino3.4", "dmi")

  res <- bind_rows(r1, r2, r3, r4, r5, r6)
  return(res)
}

crosswin_models <- function(temp_c, temp_b, v1, v2) {
  cw <- crosswin(xvar = temp_c[, v1],
                 xvar2 = temp_c[, v2],
                 cdate = temp_c$chr_date,
                 bdate = temp_b$chr_date,
                 cinterval = "month",
                 range = c(24, 0),
                 type = "absolute",
                 refday = c(01, 01),
                 stat = "mean",
                 stat2 = "mean")

  cw$v1 <- v1
  cw$v2 <- v2

  return(cw)
}

cw <- surv_set %>%
  filter(age_class == "adult") %>%
  mutate(crosswin = purrr::map2(climate, trials, crosswin_all)) %>%
  select(site, age_class, crosswin) %>%
  unnest()


cw$v1 <- revalue(cw$v1, c("rain_monthly_mm" = "Rainfall",
                          "tavg_anomaly" = "Temperature",
                          "nino3.4" = "ENSO Index",
                          "dmi" = "IOD Index"))

cw$v1 <- factor(cw$v1,
                levels = c("Rainfall", "Temperature", "ENSO Index", "IOD Index"))

cw$v2 <- revalue(cw$v2, c("rain_monthly_mm" = "Rainfall",
                          "tavg_anomaly" = "Temperature",
                          "nino3.4" = "ENSO Index",
                          "dmi" = "IOD Index"))

cw$v2 <- factor(cw$v2,
                levels = c("Rainfall", "Temperature", "ENSO Index", "IOD Index"))

# plotcor_fc(filter(temp, v1 == "Rainfall"), type = "C") +
#   theme_gcb() +
#   facet_grid(v2 ~ site, labeller = global_labeller) +
#   scale_fill_gradientn(colours = brewer.pal(11, "BrBG"), name = "",
#                        lim = c(-1, 1)) +
#   theme(legend.key.width = unit(1.5, "cm"),
#         legend.key.height = unit(0.25, "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   labs(title = "")
#
# plotcor_fc(filter(temp, v1 == "Temperature"), type = "C") +
#   theme_gcb() +
#   facet_grid(v2 ~ site, labeller = global_labeller) +
#   scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")), name = "",
#                        lim = c(-1, 1)) +
#   theme(legend.key.width = unit(1.5, "cm"),
#         legend.key.height = unit(0.25, "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   labs(title = "")
#
# plotcor_fc(filter(temp, v1 == "ENSO Index"), type = "C") +
#   theme_gcb() +
#   facet_grid(v2 ~ site, labeller = global_labeller) +
#   theme(legend.key.width = unit(1.5, "cm"),
#         legend.key.height = unit(0.25, "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   labs(title = "")

p <- list()
for (i in seq_along(site_list)) {
  p[[i]] <- plotcor_fc(filter(cw, site == site_list[i]), type = "C") +
    theme_gcb_x2() +
    facet_grid(v1 ~ v2) +
    theme(legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.25, "cm"),
          plot.title = element_text(hjust = 0),
          plot.caption = element_text(size = 16, hjust = 0.5)) +
    labs(title = site_map[site_list[i]],
         subtitle = "Weather variable pairwise correlations",
         caption = "Pearson correlation coefficient")
}

for (i in seq_along(p)) {
  p_temp <- p[[i]]

  ggsave(path = "plots/var_cor_plots",
         filename = paste0("FigS", i + 3, "_", site_map[site_list[i]], "_var_cor.pdf"),
         plot = p_temp, width = 4.5, height = 4.5, units = "in")
}

p_temp <- cowplot::plot_grid(plotlist = p, ncol = 2, scale = 0.95,
                   labels = paste0("(", letters[1:7], ")"))

ggsave(path = "plots/var_cor_plots",
       filename = "var_cor.pdf",
       plot = p_temp, width = 11, height = 12, units = "in")


# ---- autowin ------------------------------------------------------------

bluem <- filter(fert_set, site == "kakamega")
bluem$trials
bluem_c <- bluem$climate[[1]] %>%
  filter(year_of >= 1978) %>%
  select(date_of, dmi, nino3.4, dmi, rain_monthly_mm, tavg_anomaly)
bluem_b <- data_frame(refdate = unique(bluem$trials[[1]]$date_of))

slice_clim_avg <- function(cl, bi, wopen, wclose) {

  res <- bi %>%
    mutate(wopen = refdate - months(wopen),
           wclose = refdate - months(wclose))

  bc <- list()
  for (i in 1:nrow(res)) {
    temp <- cl[cl$date_of %within% interval(res$wopen[i], res$wclose[i]), ]
    temp <- temp %>%
      summarise_each(funs(mean), -date_of)

    temp$refdate <- bi$refdate[i]

    bc[[i]] <- temp
  }

  bc <- bind_rows(bc)

  bc <- bc %>%
    gather(var, mean_val, -refdate)

  return(bc)

}

blue_p1 <- slice_clim_avg(bluem_c, bluem_b, 20, 18)
blue_p2 <- slice_clim_avg(bluem_c, bluem_b, 4, 3)

blue_p1_p2 <- inner_join(blue_p1, blue_p2, by = c("refdate", "var"))
blue_p1_p2 %>%
  group_by(var) %>%
  summarise(cor = cor(mean_val.x, mean_val.y))


# ---- all_together? ------------------------------------------------------


for (i in seq_along(p)) {

  # title <- cowplot::ggdraw() + cowplot::draw_label(site_map[site_list[i]], fontface='bold')

  p_nest <- cowplot::plot_grid(plotlist = list(p_fert_delta[[i]], p_fert_beta[[i]]),
                               # labels = paste0("(", letters[3:4], ")"),
                               labels = c("(c)", "(d)"),
                               nrow = 2)

  p_temp <- cowplot::plot_grid(plotlist = list(p_surv_delta[[i]], p_surv_beta[[i]],
                                               p_nest, p[[i]]),
                               labels = c("(a)", "(b)", "", "(e)"),
                               ncol = 2,
                               scale = 0.95,
                               rel_heights = c(1, 1.25))

  # p_combined <- cowplot::plot_grid(title, p_temp, ncol = 1,
  #                                  rel_heights = c(0.025, 1))


  ggsave(path = "plots/combined_plots",
         filename = paste0("FigS", i + 3, "_",
                           str_replace_all(tolower(site_map[site_list[i]]), " ", ""),
                           "_combined.pdf"),
         plot = p_temp, width = 14, height = 16.5, units = "in")
}



# ---- explore_results ----------------------------------------------------

surv_df <- surv_set %>%
  select(site, age_class, datasets) %>%
  unnest() %>%
  select(site, age_class, deltaAICc)

nrow(surv_df[surv_df$deltaAICc >= 0, ]) / nrow(surv_df)
nrow(surv_df[surv_df$deltaAICc < -3, ]) / nrow(surv_df)

nrow(surv_df[surv_df$deltaAICc < 0, ])

# Check results
surv_set %>% select(site, age_class, pcs) %>% unnest() %>% arrange(pc)

# Sifaka newborn
surv_set[18, ]$datasets[[1]] %>% arrange(deltaAICc)
climwin::plotweights(surv_set[18, ]$models[[1]][[3]]$Dataset, cw2 = 0.75)
climwin::plotwin(surv_set[18, ]$models[[1]][[3]]$Dataset, cw = 0.75)

# Baboon adult
surv_set[4, ]$datasets[[1]] %>% arrange(deltaAICc)
climwin::plotweights(surv_set[4, ]$models[[1]][[3]]$Dataset, cw2 = 0.75)
climwin::plotwin(surv_set[4, ]$models[[1]][[3]]$Dataset, cw = 0.75)


fert_df <- fert_set %>%
  select(site, age_class, datasets) %>%
  unnest() %>%
  select(site, age_class, deltaAICc)

nrow(fert_df[fert_df$deltaAICc >= 0, ]) / nrow(fert_df)
nrow(fert_df[fert_df$deltaAICc < -3, ]) / nrow(fert_df)

nrow(fert_df[fert_df$deltaAICc < 0, ])

# Check results
fert_set %>% select(site, age_class, pcs) %>% unnest() %>% arrange(pc)

# Sifaka DMI
climwin::plotweights(fert_set[6, ]$models[[1]][[2]]$Dataset)
climwin::plotwin(fert_set[6, ]$models[[1]][[2]]$Dataset, cw = 0.75)
fert_set[6, ] %>%
  select(site, age_class, datasets) %>%
  unnest() %>%
  select(1:6, var) %>%
  filter(var == "dmi") %>%
  arrange(deltaAICc)

# Blue monkey rain
climwin::plotweights(fert_set[3, ]$models[[1]][[3]]$Dataset)
climwin::plotwin(fert_set[3, ]$models[[1]][[3]]$Dataset, cw = 0.60)
fert_set[3, ] %>%
  select(site, age_class, datasets) %>%
  unnest() %>%
  select(1:6, var) %>%
  # filter(var == "rainfall") %>%
  arrange(deltaAICc)

# Muriqui nino
climwin::plotweights(fert_set[1, ]$models[[1]][[1]]$Dataset)
climwin::plotwin(fert_set[1, ]$models[[1]][[1]]$Dataset, cw = 0.75)
fert_set[1, ] %>%
  select(site, age_class, datasets) %>%
  unnest() %>%
  select(1:6, var) %>%
  filter(var == "index_nino3.4") %>%
  arrange(deltaAICc) %>%
  View()


# ---- climatic_variability -----------------------------------------------

cv <- function(x) {
  cv <- sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
  return(cv)
}

get_var <- function(df) {
  cov_df <- df %>%
    group_by(year_of) %>%
    summarise_each(funs(sd(., na.rm = TRUE), cv),
                   rain_anomaly, tavg_anomaly,
                   rain_monthly_mm, tavg_monthly) %>%
    select(-matches("anomaly_cv"))

  return(cov_df)
}

sites_cv <- clim_nest %>%
  mutate(cov_df = purrr::map(climate, get_var)) %>%
  select(site, cov_df) %>%
  unnest() %>%
  gather(var, value, -site, -year_of)

sites_cv$site <- factor(sites_cv$site, levels = site_list)
sites_cv$site <- plyr::revalue(sites_cv$site, site_map)

sites_cv$type <- if_else(str_detect(sites_cv$var, "anomaly"), "Anomaly", "Raw")
sites_cv$type <- factor(sites_cv$type, levels = c("Raw", "Anomaly"))
sites_cv <- separate(sites_cv, var, into = c("var", "measure"), sep = -4)
sites_cv$measure <- str_replace(sites_cv$measure, "_", "")
sites_cv$measure <- toupper(sites_cv$measure)
sites_cv$var <- str_replace_all(sites_cv$var, "_", "")

sites_cv$var <- forcats::fct_recode(sites_cv$var,
                                    "Rainfall" = "rainanomaly",
                                    "Temperature" = "tavganomaly",
                                    "Rainfall" = "rainmonthlymm",
                                    "Temperature" = "tavgmonthly")


cdat <- sites_cv %>%
  group_by(site, var, type, measure) %>%
  summarise(cv_mean = mean(value, na.rm = TRUE),
            n = n())


sites_cv <- inner_join(sites_cv, select(births_mean_vec,
                                        site = Study.Id, distance))

sites_cv_sum <- sites_cv %>%
  # filter(measure == "CV") %>%
  group_by(site, var, measure, type) %>%
  summarise(iqr = IQR(value, na.rm = TRUE),
            distance = first(distance))

ggplot(filter(sites_cv_sum, type == "Anomaly"),
       aes(x = forcats::fct_reorder(site, distance, .desc = TRUE), y = iqr, fill = distance)) +
  geom_col(color = "black") +
  facet_wrap(~var, scales = "free_x", ncol = 1) +
  theme_gcb_x2() +
  scale_fill_gradientn(colours = rev(viridis(11, option = "plasma")),
                       # name = "Degree of breeding seasonality",
                       name = "", limits = c(0, 1), breaks = c(0, 1),
                       labels = c("Less Breeding Seasonality", "More Breeding Seasonality")) +
  coord_flip() +
  theme(legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(1.5, "cm"))

# ggplot(sites_cv, aes(x = value, fill = site, color = site)) +
#   geom_density(alpha = 0.3) +
#   facet_wrap(~ var, scales = "free") +
#   geom_vline(data = cdat, aes(xintercept = cv_mean,  color = site),
#              linetype = 2, size = 0.5) +
#   theme_gcb_x2() +
#   labs(x = "Annual Coefficient of Variation", y = "Density") +
#   scale_fill_brewer(palette = "Dark2") +
#   scale_color_brewer(palette = "Dark2")
#
#
# p1 <- ggplot(filter(sites_cv, type == "Raw"),
#              aes(x = year_of, y = value)) +
#   geom_point(aes(fill = distance, color = distance), shape = 21, size = 1,
#              alpha = 0.7, stroke = 0.25) +
#   facet_grid(var ~ site, scales = "free_y") +
#   geom_hline(data = filter(cdat, type == "Raw"),
#              aes(yintercept = cv_mean),
#              linetype = 2, color = "black") +
#   theme_gcb() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   scale_color_gradientn(colours = rev(viridis(11, option = "plasma")),
#                         name = "Degree of breeding seasonality",
#                         limits = c(0, 1)) +
#   scale_fill_gradientn(colours = rev(viridis(11, option = "plasma")),
#                        name = "Degree of breeding seasonality",
#                        limits = c(0, 1)) +
#   theme(legend.key.height = unit(0.25, "cm"),
#         legend.key.width = unit(1.5, "cm")) +
#   labs(y = "Intra-annual Variability", x = "Year",
#        title = "Raw Values")
#
# p2 <- ggplot(filter(sites_cv, type == "Anomaly"),
#              aes(x = year_of, y = value)) +
#   geom_point(aes(fill = distance, color = distance), shape = 21, size = 1,
#              alpha = 0.7, stroke = 0.25) +
#   facet_grid(var ~ site, scales = "free_y") +
#   geom_hline(data = filter(cdat, type == "Anomaly"),
#              aes(yintercept = cv_mean),
#              linetype = 2, color = "black") +
#   theme_gcb() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   scale_color_gradientn(colours = rev(viridis(11, option = "plasma")),
#                         name = "Degree of breeding seasonality",
#                         limits = c(0, 1)) +
#   scale_fill_gradientn(colours = rev(viridis(11, option = "plasma")),
#                        name = "Degree of breeding seasonality",
#                        limits = c(0, 1)) +
#   theme(legend.key.height = unit(0.25, "cm"),
#         legend.key.width = unit(1.5, "cm")) +
#   labs(y = "Intra-annual Variability", x = "Year",
#        title = "Anomalies")

p1 <- ggplot(filter(sites_cv, var == "Rainfall"),
       aes(x = type, y = value)) +
  geom_point(aes(fill = distance, color = distance), shape = 21, size = 1,
             alpha = 0.7, stroke = 0.25,
             position = position_jitter(width = 0.15, height = 0)) +
  facet_grid(measure ~ site, scales = "free_y") +
  geom_point(data = filter(cdat, var == "Rainfall"), aes(y = cv_mean),
             shape = 21, color = "black", fill = "white", size = 2) +
  theme_gcb() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_color_gradientn(colours = rev(viridis(11, option = "plasma")),
                        # name = "Degree of breeding seasonality",
                        name = "", limits = c(0, 1), breaks = c(0, 1),
                        labels = c("Less Breeding Seasonality", "More Breeding Seasonality")) +
  scale_fill_gradientn(colours = rev(viridis(11, option = "plasma")),
                       # name = "Degree of breeding seasonality",
                       name = "", limits = c(0, 1), breaks = c(0, 1),
                       labels = c("Less Breeding Seasonality", "More Breeding Seasonality")) +
  theme(legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(1.5, "cm")) +
  labs(x = "", y = "SD of Monthly Rainfall (mm)",
       title = "Rainfall Variability")

p2 <- ggplot(filter(sites_cv, var == "Temperature"),
       aes(x = type, y = value)) +
  geom_point(aes(fill = distance, color = distance), shape = 21, size = 1,
             alpha = 0.7, stroke = 0.25,
             position = position_jitter(width = 0.15, height = 0)) +
  facet_grid(measure ~ site, scales = "free_y") +
  geom_point(data = filter(cdat, var == "Temperature"), aes(y = cv_mean),
             shape = 21, color = "black", fill = "white", size = 2) +
  theme_gcb() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_color_gradientn(colours = rev(viridis(11, option = "plasma")),
                        # name = "Degree of breeding seasonality",
                        name = "", limits = c(0, 1), breaks = c(0, 1),
                        labels = c("Less Breeding Seasonality", "More Breeding Seasonality")) +
  scale_fill_gradientn(colours = rev(viridis(11, option = "plasma")),
                       # name = "Degree of breeding seasonality",
                       name = "", limits = c(0, 1), breaks = c(0, 1),
                       labels = c("Less Breeding Seasonality", "More Breeding Seasonality")) +
  theme(legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(1.5, "cm")) +
  labs(x = "", y = expression(paste("SD of Monthly Temperature (", degree~C, ")")),
       title = "Temperature Variability")

cowplot::plot_grid(p1, p2, labels = c("(a)", "(b)"), scale = 0.95, ncol = 1)

# get_cov_monthly <- function(df) {
#   cov_df <- df %>%
#     group_by(month_of) %>%
#     summarise_each(funs(cv), rain_monthly_mm, tavg_monthly)
#
#   return(cov_df)
# }
#
# sites_cv_monthly <- clim_nest %>%
#   mutate(cov_df = purrr::map(climate, get_cov_monthly)) %>%
#   select(site, cov_df) %>%
#   unnest() %>%
#   gather(var, value, -site, -month_of)
#
# sites_cv_monthly$site <- factor(sites_cv_monthly$site, levels = site_list)
# sites_cv_monthly$site <- plyr::revalue(sites_cv_monthly$site, site_map)
# sites_cv_monthly$var <- forcats::fct_recode(sites_cv_monthly$var, Rainfall = "rain_monthly_mm",
#                                             Temperature = "tavg_monthly")
#
# cdat <- sites_cv_monthly %>%
#   group_by(site, var) %>%
#   summarise(cv_mean = mean(value, na.rm = TRUE),
#             n = n())