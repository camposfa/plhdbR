var_map <- c(
  rainfall = "Rainfall",
  mean_temp = "Temperature",
  index_nino3.4 = "ENSO Index",
  spei_03 = "Drought Index",
  dmi = "IOD Index",
  dmi_spc = "IOD Index (spc)",
  enso_spc = "ENSO Index (spc)"
)

var_map2 <- c("rain_monthly_mm" = "Rainfall",
              "tavg_anomaly" = "Temperature",
              "nino3.4" = "ENSO Index",
              "dmi" = "IOD Index")

term_map <- c(
  age_classjuvenile = "Juveniles vs. Adults",
  age_classnewborn = "Newborns vs. Adults",
  `value` = "Climate Variable",
  `value:age_classjuvenile` = "Juvelines:Climate",
  `value:age_classnewborn` = "Newborns:Climate"
)

site_map <- c(
  beza = "Sifaka",
  ssr = "Capuchin",
  `rppn-fma` = "Muriqui",
  kakamega = "Blue Monkey",
  amboseli = "Baboon",
  gombe = "Chimpanzee",
  karisoke = "Gorilla"
)

quarter_map <- c(
  annual = "Calendar Y",
  coldest_q = "Coldest Q",
  driest_q = "Driest Q",
  warmest_q = "Warmest Q",
  wettest_q = "Wettest Q"
)

age_map <- c(
  newborn = "Infant",
  juvenile = "Juvenile",
  adult = "Adult",
  combined = "Age Classes Combined"
)

global_labeller <- labeller(
  age_class = age_map,
  var = var_map,
  term = term_map,
  site = site_map,
  grp = site_map,
  quarter = quarter_map
)
