devtools::install_github("camposfa/plhdbR")

library(ggplot2)
library(lme4)
library(plhdbR)

Sys.setenv(TZ = 'UTC')

load(".RData")
`%ni%` = Negate(`%in%`)

load_plhdb_packages()

f <- "data/biography_2015_06_17.csv"
lh <- read_bio_table(f)
summary(lh)



# Fix errors

find_bio_errors(lh)

# Duplicate entries for Karisoke "SUS"
lh <- lh %>%
  filter(!(Animal.Id == "SUS" & (year(Birth.Date) == 2014 | year(Entry.Date) == 2014)))

# Date error for "TEKINF"
lh[lh$Animal.Id == "TEKINF", ]$Entry.Date <- ymd("2014-12-23")





f <- "data/fertility_2015_06_17.csv"
fert <- read_fert_table(f)
summary(fert)

find_fert_errors(fert)

# Takes about 1 minute
m <- stage_specific_survival(lh)
summary(m)

# Make trials
surv_trials <- make_survivorship_trials(m)


# Models
mod_df <- surv_trials %>%
  rename(site = Study.Id) %>%
  left_join(ann_mean) %>%
  gather(var, value, contains("mean"), shannon_rain)

mod <- mod_df %>%
  group_by(site, age_class, var) %>%
  do(climate_mod = glmer(fate ~ value + (1 | year_of), data = ., family = "binomial"))

mod_null <- mod_df %>%
  filter(var == "rain_monthly_mm_mean") %>%
  group_by(site, age_class) %>%
  do(null_mod = glmer(fate ~ 1 + (1 | year_of), data = ., family = "binomial"))

mod_glance <- mod %>%
  do(glance(.$climate_mod))

mod_glance$site <- mod$site
mod_glance$age_class <- mod$age_class
mod_glance$var <- mod$var

# mod_sel <- mod %>%
#   do(site = first(.$site),
#      age_class = first(.$age_class),
#      sel = model.sel(.$climate_mod, .$null_mod)) %>%
#   mutate(site = unlist(site),
#          age_class = unlist(age_class))

mod_sel <- mod %>%
  group_by(site, age_class) %>%
  filter() %>%
  do(m_table = model.sel(.$climate_mod),
     vars = data.frame(var = .$var))


# models <- list()
# count <- 1
#
# for(i in 1:length(levels(temp$Study.Id))){
#
#   current_study <- levels(temp$Study.Id)[i]
#
#   for(j in 1:length(levels(factor(temp$age_class)))){
#     current_ac <- levels(temp$age_class)[j]
#
#     set <- temp %>%
#       filter(Study.Id == current_study & age_class == current_ac) %>%
#       data.frame()
#
#     models[[count]] <- glmer(fate ~ 1 + (1 | year_of), data = set, family = "binomial")
#
#     count <- count + 1
#   }
#
# }
#
# out <- models[[19]]
#
# x=coef(out)$year_of
# x=as.vector(x[,1])
# x=exp(x)/(1+exp(x))
