devtools::install_github("camposfa/plhdbR")


library(plhdbR)
library(ggplot2)
library(htmlTable)
Sys.setenv(TZ = 'UTC')

load(".RData")
`%ni%` = Negate(`%in%`)

load_plhdb_packages()



# ---- climate ------------------------------------------------------------

mei <- load_climate_index("mei")
oni <- load_climate_index("oni")
mei <- load_climate_index("mei")
soi <- load_climate_index("soi")
dmi <- load_climate_index("dmi")

indices <- load_climate_index(c("nao", "pdo"))


f <- "data/biography_2015_05_20.csv"
lh <- read_bio_table(f)
summary(lh)

f <- "data/fertility_2015_05_20.csv"
fert <- read_fert_table(f)
summary(fert)


# ---- fix_errors ---------------------------------------------------------

lh[lh$Animal.Id == "247" & lh$Study.Id == "beza", ]$Entry.Date <- ymd("1984-07-15")

b <- lh

m <- stage_specific_survival(b)
summary(m)

ggplot(m, aes(x = year_of, y = s)) +
  geom_line() +
  facet_grid(Study.Id ~ age_class) +
  theme_bw()


temp <- m %>%
  ungroup() %>%
  rename(site = Study.Id, weighted_prob_of_survival = s) %>%
  mutate(site = as.character(site)) %>%
  arrange(site, age_class, year_of)

write.csv(temp, "data/multiyear_survivorship.csv", row.names = FALSE)

htmlTable(txtRound(temp, 2, excl.cols = c(1:4, 7:8)),
          col.rgroup = c("none", "#F7F7F7"),
          rnames = FALSE,
          align = "lclcrrcc")


# more --------------------------------------------------------------------



filter(lh, as.Date(Max.Birth.Date) > Sys.Date())

filter(lh, as.Date(Entry.Date) > Sys.Date())
filter(lh, as.Date(Depart.Date) > Sys.Date())
filter(lh, as.Date(Min.Birth.Date) > Sys.Date())
filter(lh, as.Date(Birth.Date) > Sys.Date())
filter(lh, as.Date(Max.Birth.Date) > Sys.Date())

# Duplicate animals
lh %>%
  group_by(Study.Id, Animal.Id) %>%
  summarise(n_records = n()) %>%
  filter(n_records > 1)

realistic_dates <- new_interval(ymd_hms("1914-01-01 00:00:00"), Sys.time())

temp <- lh %>%
  filter(Birth.Date %within% realistic_dates &
           Min.Birth.Date %within% realistic_dates &
           Max.Birth.Date %within% realistic_dates &
           Entry.Date %within% realistic_dates &
           Depart.Date %within% realistic_dates)

lh %>%
  anti_join(temp) %>%
  data.frame()

temp <- find_fert_errors(fert)


# ---- find_errors --------------------------------------------------------

find_bio_errors(lh)

# Currently an error for animal AKI
# Look at AKI's data
lh %>% filter(Animal.Id == "AKI") %>% glimpse()

# Fix error
lh[lh$Animal.Id == "AKI", ]$Max.Birth.Date <- ymd("2007-11-26")

# Check again
find_bio_errors(lh)

# Find mom errors
find_mom_id_errors(lh)


temp <- lh %>%
  group_by(Study.Id) %>%
  do(
    inds = levels(factor(.$Animal.Id)),
    moms = levels(factor(.$Mom.Id))
  ) %>%
  do(
    missing = .$moms[.$moms %ni% .$inds]
  )

temp <- lh %>%
  group_by(Study.Id) %>%
  do(
    inds = levels(factor(.$Animal.Id)),
    moms = levels(factor(.$Mom.Id))
  )

names(temp$missing) <- levels(factor(lh$Study.Id))

temp <- combine(temp)




for(i in 1:7){
  temp <- filter(lh, Study.Id)
  inds <- levels(factor(lh$Animal.Id))
  moms <- levels(factor(lh$Mom.Id))

}


# ---- vital_rates --------------------------------------------------------

t <- age_specific_fertility(lh, fert)
