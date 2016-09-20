fert_durations <- fert %>%
  filter(!(Study.Id == "kakamega" & year(Start.Date) < 1997)) %>%
  group_by(Study.Id) %>%
  summarise(min_start = min(Start.Date),
            max_end = max(Stop.Date),
            n = n()) %>%
  mutate(dur = as.duration(max_end - min_start) / dyears(1))

fert_durations <- fert %>%
  filter(!(Study.Id == "kakamega" & year(Start.Date) < 1997)) %>%
  group_by(Study.Id) %>%
  summarise(n = n_distinct(Animal.Id)) %>%
  right_join(fert_durations)

fert_individ_years <- fert %>%
  filter(!(Study.Id == "kakamega" & year(Start.Date) < 1997)) %>%
  mutate(dur = Stop.Date - Start.Date) %>%
  group_by(Study.Id, Animal.Id) %>%
  summarise(ind_dur = sum(dur)) %>%
  ungroup %>%
  group_by(Study.Id) %>%
  summarise(fert_yrs = sum(ind_dur)) %>%
  mutate(fert_yrs = fert_yrs / dyears(1)) %>%
  inner_join(fert_durations)



study_durations <- lh %>%
  group_by(Study.Id) %>%
  dplyr::summarise(min_entry = min(Entry.Date),
                   max_depart = max(Depart.Date),
                   n_rec = n()) %>%
  mutate(dur = as.duration(max_depart - min_entry))

lh %>%
  mutate(dur = Depart.Date - Entry.Date) %>%
  group_by(Study.Id, Animal.Id) %>%
  summarise(ind_dur = sum(dur)) %>%
  ungroup() %>%
  group_by(Study.Id) %>%
  summarise(lh_yrs = sum(ind_dur)) %>%
  mutate(lh_yrs = lh_yrs / dyears(1)) %>%
  inner_join(study_durations) %>%
  mutate(dur = dur / dyears(1))