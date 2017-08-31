f_by_year <- f %>%
  filter(!(site == "kakamega" & year_of < 1997)) %>%
  group_by(site) %>%
  mutate(sum_female_years = cumsum(female_years))

f_by_year$site <- revalue(f_by_year$site, site_map)

temp <- f_by_year %>%
  filter(year_of == 2013)

p3 <- ggplot(f_by_year,
# p3 <- ggplot(filter(f_by_year, site %in% c("Gorilla", "Chimpanzee")),
       aes(x = year_of, y = female_years)) +
  geom_line(aes(color = site, group = site)) +
  ggrepel::geom_text_repel(data = temp,
  # ggrepel::geom_text_repel(data = filter(temp, site %in% c("Gorilla", "Chimpanzee")),
                           aes(x = year_of, y = female_years,
                               label = site, color = site),
                           nudge_x = 6, segment.size = NA, show.legend = FALSE) +
  theme_journal_x2() +
  coord_cartesian(xlim = c(1963, 2022)) +
  scale_color_brewer(palette = "Dark2", guide = FALSE) +
  labs(x = "Year", y = "Annual individual-years\nof fertility data")

p1 <- ggplot(f_by_year,
# p1 <- ggplot(filter(f_by_year, site %in% c("Gorilla", "Chimpanzee")),
             aes(x = year_of, y = sum_female_years)) +
  geom_line(aes(color = site, group = site)) +
  ggrepel::geom_text_repel(data = temp,
  # ggrepel::geom_text_repel(data = filter(temp, site %in% c("Gorilla", "Chimpanzee")),
                           aes(x = year_of, y = sum_female_years,
                               label = site, color = site),
                           nudge_x = 6, segment.size = NA, show.legend = FALSE) +
  theme_journal_x2() +
  coord_cartesian(xlim = c(1963, 2022)) +
  scale_color_brewer(palette = "Dark2", guide = FALSE) +
  labs(x = "Year", y = "Cummulative individual-years")


f1 <- f
f1$site <- revalue(f1$site, site_map)

temp <- f1 %>%
  filter(year_of == 2013)

p2 <- ggplot(f1,
# p2 <- ggplot(filter(f1, site %in% c("Gorilla", "Chimpanzee")),
              aes(x = year_of, y = n_animals)) +
  geom_line(aes(color = site, group = site)) +
  ggrepel::geom_text_repel(data = temp,
  # ggrepel::geom_text_repel(data = filter(temp, site %in% c("Gorilla", "Chimpanzee")),
                           aes(x = year_of, y = n_animals,
                               label = site, color = site),
                           nudge_x = 6, segment.size = NA, show.legend = FALSE) +
  theme_journal_x2() +
  labs(x = "Year", y = "# Adult Females") +
  scale_color_brewer(palette = "Dark2", guide = FALSE) +
  coord_cartesian(xlim = c(1963, 2022))

cowplot::plot_grid(p2, p3, p1, ncol = 1, align = "hv")













apes <- filter(fert, Study.Id %in% c("karisoke", "gombe"))
apes <- apes %>%
  group_by(Study.Id, Animal.Id) %>%
  summarise(first_start = min(Start.Date)) %>%
  inner_join(apes)

apes$is_break <- 1

apes <- apes %>%
  group_by(Study.Id, Animal.Id) %>%
  mutate(is_break = lead(is_break))

apes <- apes %>%
  ungroup() %>%
  group_by(Study.Id) %>%
  mutate(Animal.Id = fct_reorder(Animal.Id, first_start))

ggplot() +
  geom_segment(data = apes, aes(x = Animal.Id,
                   xend = Animal.Id,
                   y = Start.Date, yend = Stop.Date),
               position = position_jitter(height = 0.1, width = 0)) +
  geom_point(data = filter(apes, !is.na(is_break)),
             aes(x = fct_reorder(Animal.Id, first_start),
                 y = Stop.Date), color = "red",
             size = 1) +
  theme_journal() +
  facet_wrap(~Study.Id, scales = "free") +
  coord_flip()




# ---- break_time ---------------------------------------------------------

brks <- apes %>%
  group_by(Study.Id, Animal.Id) %>%
  mutate(d_diff = as.numeric(lead(Start.Date) - Stop.Date)) %>%
  ungroup() %>%
  filter(!is.na(d_diff)) %>%
  group_by(Study.Id) %>%
  summarise(n = n(),
            total_time = sum(d_diff) / 365.25)
