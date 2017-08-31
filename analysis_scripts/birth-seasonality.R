source("~/Dropbox/R/theme_fte.R")
library(paceR)


# ---- circular_method ----------------------------------------------------

births <- filter(lh, Entry.Type == "B")

births$Study.Id <- factor(births$Study.Id, levels = site_list)
births$Study.Id <- plyr::revalue(births$Study.Id, site_map)

births <- births %>%
  mutate(month_of = as.character(month.abb[month(Birth.Date)]),
         year_of = year(Birth.Date),
         day_of = day(Birth.Date),
         yday_of = yday(Birth.Date),
         plot_date = ymd(paste(2000, month_of, day_of, sep = "-")),
         bin_week = round_date(plot_date, "week"))

births$month_of <- factor(births$month_of, levels = month.abb)

# If sampling equal numbers in all species
# births <- births %>%
#   group_by(Study.Id) %>%
#   sample_n(size = 198)

births_mean_vec <- births %>%
  group_by(Study.Id) %>%
  mutate(y_date = decimal_date(plot_date) - year(plot_date),
         date_deg = y_date * 360,
         weight = 1) %>%
  do(direction = vector.averaging(.$date_deg, .$weight)$direction,
     distance = vector.averaging(.$date_deg, .$weight)$distance)

births_mean_vec$direction <- unlist(births_mean_vec$direction)
births_mean_vec$distance <- unlist(births_mean_vec$distance)

births_monthly <- births %>%
  ungroup() %>%
  group_by(Study.Id, month_of) %>%
  summarise(n_births = n()) %>%
  ungroup() %>%
  group_by(Study.Id) %>%
  mutate(prop_births = n_births / sum(n_births))

births_circ <- births %>%
  group_by(Study.Id) %>%
  mutate(y_date = decimal_date(plot_date) - year(plot_date),
         date_deg = y_date * 360,
         weight = 1) %>%
  select(Study.Id, y_date, date_deg)

births_rayliegh <- births_circ %>%
  group_by(Study.Id) %>%
  mutate(date_rad = date_deg * pi / 180) %>%
  do(rt = circular::rayleigh.test(as.vector(.$date_rad)))

ggplot(births_monthly,
       aes(x = factor(month_of), y = prop_births, fill = prop_births)) +
  geom_bar(color = "black", stat = "identity", width = 0.7,
           size = 0.3) +
  facet_wrap(~Study.Id) +
  scale_fill_gradientn(colors = rev(viridis(11, option = "magma")),
                       # trans = sqrt_trans(),
                       guide = FALSE,
                       name = "",
                       limits = c(0, 1)) +
  theme_gcb() +
  theme(panel.background = element_rect(size = 0.5, color = "black", fill = NA),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(1.5, "cm")) +
  labs(y = "", x = "Proportion of births during month") +
  geom_hline(yintercept = 0.25, alpha = 0.1) +
  geom_hline(yintercept = 0.5, alpha = 0.1) +
  geom_hline(yintercept = 0.75, alpha = 0.1) +
  geom_hline(yintercept = 1, alpha = 0.1) +
  # scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  scale_y_sqrt(breaks = seq(0, 1, by = 0.25), limits = c(0, 1.25)) +
  coord_polar()

births_mean_vec$p.value <- births_rayliegh$rt %>%
  map_dbl("p.value") %>%
  round(3) %>%
  as.character()

births_mean_vec <- births_mean_vec %>%
  mutate(p.value = ifelse(p.value == "0", "p > 0.01", paste0("p = ", p.value)))

ggplot() +
  geom_segment(data = births_mean_vec,
               aes(x = direction, xend = direction, y = distance, yend = 0),
               color = "red", size = 1, alpha = 0.8) +
#   geom_segment(data = births_circ,
#                aes(x = date_deg, xend = date_deg, y = distance, yend = 0),
#                color = "blue", size = 1, alpha = 0.8) +
  coord_polar() +
  scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 30)) +
  facet_wrap(~Study.Id, ncol = 3) +
  theme_gcb() +
  theme(panel.background = element_rect(size = 0.5, color = "black"),
        axis.ticks.x = element_blank(),
        # axis.text.x = element_blank(),
        axis.text.x = element_text(color = "white"),
        panel.grid.major.x = element_line(color = "gray95")) +
  scale_y_sqrt(breaks = seq(0, 1, by = 0.25), limits = c(0, 1.25)) +
  labs(y = "", x = "") +
  geom_text(data = births_mean_vec, x = pi, y = -1.2,
           aes(label = paste0("r = ", round(distance, 2),
                              "                        ",
                              p.value)),
            size = 3, color = "red")



births_mean_vec <- births_mean_vec %>%
  mutate(a = format.Date(date_decimal(2000 + direction/360), "%d-%b"))


# Summarise top and bottom months

births_monthly %>%
  group_by(Study.Id) %>%
  top_n(3, prop_births) %>%
  summarise(btop_3 = round(sum(prop_births), 3) * 100)

births_monthly %>%
  group_by(Study.Id) %>%
  top_n(-3, prop_births) %>%
  summarise(bottom_3 = round(sum(prop_births), 3) * 100)


# ---- second_method ------------------------------------------------------

get_birth_season <- function(x) {

  s_date <- yday(x$a)[1]

  c50 <- nrow(x) / 2
  cc <- nrow(filter(x, yday_of == s_date))

  while (TRUE) {
    if (cc > c50) break

    cc <- cc + nrow(filter(x, yday_of == s_date))

    if (s_date == 0) {
      s_date <- 366
    }
    else {
      s_date <- s_date - 1
    }
  }

  r_date <- ymd(as.Date(s_date, origin = as.Date("1999-12-31")))
  return(r_date)

}



# Work backwards and find earliest date for which 50% of all births are between
# it and the circular average
temp2 <- inner_join(births, births_mean_vec)

temp2 <- temp2 %>%
  group_by(Study.Id) %>%
  do(r_date = get_birth_season(.)) %>%
  mutate(r_date = unlist(r_date),
         date_deg = (decimal_date(r_date) - year(r_date)) * 360,
         distance = 1)

ggplot() +
  geom_rug(data = births, aes(x = plot_date),
           alpha = 0.3, color = "black") +
  #   geom_density(data = births, aes(x = plot_date, y = ..scaled..),
  #                alpha = 0.5, adjust = 1/2, fill = "gray70") +
  geom_histogram(data = births, aes(x = plot_date),
                 color = "black", fill = "gray70", bins = 24, alpha = 0.8) +
  geom_segment(data = births_mean_vec, aes(x = a, xend = a, y = -Inf, yend = Inf),
               color = "red", size = 1) +
  geom_segment(data = temp2, aes(x = r_date, xend = r_date, y = -Inf, yend = Inf),
               color = "blue", size = 1) +
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") +
  facet_wrap(~Study.Id, scales = "free", ncol = 1) +
  theme_fc() +
  theme(legend.key.width = grid::unit(1.5, "cm"),
        panel.margin = unit(c(0.5), "cm")) +
  labs(x = "\nMonth", y = "Number of Births\n") +
  coord_cartesian(xlim = c(ymd("2000-01-17"), ymd("2000-12-16"))) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

ggplot() +
  geom_bar(data = births, aes(x = bin_week), stat = "count",
                 color = "black", fill = "gray70", alpha = 0.8) +
  facet_wrap(~Study.Id, scales = "free_y", ncol = 1) +
  theme_fc() +
  theme(legend.key.width = grid::unit(1.5, "cm"),
        panel.margin = unit(c(0.5), "cm")) +
  labs(x = "\nMonth", y = "Number of Births\n") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))


half_months <- c(ymd(paste("2000",
                     rep(1:12, each = 2),
                     rep(c(1, 16), times = 12),
                     sep = "-")),
                 ymd("2000-12-31"))

# Biweekly bins
get_bins <- function(df)
{
#   biweek_count = hist(df$plot_date, plot = FALSE,
#                                  breaks = seq(ymd("2000-01-01"),
#                                               ymd("2001-01-13"), "2 weeks"))$counts
#
#   res <- data.frame(biweek_start = seq(ymd("2000-01-01"),
#                                        ymd("2001-01-13"), "2 weeks")[1:26],
#                     biweek_count = biweek_count[1:26])

    biweek_count = hist(df$plot_date, plot = FALSE, breaks = half_months)$counts

    res <- data.frame(biweek_start = half_months[1:24],
                      biweek_count = biweek_count[1:24])

  return(tbl_df(res))
}

births_biweekly <- births %>%
  group_by(Study.Id) %>%
  do(get_bins(.))


ggplot() +
  geom_bar(data = births_biweekly, aes(x = biweek_start, y = biweek_count),
           stat = "identity",
           color = "black", fill = "gray70", alpha = 0.8) +
  facet_wrap(~Study.Id, scales = "free_y", ncol = 1) +
  theme_fc() +
  theme(legend.key.width = grid::unit(1.5, "cm"),
        panel.margin = unit(c(0.5), "cm")) +
  labs(x = "\nMonth", y = "Number of Births\n") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text.x = element_text(angle = 90)) +
#   scale_x_datetime(breaks = seq(ymd("2000-01-01"),
#                                 ymd("2001-01-13"), "2 weeks")[1:26],
    scale_x_datetime(breaks = half_months[1:24],
                   date_labels = "%b-%d")

births_biweekly %>%
  group_by(Study.Id) %>%
  summarise(min_biweek = min(biweek_count)) %>%
  inner_join(births_biweekly) %>%
  filter(min_biweek == biweek_count) %>%
  View()

# Start dates
# Muriqui 2000-01-16
# Baboon 2000-05-01
# Blue Monkey 2000-10-16
# Chimpanzee 2000-12-01
# Gorilla 2000-07-16
# Sifaka 2000-04-01
# Capuchin 2000-01-16

temp <- temp %>%
  ungroup() %>%
  complete(Study.Id, month_of, fill = list(n_births = 0, prop_births = 0))

temp %>%
  group_by(Study.Id) %>%
  summarise(min_month = min(n_births)) %>%
  left_join(temp) %>%
  filter(min_month == n_births)

# Start dates
# Muriqui 2000-02-01
# Baboon 2000-05-01
# Blue Monkey 2000-08-01
# Chimpanzee 2000-12-01
# Gorilla 2000-02-01
# Sifaka 2000-04-01
# Capuchin 2000-09-01