library(circular)

births_df <- lh %>%
  select(Study.Id, Animal.Id, Birth.Date, Sex) %>%
  mutate(y_date = decimal_date(Birth.Date) - year(Birth.Date),
         date_rad = y_date * 2 * pi,
         date_deg = y_date * 360)

births_df$plot_date <- ymd(paste("2000", month(births_df$Birth.Date),
                                day(births_df$Birth.Date),
                               sep = "-"))

births_df$Sex <- mapvalues(births_df$Sex, from = c("M", "F", "U"),
                         to = c("Male", "Female", "Unknown"))

# Sexes combined combined
temp <- births_df %>%
  group_by(Study.Id) %>%
  do(c = circular(.$date_rad))

temp1 <- temp %>%
  do(r = rayleigh.test(.$c))

births_df <- births_df %>%
  mutate(tally = 1) %>%
  group_by(Study.Id, plot_date) %>%
  mutate(counter = cumsum(tally))


b <- seq(ymd('2000-01-15'),ymd('2000-12-15'), by = '1 month')

ggplot(births_df, aes(x = plot_date, y = 1)) +
  geom_hline(yintercept = 1, size = 0.5, color = "gray50") +
  geom_point(size = 3.5, position = position_jitter(width = 0, height = 0.05),
             alpha = 0.2) +
  scale_x_datetime(breaks = b,
                   minor_breaks = date_breaks(width = "1 month"),
                   labels = date_format("%b"),
                   limits = c(ymd("2000-01-01"), ymd("2000-12-31"))) +
  scale_y_continuous(limits = c(0, 1.1)) +
  coord_polar() +
  theme_bw() +
  facet_wrap(~Study.Id) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(color = "grey90"))