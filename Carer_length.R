scrubbed_data_file <- 'P:\\suffolk-scrubbed-episodes-20201203.csv'
data <- read.csv(scrubbed_data_file)

data$new_carer <- case_when(
  data$RNE == "S" ~ 1,
  data$RNE == 'L' ~ 0,
  data$RNE == 'P' ~ 1,
  data$RNE == 'T' ~ 0,
  TRUE ~ 0
)
tab <- data %>%
  mutate(report_date = ymd(report_date), ceased = ymd(ceased)) %>%
  group_by(period_id) %>%
  mutate(carer_number = cumsum(new_carer)) %>%
  group_by(period_id, carer_number) %>%
  summarise(carer_duration = day_diff(min(report_date), max(ceased)))
x <- dcast(period_id ~ carer_number, data = tab)
tab %>%
  filter(carer_number < 10 & carer_number > 0) %>%
  mutate(carer_number = factor(carer_number)) %>%
  ggplot(aes(carer_duration, colour = carer_number)) +
  geom_density() +
  facet_grid(rows = vars(carer_number))
