library(lubridate)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyquant)


theme_mastodon <- theme(plot.title = element_text(family = "OpenSans-Bold", hjust = 0.5, size = 20,
                                                  margin = margin(0,0,15,0)),
                        axis.title = element_text(family = "OpenSans-SemiBold", hjust = 0.5, size = 16),
                        axis.text = element_text(family = "OpenSans-Regular", hjust = 0.5, size = 10),
                        axis.text.x = element_text(angle = -45),
                        axis.title.x = element_text(margin = margin(15,0,0,0)),
                        axis.title.y = element_text(margin = margin(0,10,0,0)),
                        plot.margin = margin(10,20,10,10),
                        panel.background = element_blank(),
                        panel.grid = element_line(color = "#eeeeee"))

year_diff <- function(start, stop) {
  as.numeric(difftime(stop, start, units = "days")) %/% 365.25
}

following_monday <- function(x) {
  x - as.numeric(x-1+5)%%7 + 8
}

colours <- tableau_color_pal("Tableau 20")(20)

# Check historic episodes

episodes <- read.csv(scrubbed_episodes, header = TRUE, stringsAsFactors = FALSE, na.strings ="NA")
episodes$report_date <- ymd(episodes$report_date)
episodes$ceased <- ymd(episodes$ceased)

episodes %>%
  group_by(period_id) %>%
  summarise(join_date = min(report_date, na.rm = TRUE)) %>%
  mutate(month = floor_date(join_date, "3 months")) %>%
  group_by(month) %>%
  summarise(joiners = n()) %>%
  ggplot(aes(month, joiners)) +
  geom_line()

## Load simulated episodes

save_charts_training_years <- function(input_format, training_years, project_from, la, la_label) {
  simulated_episodes_file <- sprintf(input_format, training_years)
  simulated_episodes <- read.csv(simulated_episodes_file, stringsAsFactors = FALSE, na.strings = "")
  simulated_episodes$Birthday <- ymd(simulated_episodes$Birthday)
  simulated_episodes$Start <- ymd(simulated_episodes$Start)
  simulated_episodes$End <- ymd(simulated_episodes$End)
  simulated_episodes$Admission.Age <- as.factor(simulated_episodes$Admission.Age)
  simulated_periods <- simulated_episodes %>%
    mutate(Simulation, ID = paste0(ID, "-")) %>%
    filter(Start >= as.Date("2009-01-01") & Start < as.Date("2028-01-01")) %>%
    group_by(ID, Simulation, Admission.Age, Birthday) %>%
    summarise(Start = min(Start), End = max(End)) %>%
    mutate(month = floor_date(Start, "month"),
           quarter = floor_date(Start, "quarter"),
           year = floor_date(Start, "year"))
  for (period in c("month", "quarter", "year")) {
    simulated_periods %>%
      group_by_at(c("Admission.Age", period, "Simulation")) %>%
      summarise(joiners = n()) %>%
      summarise(average_joiners = median(joiners)) %>%
      ggplot(aes_string(period, "average_joiners", color = "Admission.Age")) +
      geom_rect(xmin = project_from - years(training_years), xmax = project_from, ymin = 0, ymax = 500, color = NA, fill = "#FFFF99", alpha = 0.01) + 
      geom_line(alpha = 0.8) +
      scale_color_manual(values = colours) +
      theme_mastodon +
      labs(title = paste(la_label, "joiners per age, reported & projected"),
           x = period,
           y = paste0("Joiners per ", period),
           color = "Age")
    ggsave(sprintf("%s-out-%s-%s.png", la, period, training_years))
  }
}

project_from <- as.Date("2019-03-31")
simulated_episodes_format <- "/Users/henry/Mastodon C/witan.cic/data/ccc-%styr-10yr.csv"
save_charts_training_years(simulated_episodes_format, 3, project_from, "ccc", "Cambridge")
save_charts_training_years(simulated_episodes_format, 4, project_from, "ccc", "Cambridge")
save_charts_training_years(simulated_episodes_format, 5, project_from, "ccc", "Cambridge")
save_charts_training_years(simulated_episodes_format, 6, project_from, "ccc", "Cambridge")

simulated_episodes_format <- "/Users/henry/Mastodon C/witan.cic/data/suffolk-%styr-10yr.csv"
save_charts_training_years(simulated_episodes_format, 3, project_from, "suffolk", "Suffolk")
save_charts_training_years(simulated_episodes_format, 4, project_from, "suffolk", "Suffolk")
save_charts_training_years(simulated_episodes_format, 5, project_from, "suffolk", "Suffolk")
save_charts_training_years(simulated_episodes_format, 6, project_from, "suffolk", "Suffolk")

simulated_episodes_format <- "/Users/henry/Mastodon C/witan.cic/data/ncc-%styr-10yr.csv"
save_charts_training_years(simulated_episodes_format, 3, project_from, "ncc", "Norfolk")
save_charts_training_years(simulated_episodes_format, 4, project_from, "ncc", "Norfolk")
save_charts_training_years(simulated_episodes_format, 5, project_from, "ncc", "Norfolk")
save_charts_training_years(simulated_episodes_format, 6, project_from, "ncc", "Norfolk")

simulated_episodes %>% group_by_at(c("Admission.Age", "Simulation"))

## Turn episodes into periods...

simulated_episodes %>%
  filter(Simulation == 1) %>%
  group_by(ID) %>%
  summarise(join_date = min(Start, na.rm = TRUE)) %>%
  mutate(month = floor_date(join_date, "month")) %>%
  group_by(month) %>%
  summarise(joiners = n()) %>%
  ggplot(aes(month, joiners)) +
  geom_line()
ggsave("TEST.png")
project_from <- as.Date("2019-03-31")

joiners_per_month <- simulated_periods %>%
  mutate(week = following_monday(Start),
         month = floor_date(Start, "month"),
         year = year(Start)) %>%
  group_by(Admission.Age, month, Simulation) %>%
  summarise(joiners = n()) %>%
  summarise(average_joiners = median(joiners))

joiners_per_year <- simulated_periods %>%
  mutate(week = following_monday(Start),
         month = floor_date(Start, "month"),
         year = year(Start)) %>%
  group_by(Admission.Age, year, Simulation) %>%
  summarise(joiners = n()) %>%
  summarise(average_joiners = median(joiners))

joiners_per_quarter <- simulated_periods %>%
  mutate(quarter = floor_date(Start, "3 months")) %>%
  group_by(Admission.Age, quarter, Simulation) %>%
  summarise(joiners = n()) %>%
  summarise(average_joiners = median(joiners))



train_years <- 7



ggplot(joiners_per_year %>% filter(year > 2009 & year < 2028 & Admission.Age != 0),
       aes(year, average_joiners, color = Admission.Age)) +
  geom_rect(xmin = 2019- train_years, xmax = 2019, ymin = 0, ymax = 100, color = NA, fill = "#FFFF99", alpha = 0.01) + 
  geom_line(alpha = 0.8) +
  scale_color_manual(values = colours) +
  theme_mastodon +
  labs(title = paste(la_label, "joiners per age, reported & projected (excl. age 0)"),
       x = "Year",
       y = "Joiners per year",
       color = "Age")

ggplot(joiners_per_year %>% filter(as.integer(year) > 2009 & as.integer(year) < 2028 & Admission.Age == 0),
       aes(year, average_joiners, color = Admission.Age)) +
  geom_rect(xmin = year(project_from) - train_years, xmax = year(project_from), ymin = 0, ymax = 100, color = NA, fill = "#FFFF99", alpha = 0.01) + 
  geom_line(alpha = 0.8) +
  scale_color_manual(values = colours) +
  theme_mastodon +
  labs(title = paste(la_label, "joiners per age, reported & projected"),
       x = "Month",
       y = "Joiners per month",
       color = "Age")

ggplot(joiners_per_quarter %>% filter(year(quarter) > 2009 & year(quarter) < 2028 & Admission.Age == 0),
       aes(quarter, average_joiners, color = Admission.Age)) +
  geom_rect(xmin = project_from - years(train_years), xmax = project_from, ymin = 0, ymax = 100, color = NA, fill = "#FFFF99", alpha = 0.01) + 
  geom_line(alpha = 0.8) +
  scale_color_manual(values = colours) +
  theme_mastodon +
  labs(title = paste(la_label, "joiners per age, quarterly reported & projected"),
       x = "Quarter",
       y = "Joiners per month",
       color = "Age")


simulated_episodes %>%
  filter(Simulation == 1 & Admission.Age == 0) %>%
  group_by(ID) %>%
  summarise(join_date = min(Start, na.rm = TRUE)) %>%
  arrange(join_date) %>%
  mutate(interval = as.integer(difftime(join_date, lag(join_date), units = "days"))) %>%
  filter(join_date > project_from - years(train_years) & join_date < project_from) %>%
  ggplot(aes(join_date, interval)) +
  geom_point() +
  geom_smooth() +
  theme_mastodon

# calculate total in care per age per day

dates <- data.frame(date = seq(as.Date("2016-01-01"), as.Date("2021-01-01"), by = "day"), join = TRUE)

daily_episodes <- csv %>%
  mutate(join = TRUE) %>%
  inner_join(dates, by = "join") %>%
  filter(date >= Start & (date <= End | is.na(End)))

joined_by_date_2 <- joined_by_date %>%
  mutate(age = factor(year_diff(Birthday, date)),
         Simulation = factor(Simulation))

base_by_age <- joined_by_date_2 %>%
  mutate(Simulation = factor(Simulation)) %>%
  group_by(date, Simulation, age) %>%
  summarise(n = n())

ggplot(base_by_age %>% filter(age == 0 & Simulation == 1), aes(date, n, color = Simulation)) + geom_line()

simulated_episodes <- "/Users/henry/Mastodon C/witan.cic/data/linear-episodes.csv"
data <- read.csv(simulated_episodes) %>%
  mutate(Birthday = ymd(Birthday), Start = ymd(Birthday), End = ymd(Birthday), Simulation = factor(as.character(Simulation)))

data <- data %>% mutate(Start.Quarter = floor_date(Start, "quarter")) %>% filter(Episode == 1) %>%
  filter(Start.Quarter >= as.Date("2015-01-01")) %>%
  group_by(Start.Quarter, Admission.Age, Simulation) %>%
  summarise(n = n())

ggplot(data, aes(Start.Quarter, n, color = Simulation)) +
  geom_line() +
  facet_wrap(vars(Admission.Age))
