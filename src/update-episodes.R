library(dplyr)
library(lubridate)

data <- read.csv("~/Downloads/episodes-rewind-1yr-train-5yr-project-2yr-runs-100-seed-42.csv", 
                  header = TRUE)

data %>%
  mutate(Start = ymd(Start), End = ymd(End)) %>%
  group_by(ID) %>%
  mutate(Period.Start = min(Start), Period.End = max(End)) %>%
  mutate(Period.Duration = Period.End - Period.Start) %>%
  ungroup()
