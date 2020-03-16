## To make this work you need to define raw_episodes as the input file
## and scrubbed_episodes as the output file.

library(dplyr)
library(lubridate)

## Update with episodes data from SSDA903
## raw_episodes <- "data/SSDA903.csv"

parse.dates <- function(episodes) {
  episodes$report_date <- ymd(episodes$report_date)
  episodes$ceased <- ymd(episodes$ceased)
  episodes
}

remove.stale.episodes <- function(episodes) {
  max_report_year <- max(episodes$report_year)
  episodes %>% filter(report_year == max_report_year | (report_year < max_report_year & !is.na(ceased)))
}

remap.placements <- function(episodes) {
  episodes %>% mutate(placement = ifelse(placement %in% c("U1", "U2", "U3"), "Q1", ifelse(placement %in% c("U4", "U5", "U6"), "Q2", placement)))
}

remove.respite.episodes <- function(episodes) {
  episodes %>% filter(!legal_status %in% c("V3", "V4"))
}

# We can break each child's episodes into periods of continuous care
assoc.period.id <- function(episodes) {
  episodes <- episodes %>% arrange(ID, report_date)
  new_periods <- coalesce(episodes$ID == lag(episodes$ID) & episodes$report_date > lag(episodes$ceased), FALSE)
  episodes$period_id <- paste0(episodes$ID, "-", ave(ifelse(new_periods, 1.0, 0.0), episodes$ID, FUN = cumsum) + 1)
  episodes
}

# After assigning period ID, we can assign contiguous episode numbers within each period
assoc.episode.number <- function(episodes) {
  episodes <- episodes %>% arrange(period_id, report_date)
  episodes$episode_number <- ave(rep(1.0, nrow(episodes)), episodes$period_id, FUN = cumsum)
  episodes
}

# After assigning period ID, we can assign phases of same-placement care within each period
assoc.phase.id <- function(episodes) {
  episodes <- episodes %>% arrange(period_id, report_date)
  new_phases <- coalesce(episodes$period_id == lag(episodes$period_id) & episodes$placement != lag(episodes$placement), FALSE)
  episodes$phase_number <- ave(ifelse(new_phases, 1.0, 0.0), episodes$period_id, FUN = cumsum) + 1
  episodes$phase_id <- paste0(episodes$period_id, "-", episodes$phase_number)
  episodes
}

episodes <- read.csv(raw_episodes, header = TRUE, stringsAsFactors = FALSE, na.strings ="") %>%
  parse.dates %>%
  remove.stale.episodes %>%
  remap.placements %>%
  assoc.period.id %>%
  assoc.episode.number %>%
  assoc.phase.id

respite.children <- episodes %>% group_by(period_id) %>% filter(length(unique(legal_status %in% c("V3", "V4"))) > 1) %>% ungroup %>% dplyr::select(ID) %>% unique %>% as.data.frame
respite.children$ID
episodes <- episodes %>% filter(!ID %in% respite.children$ID)

write.csv(episodes, scrubbed_episodes, na = "")
