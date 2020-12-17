library(dplyr)
library(lubridate)
library(tidyquant)
library(extrafont)
library(ggplot2)
library(ggthemes)
source('src/helpers.R')

actual_episodes_file <- ''
projected_episodes_file <- ''
output_file <- ''
output_file_joiners <- ''
project_from <- as.Date("2019-08-13")
output_file_layercake <- ''
project_yrs <- 5
train_from <- project_from - years(3)

font_import()
loadfonts()

output_all_charts <- function() {
  set.seed(5)
  episodes <- read.csv(actual_episodes_file, header = TRUE, stringsAsFactors = FALSE, na.strings ="NA")
  episodes$report_date <- ymd(episodes$report_date)
  episodes$ceased <- ymd(episodes$ceased)
  end_date <- max(c(episodes$report_date), na.rm = TRUE)
  birthdays <- episodes %>% group_by(ID) %>% summarise(birthday = imputed_birthday(DOB[1], min(report_date), coalesce(max(ceased), end_date)))
  episodes <- episodes %>% inner_join(birthdays)
  episodes <- episodes %>% group_by(phase_id) %>% mutate(admission_age = year_diff(min(birthday), min(report_date))) %>% ungroup
  episodes$placement_category <- substr(episodes$placement, 1, 1)
  
  projected_episodes <- read.csv(projected_episodes_file, header = TRUE, stringsAsFactors = FALSE, na.strings ="")
  projected_episodes$Start <- ymd(projected_episodes$Start)
  projected_episodes$End <- ymd(projected_episodes$End)
  projected_episodes$Birthday <- ymd(projected_episodes$Birthday)
  projected_episodes$Placement.Category <- substr(projected_episodes$Placement, 1, 1)
  
  dates <- seq(as.Date("2015-01-01"), as.Date("2022-02-01"), by = "week")
  placements <- (episodes %>% group_by(placement) %>% summarise(n = n()) %>% arrange(desc(n)))$placement
  placement_categories <- (episodes %>% group_by(placement_category) %>% summarise(n = n()) %>% arrange(desc(n)))$placement_category
  
  colours <- c("#4E79A7", "#F28E2B", "grey", "#F28E2B", "#4E79A7", "black")
  names(colours) <- c("lower.ci", "q1", "median", "q3", "upper.ci", "actual")
  
  projected <- data.frame(date = c(), lower.ci = c(), q1 = c(), median = c(), q3 = c(), upper.ci = c())
  for (date in dates) {
    counts_by_simulation <- projected_episodes %>%
      filter(Start <= date & (is.na(End) | End >= date)) %>%
      group_by(Simulation) %>%
      summarise(n = n())
    quants <- quantile(counts_by_simulation$n, probs = c(0.05, 0.25, 0.5, 0.75, 0.975))
    projected <- rbind(projected, data.frame(date = c(date), lower.ci = c(quants[1]), q1 = c(quants[2]), median = c(quants[3]), q3 = c(quants[4]), upper.ci = c(quants[5])))
  }
  projected$date <- as.Date(projected$date)
  projected <- projected %>% filter(lower.ci != upper.ci)
  actuals <- data.frame(date = c(), variable = c(), value = c())
  for (date in dates[dates < end_date]) {
    counts <- episodes %>%
      filter(report_date <= date & (is.na(ceased) | ceased > date)) %>%
      summarise(n = n())
    actuals <- rbind(actuals, data.frame(date = c(date), variable = c("actual"), value = c(counts[[1]])))
  }
  actuals$date <- as.Date(actuals$date)
  print(ggplot() +
          geom_line(data = actuals, aes(x = date, y = value)) +
          geom_line(data = projected, aes(x = date, y = median), linetype = 2) +
          geom_ribbon(data = projected, aes(x = date, ymin = lower.ci, ymax = upper.ci), fill = "gray", alpha = 0.3) +
          geom_ribbon(data = projected, aes(x = date, ymin = q1, ymax = q3), fill = "gray", alpha = 0.3) +
          geom_vline(xintercept = train_from, color = "red", linetype = 2) +
          theme_mastodon +
          scale_color_manual(values = colours) +
          labs(title = "CiC", x = "Date", y = "CiC"))
  
  for (test.placement in placement_categories) {
    projected <- data.frame(date = c(), lower.ci = c(), q1 = c(), median = c(), q3 = c(), upper.ci = c())
    for (date in dates) {
      counts_by_simulation <- projected_episodes %>%
        filter(Start <= date & (is.na(End) | End >= date)) %>%
        filter(Placement.Category == test.placement) %>%
        group_by(Simulation) %>%
        summarise(n = n())
      quants <- quantile(counts_by_simulation$n, probs = c(0.05, 0.25, 0.5, 0.75, 0.975))
      projected <- rbind(projected, data.frame(date = c(date), lower.ci = c(quants[1]), q1 = c(quants[2]), median = c(quants[3]), q3 = c(quants[4]), upper.ci = c(quants[5])))
    }
    projected$date <- as.Date(projected$date)
    projected <- projected %>% filter(lower.ci != upper.ci)
    actuals <- data.frame(date = c(), variable = c(), value = c())
    for (date in dates[dates < end_date]) {
      counts <- episodes %>%
        filter(report_date <= date & (is.na(ceased) | ceased > date)) %>%
        filter(placement_category == test.placement) %>%
        summarise(n = n())
      actuals <- rbind(actuals, data.frame(date = c(date), variable = c("actual"), value = c(counts[[1]])))
    }
    actuals$date <- as.Date(actuals$date)
    print(ggplot() +
            geom_line(data = actuals, aes(x = date, y = value)) +
            geom_line(data = projected, aes(x = date, y = median), linetype = 2) +
            geom_ribbon(data = projected, aes(x = date, ymin = lower.ci, ymax = upper.ci), fill = "gray", alpha = 0.3) +
            geom_ribbon(data = projected, aes(x = date, ymin = q1, ymax = q3), fill = "gray", alpha = 0.3) +
            geom_vline(xintercept = train_from, color = "red", linetype = 2) +
            theme_mastodon +
            scale_color_manual(values = colours) +
            labs(title = paste0(test.placement), x = "Date", y = "CiC"))
  }
  
  for (test.placement in placements) {
    projected <- data.frame(date = c(), lower.ci = c(), q1 = c(), median = c(), q3 = c(), upper.ci = c())
    for (date in dates) {
      counts_by_simulation <- projected_episodes %>%
        filter(Start <= date & (is.na(End) | End >= date)) %>%
        filter(Placement == test.placement) %>%
        group_by(Simulation) %>%
        summarise(n = n())
      quants <- quantile(counts_by_simulation$n, probs = c(0.05, 0.25, 0.5, 0.75, 0.975))
      projected <- rbind(projected, data.frame(date = c(date), lower.ci = c(quants[1]), q1 = c(quants[2]), median = c(quants[3]), q3 = c(quants[4]), upper.ci = c(quants[5])))
    }
    projected$date <- as.Date(projected$date)
    projected <- projected %>% filter(lower.ci != upper.ci)
    actuals <- data.frame(date = c(), variable = c(), value = c())
    for (date in dates[dates < end_date]) {
      counts <- episodes %>%
        filter(report_date <= date & (is.na(ceased) | ceased > date)) %>%
        filter(placement == test.placement) %>%
        summarise(n = n())
      actuals <- rbind(actuals, data.frame(date = c(date), variable = c("actual"), value = c(counts[[1]])))
    }
    actuals$date <- as.Date(actuals$date)
    print(ggplot() +
            geom_line(data = actuals, aes(x = date, y = value)) +
            geom_line(data = projected, aes(x = date, y = median), linetype = 2) +
            geom_ribbon(data = projected, aes(x = date, ymin = lower.ci, ymax = upper.ci), fill = "gray", alpha = 0.3) +
            geom_ribbon(data = projected, aes(x = date, ymin = q1, ymax = q3), fill = "gray", alpha = 0.3) +
            geom_vline(xintercept = train_from, color = "red", linetype = 2) +
            theme_mastodon +
            scale_color_manual(values = colours) +
            labs(title = paste0(test.placement), x = "Date", y = "CiC"))
  }
  
  counts_by_age_simulation <- data.frame(Age = c(), Simulation = c(), n = c(), Date = c())
  for (date in dates) {
    date <- as.Date(date)
  counts_by_age_simulation <- rbind(counts_by_age_simulation,
                                    projected_episodes %>%
    filter(Start <= date & (is.na(End) | End >= date)) %>%
    mutate(Age = year_diff(Birthday, date)) %>%
    group_by(Age, Simulation) %>%
    summarise(n = n()) %>%
      mutate(Date = date)
    )
  }

  print(counts_by_age_simulation %>%
    mutate(Age.Group = case_when(Age < 1 ~ "< 1",
                                 Age <= 3 ~ "1-3",
                                 Age <= 6 ~ "4-6",
                                 Age <= 9 ~ "7-9",
                                 Age <= 12 ~ "10-12",
                                 Age <= 15 ~ "13-15",
                                 Age <= 17 ~ "16-17",
                                 TRUE ~ "Other")) %>%
    group_by(Age.Group, Date, Simulation) %>%
    summarise(n = sum(n)) %>%
    summarise(n = median(n)) %>%
    ungroup %>%
    mutate(Age.Group = factor(Age.Group, levels = c("< 1", "1-3", "4-6", "7-9", "10-12", "13-15", "16-17"))) %>%
    ggplot(aes(Date, n, fill = Age.Group)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
    theme_mastodon +
    labs(x = "Date", y = "Count", title = "% of children in care by age group"))

  for (test.age in 0:17) {
    projected <- data.frame(date = c(), lower.ci = c(), q1 = c(), median = c(), q3 = c(), upper.ci = c())
    for (date in dates) {
      date <- as.Date(date)
      counts_by_simulation <- counts_by_age_simulation %>%
        filter(Date == date & Age == test.age)
      quants <- quantile(counts_by_simulation$n, probs = c(0.05, 0.25, 0.5, 0.75, 0.975))
      projected <- rbind(projected, data.frame(date = c(date), lower.ci = c(quants[1]), q1 = c(quants[2]), median = c(quants[3]), q3 = c(quants[4]), upper.ci = c(quants[5])))
    }
    projected$date <- as.Date(projected$date)
    projected <- projected %>% filter(lower.ci != upper.ci)
    actuals <- data.frame(date = c(), variable = c(), value = c())
    for (date in dates[dates < end_date]) {
      date <- as.Date(date)
      counts <- episodes %>%
        filter(report_date <= date & (is.na(ceased) | ceased > date)) %>%
        filter(year_diff(birthday, date) == test.age) %>%
        summarise(n = n())
      actuals <- rbind(actuals, data.frame(date = c(date), variable = c("actual"), value = c(counts[[1]])))
    }
    actuals$date <- as.Date(actuals$date)
    print(ggplot() +
            geom_line(data = actuals, aes(x = date, y = value)) +
            geom_line(data = projected, aes(x = date, y = median), linetype = 2) +
            geom_ribbon(data = projected, aes(x = date, ymin = lower.ci, ymax = upper.ci), fill = "gray", alpha = 0.3) +
            geom_ribbon(data = projected, aes(x = date, ymin = q1, ymax = q3), fill = "gray", alpha = 0.3) +
            geom_vline(xintercept = train_from, color = "red", linetype = 2) +
            theme_mastodon +
            scale_color_manual(values = colours) +
            labs(title = paste0("Age ", test.age), x = "Date", y = "CiC"))
  }
  
  # Joiners
  
  join_leave_projected <- projected_episodes %>%
    # mutate(Birthday = ceiling_date(Birthday, unit = "month")) %>%
    group_by(Simulation, ID) %>%
    summarise(Join = min(Start),
              Leave = max(End),
              Birthday = Birthday[1]) %>%
    mutate(Join.Age = year_diff(Birthday, Join),
           Leave.Age = year_diff(Birthday, Leave - days(1)))
  
  join_leave_actual_summary <- episodes %>%
    # mutate(birthday = ceiling_date(birthday, unit = "month")) %>%
    group_by(period_id) %>%
    summarise(Join = min(report_date),
              Leave = max(ceased),
              Birthday = birthday[1]) %>%
    mutate(Join.Age = year_diff(Birthday, Join),
           Leave.Age = year_diff(Birthday, Leave - days(1)))
  
  join_projected_ci <- join_leave_projected %>%
    mutate(Join = floor_date(Join, unit = "month")) %>%
    rename(date = Join) %>%
    group_by(date, Simulation) %>%
    summarise(n = n()) %>%
    summarise(lower.ci = quantile(n, probs = 0.05),
              q1 = quantile(n, probs = 0.25),
              median = quantile(n, probs = 0.5),
              q3 = quantile(n, probs = 0.75),
              upper.ci = quantile(n, probs = 0.975)) %>%
    filter(date > as.Date("2016-12-01"))
  
  leave_projected_ci <- join_leave_projected %>%
    mutate(Leave = floor_date(Leave, unit = "month")) %>%
    rename(date = Leave) %>%
    group_by(date, Simulation) %>%
    summarise(n = n()) %>%
    summarise(lower.ci = quantile(n, probs = 0.05),
              q1 = quantile(n, probs = 0.25),
              median = quantile(n, probs = 0.5),
              q3 = quantile(n, probs = 0.75),
              upper.ci = quantile(n, probs = 0.975)) %>%
    filter(date > as.Date("2016-12-01"))
  
  join_actuals <- join_leave_actual_summary %>%
    mutate(Join = floor_date(Join, unit = "month")) %>%
    group_by(Join) %>%
    summarise(variable = "actual", value = n()) %>%
    rename(date = Join) %>%
    filter(date > as.Date("2016-01-01"))
  
  leave_actuals <- join_leave_actual_summary %>%
    mutate(Leave = floor_date(Leave, unit = "month")) %>%
    group_by(Leave) %>%
    summarise(variable = "actual", value = n()) %>%
    rename(date = Leave) %>%
    filter(date > as.Date("2016-01-01"))
  
  net_actuals <- join_actuals %>%
    inner_join(leave_actuals, by = "date") %>%
    mutate(value = value.x - value.y,
           variable = variable.x) %>%
    dplyr::select(date, variable, value)
  
  print(ggplot(rbind(join_actuals %>% mutate(variable = "Join"),
               leave_actuals %>% mutate(variable = "Leave"),
               net_actuals %>% mutate(variable = "Net")) %>%
           mutate(variable = factor(variable, levels = c("Join", "Net", "Leave"))),
         aes(date, value, colour = variable)) +
    facet_grid(rows = vars(variable), scales = "free_y") +
    geom_line() +
    geom_ma(n = 12) +
    scale_colour_manual(values = tableau_color_pal("Tableau 20")(20)) +
    theme_mastodon +
    labs(x = "Date", y = "Count", title = "Joiners, leavers & net growth + 12 period moving average") +
      coord_cartesian(xlim = c(as.Date("2016-01-01"), as.Date("2020-01-01"))))
  
  print(ggplot() +
          geom_line(data = join_actuals, aes(x = date, y = value)) +
          geom_line(data = join_projected_ci, aes(x = date, y = median), linetype = 2) +
          geom_ribbon(data = join_projected_ci, aes(x = date, ymin = lower.ci, ymax = upper.ci), fill = "gray", alpha = 0.3) +
          geom_ribbon(data = join_projected_ci, aes(x = date, ymin = q1, ymax = q3), fill = "gray", alpha = 0.3) +
          geom_vline(xintercept = train_from, color = "red", linetype = 2) +
          theme_mastodon +
          scale_color_manual(values = colours) +
          labs(title = "Joiners per month", x = "Date", y = "CiC") +
          coord_cartesian(xlim = c(min(dates), max(dates))))
  
  print(ggplot() +
          geom_line(data =  leave_actuals, aes(x = date, y = value)) +
          geom_line(data = leave_projected_ci, aes(x = date, y = median), linetype = 2) +
          geom_ribbon(data = leave_projected_ci, aes(x = date, ymin = lower.ci, ymax = upper.ci), fill = "gray", alpha = 0.3) +
          geom_ribbon(data = leave_projected_ci, aes(x = date, ymin = q1, ymax = q3), fill = "gray", alpha = 0.3) +
          geom_vline(xintercept = train_from, color = "red", linetype = 2) +
          theme_mastodon +
          scale_color_manual(values = colours) +
          labs(title = "Leavers per month", x = "Date", y = "CiC") +
          coord_cartesian(xlim = c(min(dates), max(dates))))
  
  for (test.age in 0:17){
    
    test.age <- 9
    join_projected_ci <- join_leave_projected %>%
      filter(Join.Age == test.age) %>%
      mutate(Join = floor_date(Join, unit = "month")) %>%
      rename(date = Join) %>%
      group_by(date, Simulation) %>%
      summarise(n = n()) %>%
      summarise(lower.ci = quantile(n, probs = 0.05),
                q1 = quantile(n, probs = 0.25),
                median = quantile(n, probs = 0.5),
                q3 = quantile(n, probs = 0.75),
                upper.ci = quantile(n, probs = 0.975))
    
    leave_projected_ci <- join_leave_projected %>%
      filter(Leave.Age == test.age) %>%
      mutate(Leave = floor_date(Leave, unit = "month")) %>%
      rename(date = Leave) %>%
      group_by(date, Simulation) %>%
      summarise(n = n()) %>%
      summarise(lower.ci = quantile(n, probs = 0.05),
                q1 = quantile(n, probs = 0.25),
                median = quantile(n, probs = 0.5),
                q3 = quantile(n, probs = 0.75),
                upper.ci = quantile(n, probs = 0.975))
    
    join_actuals <- join_leave_actual_summary %>%
      filter(Join.Age == test.age) %>%
      mutate(Join = floor_date(Join, unit = "month")) %>%
      group_by(Join) %>%
      summarise(variable = "actual", value = n()) %>%
      rename(date = Join)
    
    leave_actuals <- join_leave_actual_summary %>%
      filter(Leave.Age == test.age) %>%
      mutate(Leave = floor_date(Leave, unit = "month")) %>%
      group_by(Leave) %>%
      summarise(variable = "actual", value = n()) %>%
      rename(date = Leave)
    
    print(ggplot(rbind(join_actuals %>% mutate(variable = "Join"),
                 leave_actuals %>% mutate(variable = "Leave"),
                 net_actuals %>% mutate(variable = "Net")) %>%
             mutate(variable = factor(variable, levels = c("Join", "Net", "Leave"))),
           aes(date, value, colour = variable)) +
      facet_grid(rows = vars(variable), scales = "free_y") +
      geom_line() +
      geom_ma(n = 12) +
      scale_colour_manual(values = tableau_color_pal("Tableau 20")(20)) +
      theme_mastodon +
      labs(x = "Date", y = "Count", title = "Joiners, leavers & net growth + 12 period moving average") +
      coord_cartesian(xlim = c(min(dates), max(dates))))
    
    print(ggplot() +
            geom_line(data = join_actuals, aes(x = date, y = value)) +
            geom_line(data = join_projected_ci, aes(x = date, y = median), linetype = 2) +
            geom_ribbon(data = join_projected_ci, aes(x = date, ymin = lower.ci, ymax = upper.ci), fill = "gray", alpha = 0.3) +
            geom_ribbon(data = join_projected_ci, aes(x = date, ymin = q1, ymax = q3), fill = "gray", alpha = 0.3) +
            geom_vline(xintercept = train_from, color = "red", linetype = 2) +
            theme_mastodon +
            scale_color_manual(values = colours) +
            labs(title = paste0("Age ", test.age, " joiners per month"), x = "Date", y = "CiC") +
            coord_cartesian(xlim = c(min(dates), max(dates))))
    
    print(ggplot() +
            geom_line(data = leave_actuals, aes(x = date, y = value)) +
            geom_line(data = leave_projected_ci, aes(x = date, y = median), linetype = 2) +
            geom_ribbon(data = leave_projected_ci, aes(x = date, ymin = lower.ci, ymax = upper.ci), fill = "gray", alpha = 0.3) +
            geom_ribbon(data = leave_projected_ci, aes(x = date, ymin = q1, ymax = q3), fill = "gray", alpha = 0.3) +
            geom_vline(xintercept = train_from, color = "red", linetype = 2) +
            theme_mastodon +
            scale_color_manual(values = colours) +
            labs(title = paste0("Age ", test.age, " leavers per month"), x = "Date", y = "CiC") +
            coord_cartesian(xlim = c(min(dates), max(dates))))
  }
}

pdf(output_file, fonts = c("Open Sans", "Open Sans SemiBold"), paper = "a4r")
output_all_charts()
dev.off()
embed_fonts(file = output_file, outfile = output_file)

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon) - as.integer(ed$mday < sd$mday)
}

elapsed_months(as.Date("2021-12-01"), as.Date("2020-01-01")) %% 12


plot_summary <- function(project_from, project_yrs) {
  projected_episodes <- read.csv(projected_episodes_file, header = TRUE, stringsAsFactors = FALSE, na.strings ="")
  projected_episodes$Start <- ymd(projected_episodes$Start)
  projected_episodes$End <- ymd(projected_episodes$End)
  projected_episodes$Birthday <- ymd(projected_episodes$Birthday)
  projected_episodes <- projected_episodes %>% group_by(Simulation, ID) %>% mutate(Min.Start = min(Start), Max.End = max(End)) %>% ungroup
  projected_periods <- projected_episodes %>% group_by(Simulation, ID) %>% summarise(Min.Start = min(Start), Max.End = max(End)) %>% ungroup
  dates <- seq(as.Date("2011-01-01"), project_from + years(project_yrs), by = "week")
  colours = tableau_color_pal("Tableau 20")(20)
  projected <- data.frame(date = c(), joiners.count = c(), cic.count = c(), median.open.duration = c(), median.closed.duration = c())
  for (date in dates) {
    date <- as.Date(date)
    in.cic <- projected_episodes %>%
      filter(Start <= date & (is.na(End) | End >= date)) %>%
      group_by(Simulation) %>%
      summarise(n = n(), median.open.duration = as.integer(median(date - Min.Start)))
    joiners <- projected_periods %>%
      filter(Min.Start <= date) %>%
      group_by(Simulation) %>%
      summarise(n = n())
    leavers <- projected_periods %>%
      filter(Max.End < date & Max.End >= date - months(1)) %>%
      group_by(Simulation) %>%
      summarise(median.closed.duration = as.integer(median(date - Min.Start)))
    projected <- rbind(projected, data.frame(date = c(date), joiners.count = c(median(joiners$n)), cic.count = c(median(in.cic$n)), median.open.duration = c(median(in.cic$median.open.duration)),
                                             median.closed.duration = median(leavers$median.closed.duration)))
  }
  projected$joiners.count <- projected$joiners.count - min(projected$joiners.count)
  print(ggplot(projected, aes(x = date)) +
          geom_vline(aes(xintercept = project_from, colour = "#444444"), linetype = 2) +
          geom_ribbon(alpha = 0.15, aes(fill = colours[1], ymin = joiners.count / 2.0, ymax = (joiners.count + cic.count) / 2.0, x = date)) +
          geom_line(aes(x = date, y = cic.count, colour = colours[1])) +
          geom_line(aes(x = date, y = median.open.duration,  colour = colours[2])) +
          geom_line(aes(x = date, y = median.closed.duration, colour = colours[3])) +
          geom_ma(aes(x = date, y = median.closed.duration, colour = colours[4]), n = 12) +
          labs(title = "Total CiC", color = "Counted", fill = "Shaded", x = "Date") +
          scale_color_manual(values = c("#444444","#4E79A7", "#A0CBE8", "#FFBE7D", "#F28E2B"),
                             labels = c("Projection start", "CiC Count", "Median Open Duration", "Closed Duration (Prior Month Average)", "Closed Duration Moving Average")) +
          scale_fill_manual(values = c(colours[1], "#CCCCCC"),
                            labels = c("Total CiC", "Projected Period")) +
          scale_y_continuous("Counted", sec.axis = sec_axis(~ . * 2, name = "Shaded")) +
          theme_mastodon)

  all_placements <- (projected_episodes %>% group_by(Placement) %>% summarise(n = n()) %>% arrange(desc(n)))$Placement
  for (test.placement in all_placements) {
    by_age <- data.frame(date = c(), variable = c(), value = c())
    for (date in dates) {
      date <- as.Date(date)
      in.cic <- projected_episodes %>%
        filter(Start <= date & (is.na(End) | End >= date) & Placement == test.placement) %>%
        mutate(Age = year_diff(Birthday, date)) %>%
        group_by(Age, Simulation) %>%
        summarise(n = n()) %>%
        summarise(count = median(n))
      
      joiners <- projected_episodes %>%
        filter(Start <= date & Placement == test.placement) %>%
        group_by(Simulation) %>%
        summarise(n = n()) %>%
        summarise(count = median(n))
      
      res <- data.frame(date = c(date), variable = c(in.cic$Age, "Joined"), value = c(in.cic$count, joiners$count))
      
      by_age <- rbind(by_age, res)
    }
    by_age[by_age$variable == "Joined",]$value <- by_age[by_age$variable == "Joined",]$value - min(by_age[by_age$variable == "Joined",]$value)
    n_factors <- length(unique(by_age$variable))
    by_age$variable <- factor(by_age$variable, levels = rev(c("Joined", 0:18)))
    cols <- colours
    cols[n_factors] <- NA
    print(ggplot(by_age, aes(x = date, y = value, fill = variable)) +
      geom_vline(aes(xintercept = project_from), colour = "black", linetype = 2) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = cols) +
      theme_mastodon +
      labs(fill = "Age", title = test.placement, x = "Date", y = "Cumulative Count"))
  }
  
  for (test.age in c(17)) {
    by_age <- data.frame(date = c(), variable = c(), value = c())
    for (date in dates) {
      date <- as.Date(date)
      in.cic <- projected_episodes %>%
        filter(Start <= date & (is.na(End) | End >= date)) %>%
        mutate(Age = year_diff(Birthday, date)) %>%
        filter(Age == test.age) %>%
        group_by(Placement, Simulation) %>%
        summarise(n = n()) %>%
        summarise(count = mean(n))
      
      joiners <- projected_episodes %>%
        filter(Start <= date & Admission.Age == test.age) %>%
        group_by(Simulation) %>%
        summarise(n = n()) %>%
        summarise(count = mean(n))
      
      res <- data.frame(date = c(date), variable = c(in.cic$Placement, "Joined"), value = c(in.cic$count, joiners$count))
      
      by_age <- rbind(by_age, res)
    }
    by_age[by_age$variable == "Joined",]$value <- by_age[by_age$variable == "Joined",]$value - min(by_age[by_age$variable == "Joined",]$value)
    n_factors <- length(unique(by_age$variable))
    by_age$variable <- factor(by_age$variable, levels = rev(c("Joined", all_placements)))
    cols <- colours
    cols[n_factors] <- NA
    print(ggplot(by_age, aes(x = date, y = value, fill = variable)) +
            geom_vline(aes(xintercept = project_from), colour = "black", linetype = 2) +
            geom_bar(stat = "identity", position = "stack") +
            scale_fill_manual(values = cols) +
            theme_mastodon +
            labs(fill = "Age", title = test.age, x = "Date", y = "Cumulative Count"))
  }
  
  for (test.age in c(17)) {
    by_age <- data.frame(date = c(), variable = c(), value = c())
    for (date in dates) {
      date <- as.Date(date)
      in.cic <- projected_episodes %>%
        mutate(Label = if_else(Admission.Age == 17, "Joined @ 17", "Joined < 17")) %>%
        filter(Start <= date & (is.na(End) | End >= date)) %>%
        mutate(Age = year_diff(Birthday, date)) %>%
        filter(Age == test.age) %>%
        group_by(Label, Simulation) %>%
        summarise(n = n()) %>%
        summarise(count = mean(n))
      
      joiners <- projected_episodes %>%
        filter(Start <= date & Admission.Age == test.age) %>%
        group_by(Simulation) %>%
        summarise(n = n()) %>%
        summarise(count = mean(n))
      
      res <- data.frame(date = c(date), variable = c(in.cic$Label, "Joined"), value = c(in.cic$count, joiners$count))
      
      by_age <- rbind(by_age, res)
    }
    by_age[by_age$variable == "Joined",]$value <- by_age[by_age$variable == "Joined",]$value - min(by_age[by_age$variable == "Joined",]$value)
    n_factors <- length(unique(by_age$variable))
    by_age$variable <- factor(by_age$variable, levels = rev(c("Joined", "Joined @ 17", "Joined < 17")))
    cols <- colours
    cols[n_factors] <- NA
    print(ggplot(by_age, aes(x = date, y = value, fill = variable)) +
            geom_vline(aes(xintercept = project_from), colour = "black", linetype = 2) +
            geom_bar(stat = "identity", position = "stack") +
            scale_fill_manual(values = cols) +
            theme_mastodon +
            labs(fill = "Label", title = test.age, x = "Date", y = "Cumulative Count"))
  }
  
  for (test.age in c(17)) {
    by_age <- data.frame(date = c(), variable = c(), value = c())
    for (date in dates) {
      date <- as.Date(date)
      Labels <- 0:11
      in.cic <- projected_episodes %>%
        mutate(Label = elapsed_months(date, Birthday) %% 12) %>%
        filter(Start <= date & (is.na(End) | End >= date)) %>%
        mutate(Age = year_diff(Birthday, date)) %>%
        filter(Age == test.age) %>%
        group_by(Label, Simulation) %>%
        summarise(n = n()) %>%
        summarise(count = mean(n))
      
      joiners <- projected_episodes %>%
        filter(Start <= date & Admission.Age == test.age) %>%
        group_by(Simulation) %>%
        summarise(n = n()) %>%
        summarise(count = mean(n))
      
      res <- data.frame(date = c(date), variable = c(in.cic$Label, "Joined"), value = c(in.cic$count, joiners$count))
      
      by_age <- rbind(by_age, res)
    }
    by_age[by_age$variable == "Joined",]$value <- by_age[by_age$variable == "Joined",]$value - min(by_age[by_age$variable == "Joined",]$value)
    n_factors <- length(unique(by_age$variable))
    by_age$variable <- factor(by_age$variable, levels = rev(c("Joined", Labels)))
    cols <- colours
    cols[n_factors] <- NA
    print(ggplot(by_age, aes(x = date, y = value, fill = variable)) +
            geom_vline(aes(xintercept = project_from), colour = "black", linetype = 2) +
            geom_bar(stat = "identity", position = "stack") +
            scale_fill_manual(values = cols) +
            theme_mastodon +
            labs(fill = "Age (months after 17th birthday)", title = test.age, x = "Date", y = "Cumulative Count"))
  }
}

elapsed_months(as.Date("2020-11-11"), as.Date("2020-10-03"))

pdf(output_file_joiners, fonts = c("Open Sans", "Open Sans SemiBold"), paper = "a4r")
plot_summary(project_from, project_yrs)
dev.off()
embed_fonts(file = output_file_joiners, outfile = output_file_joiners)

projected_episodes <- read.csv(projected_episodes_file, header = TRUE, stringsAsFactors = FALSE, na.strings ="")

projected_episodes %>%
  # mutate(Birthday = ceiling_date(Birthday, unit = "month")) %>%
  group_by(Simulation, ID) %>%
  summarise(Join = min(Start),
            Leave = max(End),
            Birthday = Birthday[1]) %>%
  mutate(Join.Age = year_diff(Birthday, Join),
         Leave.Age = year_diff(Birthday, Leave - days(1))) %>%

## Plot frequency of segments used in matching to see if there’s bias

matched_segments <- read.csv("/Users/henry/Mastodon C/witan.cic/matched-segments.csv", col.names = c("period_id", "segment_id"))
matched_segments %>%
  group_by(segment_id) %>%
  summarise(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(bins = 250) +
  theme_mastodon +
  labs(x = "Number of times a segment was incorporated during projection", y = "Unique segments", title = "Most segments are used only once, but some are used ~50 times")

## Layer cake plots

projected_episodes <- read.csv(projected_episodes_file, header = TRUE, stringsAsFactors = FALSE, na.strings = "")
projected_episodes$Start <- ymd(projected_episodes$Start)
projected_episodes$End <- ymd(projected_episodes$End)
projected_episodes$Birthday <- ymd(projected_episodes$Birthday)
projected_episodes$Placement.Category <- substr(projected_episodes$Placement, 1, 1)

projected_episodes <- projected_episodes %>%
  filter(Simulation < 10) %>%
  mutate(Offset.End = as.integer(day_diff(Period.Start, End)),
         Provenance = ifelse(is.na(Provenance), "S", Provenance))

top_age_pathways <- projected_episodes %>% filter(Provenance == "H") %>% group_by(ID) %>% slice(1) %>%
  group_by(Admission.Age, Placement.Pathway) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
  as.data.frame
top_age_pathways <- top_age_pathways[1:50,]

offsets <- seq(0, max(projected_episodes$Period.Duration), 7)
episodes.weekly <- data.frame(offset = offsets) %>%
  inner_join(projected_episodes, by = character()) %>%
  filter(offset >= Offset & offset <= Offset.End) %>%
  mutate(Placement.P = ifelse(!is.na(Match.Offset) & offset >= Match.Offset, paste0(Placement, ".P"), Placement))

episodes.weekly <- episodes.weekly %>%
  mutate(Provenance = ifelse(Provenance == "P", "PB", Provenance)) %>%
  rbind(episodes.weekly %>%
          filter(Provenance == "P" & offset < Match.Offset) %>%
          mutate(Provenance = "PA"))

all.placements <- c("A3", "A4", "A5", "A6", "H5", "K1", "K2", "M2", "M3", "P1", "P2",
                    "Q1","Q2", "R1", "R2", "R3", "R5", "S1", "T0", "T4", "Z1",
                    'Join')

my.colours <- tableau_color_pal("Tableau 20")(20)
my.colours <- c(my.colours, "#888888", "#FFFFFF")

all.colours <- c(my.colours, my.colours)
all.placements <- c(all.placements, paste0(all.placements, ".P"))
names(all.colours) <- all.placements


pdf(file = output_file_layercake, paper = "a4r", width=11, height=8.5)
for (i in 1:nrow(top_age_pathways)) {
  episodes.filtered <- episodes.weekly %>% filter(Placement.Pathway == top_age_pathways[i,"Placement.Pathway"] &
                                                    Admission.Age == top_age_pathways[i,"Admission.Age"]) %>%
    mutate(ID = paste0(ID, ".", Simulation), Placement = ifelse(!is.na(Match.Offset) & offset > Match.Offset - 10 & offset < Match.Offset + 10, "Join", Placement)) %>%
    dplyr::select(offset, Simulation, ID, Placement, Period.Duration, Provenance, Match.Offset, Matched.ID, Matched.Offset)
  
  matches <- episodes.filtered %>% filter(!is.na(Matched.ID)) %>% dplyr::distinct(ID, Matched.ID, Match.Offset, Simulation)
  
  ordered <- episodes.filtered %>% group_by(ID) %>%
    summarise(Period.Duration = max(Period.Duration)) %>%
    arrange(Period.Duration)
  
  provenance_labels <- c("Historic Closed", "Historic Open", "Matched Closed", "Projected Closed", "Simulated")
  names(provenance_labels) <- c("H", "PA", "M", "PB", "S")
  
  episodes.filtered$ID <- factor(episodes.filtered$ID, levels = ordered$ID)
  episodes.filtered$Provenance <- factor(episodes.filtered$Provenance, levels = c("H", "PA", "M", "PB", "S"))
  print(episodes.filtered %>%
          filter(Simulation < 20) %>%
          filter((Provenance == "H" & Simulation == 1) | Provenance != "H") %>%
          ggplot(aes(offset, ID, fill = Placement)) +
          geom_tile() +
          scale_fill_manual(values = all.colours) +
          theme_mastodon +
          theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()) +
          facet_grid(cols = vars(Provenance), scales = "free_y", labeller = labeller(Provenance = provenance_labels)) +
          labs(x = "Days in care", y = "Period in care",
               title = paste0("Join age ",top_age_pathways[i,"Admission.Age"], ", pathway ", top_age_pathways[i,"Placement.Pathway"])))
}
dev.off()
embed_fonts(file = output_file_layercake,outfile = output_file_layercake)

## Matched segments

offset_groups <- read.csv("/Users/henry/Mastodon C/witan.cic/offset-groups.csv")
matched_segments <- read.csv("/Users/henry/Mastodon C/witan.cic/matched-segments.csv", col.names = c("Period", "Segment"))

all_matched_segments <- offset_groups %>%
  inner_join(matched_segments, by = c("id" = "Segment"))

all_matched_segments %>%
  group_by(from.placement, id) %>%
  summarise(n = n()) %>%
  ggplot(aes(n, fill = from.placement)) + geom_histogram(bins = 100) +
  facet_wrap(vars(from.placement), scales = "free_y") +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  theme_mastodon +
  labs(title = "Segment occurrence counts by from-placement")

all_matched_segments %>%
  group_by(age, id) %>%
  summarise(n = n()) %>%
  mutate(age = factor(age)) %>%
  ggplot(aes(n, fill = age)) + geom_histogram(bins = 100) +
  facet_wrap(vars(age), scales = "free_y") +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  theme_mastodon +
  labs(title = "Segment occurrence counts by age")

all_matched_segments %>%
  mutate(offset_zero = offset == 0) %>%
  group_by(offset_zero, id) %>%
  summarise(n = n()) %>%
  ggplot(aes(n, fill = offset_zero)) + geom_histogram(bins = 100) +
  facet_wrap(vars(offset_zero), scales = "free_y") +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  theme_mastodon +
  labs(title = "Segment occurrence counts by offset is zero")

tab <- all_matched_segments %>%
  group_by(id, from.placement, to.placement, age, terminal, duration, offset) %>%
  summarise(n = n()) %>%
  filter(n > 80) %>%
  arrange(desc(n)) # %>%
  write.csv(., file = "most-used-segments.csv")
  
  all_matched_segments %>%
    filter(id == 1495510)
  all_matched_segments %>%
    filter(id == 1414)

  episodes %>%
    filter(period_id %in% c("3370-1", "3371-1"))
  
## Age 17 duration in care
  
  episodes <- read.csv(actual_episodes_file, header = TRUE, stringsAsFactors = FALSE, na.strings ="NA")
  episodes$report_date <- ymd(episodes$report_date)
  episodes$ceased <- ymd(episodes$ceased)
  end_date <- max(c(episodes$ceased, episodes$report_date), na.rm = TRUE)
  birthdays <- episodes %>% group_by(ID) %>% summarise(birthday = imputed_birthday(DOB[1], min(report_date), coalesce(max(ceased), end_date)))
  episodes <- episodes %>% inner_join(birthdays)
  episodes <- episodes %>% group_by(phase_id) %>% mutate(admission_age = year_diff(min(birthday), min(report_date))) %>% ungroup
  episodes$placement_category <- substr(episodes$placement, 1, 1)
  
  projected_episodes <- read.csv(projected_episodes_file, header = TRUE, stringsAsFactors = FALSE, na.strings ="")
  projected_episodes$Start <- ymd(projected_episodes$Start)
  projected_episodes$End <- ymd(projected_episodes$End)
  projected_episodes$Birthday <- ymd(projected_episodes$Birthday)
  projected_episodes$Placement.Category <- substr(projected_episodes$Placement, 1, 1)

episodes %>%
  group_by(period_id, birthday) %>%
  summarise(beginning = min(report_date), end = max(ceased)) %>%
  filter(!is.na(end)) %>%
  mutate(birthday_17 = birthday + years(17)) %>%
  mutate(cease_after_birthday_17 = as.integer(end - birthday_17)) %>%
  filter(cease_after_birthday_17 > 0)

projected_periods <- projected_episodes %>%
  group_by(Simulation, Provenance, ID, Birthday) %>%
  summarise(beginning = Start[1], end = End[1]) %>%
  rename(birthday = Birthday, id = ID)

projected_periods_after_17 <- projected_periods %>%
  filter(!is.na(end)) %>%
  mutate(birthday_17 = birthday + years(17)) %>%
  mutate(cease_after_birthday_17 = as.integer(end - birthday_17)) %>%
  filter(cease_after_birthday_17 > 0)

dev.off()
ggplot(projected_periods_after_17, aes(cease_after_birthday_17, fill = Provenance)) +
  geom_histogram(bins = 12) +
  facet_wrap(vars(Provenance), scales = "free_y") +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  theme_mastodon +
  labs(title = "Age 17 leavers are projected more likely to leave before their birthday",
       x = "Days after 17th birthday")


## Survival analysis of durations in care

projected_episodes <- read.csv(projected_episodes_file, header = TRUE, stringsAsFactors = FALSE, na.strings ="")
projected_episodes$Period.Start <- ymd(projected_episodes$Period.Start)
projected_episodes$Period.End <- ymd(projected_episodes$Period.End)
projected_episodes$Birthday <- ymd(projected_episodes$Birthday)
projected_episodes$Placement.Category <- substr(projected_episodes$Placement, 1, 1)
projected_episodes$Admission.Age <- factor(projected_episodes$Admission.Age)

## Attempt 1

historic_episodes <- projected_episodes %>%
  filter(Simulation == 1 && Provenance %in% c("H", "P")) %>%
  group_by(ID) %>%
  slice(1) %>%
  select(Simulation, ID, Period.Start, Period.End, Admission.Age, Birthday, Provenance)

historic_episodes <- historic_episodes %>%
  mutate(Event = if_else(Period.End >= project_from, 0, 1)) %>%
  mutate(Measured.Duration = day_diff(Period.Start, min(Period.End, project_from)),
         Model.Duration = day_diff(Period.Start, Period.End))

impute.quantiles <- function(df) {
  res <- df %>% as.data.frame %>% mutate(`100` = coalesce(`100`, 18:1 * 365))
  res <- t(na.approx(t(res))) %>% as.data.frame
  res <- cbind(age = str_replace(rownames(df),"admission_age=", ""), res)
  colnames(res) <- c("age", 0:100)
  res
}

fit <- survfit(Surv(Measured.Duration, Event) ~ Admission.Age, data = historic_episodes)
survival_quantiles <- stats::quantile(fit, probs = seq(0,1,length.out = 101))
imputed <- impute.quantiles(survival_quantiles$quantile)
imputed$age <- 0:17

imputed_quantiles <- melt(imputed, id.vars = c("age")) %>%
  mutate(Admission.Age = factor(age),
         Group = paste(age),
         variable = as.numeric(variable) / 100.0)

ggplot() +
  geom_line(data = imputed_quantiles, aes(value, variable, group = Admission.Age), linetype = 2, colour = "orange") +
  facet_wrap(vars(Admission.Age))

all_episodes <- projected_episodes %>%
  group_by(ID) %>%
  slice(1) %>%
  select(Simulation, ID, Period.Start, Period.End, Admission.Age, Birthday, Provenance) %>%
  mutate(Model.Duration = day_diff(Period.Start, Period.End))

all_quantiles <- all_episodes %>%
  group_by(Admission.Age, Simulation) %>%
  mutate(quantile = ecdf(Model.Duration)(Model.Duration)) %>%
  arrange(Model.Duration) %>%
  mutate(Group = paste(Simulation, Admission.Age))


ggplot() +
  geom_line(data = all_quantiles, aes(Model.Duration, quantile, group = Group), alpha = 0.05) +
  geom_line(data = imputed_quantiles, aes(value, variable, group = Group), linetype = 3, colour = "red") +
  facet_wrap(vars(Admission.Age), ncol = 6, scales = "free_x") +
  theme_mastodon +
  labs(x = "Duration in care", y = "Quantile", title = "Comparison of estimated and modelled duration in care")

all_quantiles %>%
  mutate(Short.Period = Model.Duration < 250) %>%
  mutate(Leave.Year = factor(year(Period.End))) %>%
  group_by(Admission.Age, Leave.Year, Simulation, Short.Period) %>%
  summarise(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  filter(Short.Period) %>%
  ggplot(aes(Leave.Year, p)) +
  geom_boxplot() +
  facet_wrap(vars(Admission.Age)) +
  theme_mastodon

all_quantiles %>%
  mutate(Short.Period = Model.Duration < 250) %>%
  mutate(Leave.Month = format(Period.End, '%y-%m')) %>%
  group_by(Admission.Age, Leave.Month, Simulation, Short.Period) %>%
  summarise(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  filter(Short.Period) %>%
  ggplot(aes(Leave.Month, p)) +
  geom_boxplot()

all_quantiles <- all_episodes %>%
  mutate(Leave.Year = factor(year(Period.End), levels = 2015:2025)) %>%
  filter(Leave.Year %in% 2015:2025) %>%
  group_by(Leave.Year) %>%
  mutate(quantile = ecdf(Model.Duration)(Model.Duration)) %>%
  arrange(Model.Duration) %>%
  mutate(Group = paste(Leave.Year)) %>%
  

ggplot() +
  geom_line(data = all_quantiles, aes(Model.Duration, quantile, group = Group, colour = Leave.Year), alpha = 1, size = 1) +
  facet_wrap(vars(Admission.Age), ncol = 6, scales = "free_x") +
  theme_mastodon +
  labs(x = "Duration in care", y = "Quantile", title = "Comparison of estimated and modelled duration in care") +
  scale_colour_manual(values = tableau_color_pal("Tableau 20")(20))

## Attempt 2

least <- function(a, b) {
  if_else(a < b, a, b)
}

all_episodes <- projected_episodes %>%
  group_by(ID) %>%
  slice(1) %>%
  ungroup %>%
  select(Simulation, Provenance, ID, Period.Start, Period.End, Admission.Age, Birthday, Provenance) %>%
  inner_join(data.frame(Age = 1:17), by = character()) %>%
  mutate(Report.Date = Birthday + years(Age)) %>%
  filter(Report.Date > Period.Start) %>%
  mutate(Age = factor(Age)) %>%
  mutate(Years.CiC = factor(year_diff(Period.Start,  least(Period.End, Report.Date)))) %>%
  mutate(Event = if_else(Report.Date > Period.End, 1, 0)) %>%
  mutate(Measured.Duration = day_diff(Period.Start, least(Period.End, Report.Date)),
         Model.Duration = day_diff(Period.Start, Period.End))

fit <- survfit(Surv(Measured.Duration, Event) ~ Age + Years.CiC, data = all_episodes %>% filter(Simulation == 1))
survival_quantiles <- stats::quantile(fit, probs = seq(0,1,length.out = 101))

factors <- data.frame(x = rownames(survival_quantiles$quantile)) %>%
  mutate(vals = gsub("[a-zA-Z= .]", "", x)) %>%
  mutate(age = factor(as.numeric(gsub(",.*", "", vals))),
         years = factor(as.numeric(gsub(".*,", "", vals)))) %>%
  select(age, years)

imputed <- cbind(factors, survival_quantiles$quantile) %>%
  mutate(`100` = if_else(is.na(`100`), (18 - as.numeric(age)) * 365, `100`))

imputed_quantiles <- melt(imputed, id.vars = c("age", "years")) %>%
  filter(!is.na(value)) %>%
  mutate(Age = factor(age),
         Years.CiC = factor(years),
         Group = paste(age, years),
         variable = as.numeric(variable) / 100.0)

ggplot() +
  geom_line(data = imputed_quantiles, aes(value, variable, group = Age), linetype = 2, colour = "orange") +
  facet_grid(vars(Age), vars(Years.CiC)) +
  theme_mastodon

all_quantiles <- all_episodes %>%
  group_by(Age, Years.CiC, Simulation) %>%
  mutate(quantile = ecdf(Model.Duration)(Model.Duration)) %>%
  arrange(Model.Duration) %>%
  mutate(Group = paste(Simulation, Age, Years.CiC))

ggplot() +
  geom_line(data = all_quantiles, aes(Model.Duration, quantile, group = Group), alpha = 0.05) +
  geom_line(data = imputed_quantiles, aes(value, variable, group = Group), linetype = 3, colour = "red") +
  facet_grid(vars(Age), vars(Years.CiC)) +
  theme_mastodon +
  labs(x = "Duration in care", y = "Quantile", title = "Comparison of estimated and modelled duration in care")
             


## Plot durations by year and starting age


periods <- projected_episodes %>%
  group_by(Simulation, ID) %>%
  slice(1) %>%
  ungroup


periods %>%
  filter(Admission.Age == 0) %>%
  inner_join(data.frame(sample_date = seq(project_from - years(8), project_from, '1 year'),
                        sample_label = factor(seq(project_from - years(8), project_from, '1 year'))), by = character()) %>%
  filter(sample_date >= Period.Start & sample_date <= Period.End) %>%
  group_by(sample_label) %>%
  mutate(group = paste(sample_label)) %>%
  # mutate(Period.Duration = Period.Duration + rnorm(1, sd = 7)) %>%
  mutate(cdf = ecdf(Period.Duration)(Period.Duration)) %>%
  ungroup %>%
  ggplot(aes(Period.Duration, cdf, group = group, colour = sample_label)) +
  geom_line() +
  scale_color_manual(values = tableau_color_pal("Tableau 20")(20)) +
  labs(title = "Sampled cdf of age 0 joiners by year") +
  theme_mastodon


periods %>%
  filter(Admission.Age == 0) %>%
  mutate(sample_label = year(Period.End - months(7) - days(12)) - 2019) %>%
  filter(sample_label %in% -5:5) %>%
  mutate(sample_label = if_else(sample_label < 0, "Historic", "Projected")) %>%
  group_by(sample_label) %>%
  mutate(group = paste(sample_label)) %>%
  # mutate(Period.Duration = Period.Duration + rnorm(1, sd = 7)) %>%
  mutate(cdf = ecdf(Period.Duration)(Period.Duration)) %>%
  ungroup %>%
  ggplot(aes(Period.Duration, cdf, group = group, colour = sample_label)) +
  geom_line() +
  scale_color_manual(values = tableau_color_pal("Tableau 20")(20)) +
  labs(title = "Sampled cdf of age 0 joiners split by historic & projected") +
  theme_mastodon +
  xlim(c(0,800)) +
  ylim(c(0,1))
