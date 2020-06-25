library(dplyr)
library(lubridate)
library(tidyquant)

theme_mastodon <- theme(plot.title = element_text(family = "Open Sans SemiBold", hjust = 0.5, size = 20,
                                                  margin = margin(0,0,15,0)),
                        axis.title = element_text(family = "Open Sans SemiBold", hjust = 0.5, size = 16),
                        axis.text = element_text(family = "Open Sans", hjust = 0.5, size = 10),
                        axis.text.x = element_text(angle = -45),
                        axis.title.x = element_text(margin = margin(15,0,0,0)),
                        axis.title.y = element_text(margin = margin(0,10,0,0)),
                        plot.margin = margin(10,20,10,10),
                        panel.background = element_blank(),
                        panel.grid = element_line(color = "#eeeeee"))


font_import()
loadfonts()

output_all_charts <- function(la_label, train_yrs, project_yrs) {
  set.seed(5)
  output_file <- chart_path(paste0(la_label, "-train-", train_yrs, "-yr-project-", project_yrs, "yr.pdf"))
  print(input_dir)
  print(output_dir)
  if (!dir.exists(dirname(output_file)))
    dir.create(dirname(output_file), recursive = TRUE)
  pdf(output_file, fonts = c("Open Sans", "Open Sans SemiBold"), paper = "a4r")
  projection_name <- paste0("episodes-rewind-1yr-train-", train_yrs, "yr-project-", project_yrs, "yr-runs-100-seed-42")
  episodes <- read.csv(file.path(input_dir, "episodes.scrubbed.csv"), header = TRUE, stringsAsFactors = FALSE, na.strings ="NA")
  episodes$report_date <- ymd(episodes$report_date)
  episodes$ceased <- ymd(episodes$ceased)
  birthdays <- episodes %>%
    group_by(ID) %>%
    summarise(birthday = imputed_birthday(DOB[1], min(report_date), coalesce(max(ceased), end_date)))
  episodes <- episodes %>% inner_join(birthdays)
  episodes <- episodes %>% group_by(phase_id) %>% mutate(admission_age = year_diff(min(birthday), min(report_date))) %>% ungroup
  episodes$placement_category <- substr(episodes$placement, 1, 1)
  
  projected_episodes <- file.path(input_dir, paste0(projection_name, "-episodes-filter.csv"))
  projected_episodes <- read.csv(projected_episodes, header = TRUE, stringsAsFactors = FALSE, na.strings ="")
  projected_episodes$Start <- ymd(projected_episodes$Start)
  projected_episodes$End <- ymd(projected_episodes$End)
  projected_episodes$Birthday <- ymd(projected_episodes$Birthday)
  projected_episodes$Placement.Category <- substr(projected_episodes$Placement, 1, 1)
  
  dates <- seq(as.Date("2016-01-01"), as.Date("2020-02-01"), by = "week")
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
  for (date in dates) {
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
    theme_mastodon +
    scale_color_manual(values = colours) +
    labs(title = chart_title("CiC"), x = "Date", y = "CiC"))
  
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
    for (date in dates) {
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
            theme_mastodon +
            scale_color_manual(values = colours) +
            labs(title = chart_title(paste0(test.placement)), x = "Date", y = "CiC"))
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
    for (date in dates) {
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
      theme_mastodon +
      scale_color_manual(values = colours) +
      labs(title = chart_title(paste0(test.placement)), x = "Date", y = "CiC"))
  }
  
  for (test.age in 0:17) {
    projected <- data.frame(date = c(), lower.ci = c(), q1 = c(), median = c(), q3 = c(), upper.ci = c())
    for (date in dates) {
      date <- as.Date(date)
      counts_by_simulation <- projected_episodes %>%
        filter(Start <= date & (is.na(End) | End >= date)) %>%
        filter(year_diff(Birthday, date) == test.age) %>%
        group_by(Simulation) %>%
        summarise(n = n())
      quants <- quantile(counts_by_simulation$n, probs = c(0.05, 0.25, 0.5, 0.75, 0.975))
      projected <- rbind(projected, data.frame(date = c(date), lower.ci = c(quants[1]), q1 = c(quants[2]), median = c(quants[3]), q3 = c(quants[4]), upper.ci = c(quants[5])))
    }
    projected$date <- as.Date(projected$date)
    projected <- projected %>% filter(lower.ci != upper.ci)
    actuals <- data.frame(date = c(), variable = c(), value = c())
    for (date in dates) {
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
      theme_mastodon +
      scale_color_manual(values = colours) +
      labs(title = chart_title(paste0("Age ", test.age)), x = "Date", y = "CiC"))
  }
  
  # Joiners
  
  join_leave_projected <- projected_episodes %>%
    group_by(Simulation, ID) %>%
    summarise(Join = min(Start),
              Leave = max(End),
              Birthday = Birthday[1]) %>%
    mutate(Join.Age = year_diff(Birthday, Join),
           Leave.Age = year_diff(Birthday, Leave))
  
  join_leave_actual_summary <- episodes %>%
    group_by(period_id) %>%
    summarise(Join = min(report_date),
              Leave = max(ceased),
              Birthday = birthday[1]) %>%
    mutate(Join.Age = year_diff(Birthday, Join),
           Leave.Age = year_diff(Birthday, Leave))
  
  
  
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
    filter(date > as.Date("2018-12-01"))
  
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
    filter(date > as.Date("2018-12-01"))
  
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
  
  print(ggplot() +
    geom_line(data = join_actuals, aes(x = date, y = value)) +
    geom_line(data = join_projected_ci, aes(x = date, y = median), linetype = 2) +
    geom_ribbon(data = join_projected_ci, aes(x = date, ymin = lower.ci, ymax = upper.ci), fill = "gray", alpha = 0.3) +
    geom_ribbon(data = join_projected_ci, aes(x = date, ymin = q1, ymax = q3), fill = "gray", alpha = 0.3) +
    theme_mastodon +
    scale_color_manual(values = colours) +
    labs(title = chart_title("Joiners per month"), x = "Date", y = "CiC") +
    coord_cartesian(xlim = c(as.Date("2018-01-01"), as.Date("2020-01-01"))))
  
  print(ggplot() +
    geom_line(data =  leave_actuals, aes(x = date, y = value)) +
    geom_line(data = leave_projected_ci, aes(x = date, y = median), linetype = 2) +
    geom_ribbon(data = leave_projected_ci, aes(x = date, ymin = lower.ci, ymax = upper.ci), fill = "gray", alpha = 0.3) +
    geom_ribbon(data = leave_projected_ci, aes(x = date, ymin = q1, ymax = q3), fill = "gray", alpha = 0.3) +
    theme_mastodon +
    scale_color_manual(values = colours) +
    labs(title = chart_title("Leavers per month"), x = "Date", y = "CiC") +
    coord_cartesian(xlim = c(as.Date("2018-01-01"), as.Date("2020-01-01"))))
  
  for (test.age in 0:17){
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
    
    print(ggplot() +
      geom_line(data = join_actuals, aes(x = date, y = value)) +
      geom_line(data = join_projected_ci, aes(x = date, y = median), linetype = 2) +
      geom_ribbon(data = join_projected_ci, aes(x = date, ymin = lower.ci, ymax = upper.ci), fill = "gray", alpha = 0.3) +
      geom_ribbon(data = join_projected_ci, aes(x = date, ymin = q1, ymax = q3), fill = "gray", alpha = 0.3) +
      theme_mastodon +
      scale_color_manual(values = colours) +
      labs(title = chart_title(paste0("Age ", test.age, " joiners per month")), x = "Date", y = "CiC") +
      coord_cartesian(xlim = c(as.Date("2018-01-01"), as.Date("2020-01-01"))))
    
    print(ggplot() +
      geom_line(data = leave_actuals, aes(x = date, y = value)) +
      geom_line(data = leave_projected_ci, aes(x = date, y = median), linetype = 2) +
      geom_ribbon(data = leave_projected_ci, aes(x = date, ymin = lower.ci, ymax = upper.ci), fill = "gray", alpha = 0.3) +
      geom_ribbon(data = leave_projected_ci, aes(x = date, ymin = q1, ymax = q3), fill = "gray", alpha = 0.3) +
      theme_mastodon +
      scale_color_manual(values = colours) +
      labs(title = chart_title(paste0("Age ", test.age, " leavers per month")), x = "Date", y = "CiC") +
      coord_cartesian(xlim = c(as.Date("2018-01-01"), as.Date("2020-01-01"))))
  }
  dev.off()
  embed_fonts(file = output_file,outfile = output_file)
}


plot_summary <- function(input_dir, train_yrs, project_yrs, test.placement) {
  projection_name <- paste0("episodes-rewind-1yr-train-", train_yrs, "yr-project-", project_yrs, "yr-runs-100-seed-42")
  projected_episodes <- file.path(input_dir, paste0(projection_name, "-episodes-filter.csv"))
  projected_episodes <- read.csv(projected_episodes, header = TRUE, stringsAsFactors = FALSE, na.strings ="")
  projected_episodes$Start <- ymd(projected_episodes$Start)
  projected_episodes$End <- ymd(projected_episodes$End)
  projected_episodes$Birthday <- ymd(projected_episodes$Birthday)
  projected_episodes <- projected_episodes %>% group_by(Simulation, ID) %>% mutate(Min.Start = min(Start), Max.End = max(End)) %>% ungroup
  projected_periods <- projected_episodes %>% group_by(Simulation, ID) %>% summarise(Min.Start = min(Start), Max.End = max(End)) %>% ungroup
  dates <- seq(as.Date("2016-01-01"), as.Date("2021-01-01"), by = "week")
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
    geom_vline(aes(xintercept = as.Date("2019-03-01"), colour = "#444444"), linetype = 2) +
    geom_ribbon(alpha = 0.15, aes(fill = colours[1], ymin = joiners.count / 2.0, ymax = (joiners.count + cic.count) / 2.0, x = date)) +
    geom_line(aes(x = date, y = cic.count, colour = colours[1])) +
    geom_line(aes(x = date, y = median.open.duration,  colour = colours[2])) +
    geom_line(aes(x = date, y = median.closed.duration, colour = colours[3])) +
    geom_ma(aes(x = date, y = median.closed.duration, colour = colours[4]), n = 12) +
    labs(title = chart_title("Total CiC"), color = "Counted", fill = "Shaded", x = "Date") +
    scale_color_manual(values = c("#444444","#4E79A7", "#A0CBE8", "#FFBE7D", "#F28E2B"),
                       labels = c("Projection start", "CiC Count", "Median Open Duration", "Closed Duration (Prior Month Average)", "Closed Duration Moving Average")) +
    scale_fill_manual(values = c(colours[1], "#CCCCCC"),
                      labels = c("Total CiC", "Projected Period")) +
    scale_y_continuous("Counted", sec.axis = sec_axis(~ . * 2, name = "Shaded")) +
    theme_mastodon)

  # CCC Q1 & R2
  # NCC H5 & Q1
  # SCC P2 & R2

  # test.placement <- "Q1"
  # test.placement <- "R2"
  # test.placement <- "P2"
  # test.placement <- "Q2"
  # test.placement <- "K2"

  projected <- data.frame(date = c(), joiners.count = c(), cic.count = c(), median.open.duration = c(), median.closed.duration = c())
  for (date in dates) {
    date <- as.Date(date)
    in.cic <- projected_episodes %>%
      filter(Start <= date & (is.na(End) | End >= date) & Placement == test.placement) %>%
      group_by(Simulation) %>%
      summarise(n = n(), median.open.duration = as.integer(median(date - Start)))
    joiners <- projected_episodes %>%
      filter(Start <= date & Placement == test.placement) %>%
      group_by(Simulation) %>%
      summarise(n = n())
    leavers <- projected_episodes %>%
      filter(End < date & End >= date - months(1) & Placement == test.placement) %>%
      group_by(Simulation) %>%
      summarise(median.closed.duration = as.integer(median(date - Start)))
    projected <- rbind(projected, data.frame(date = c(date), joiners.count = c(median(joiners$n)), cic.count = c(median(in.cic$n)), median.open.duration = c(median(in.cic$median.open.duration)),
                                             median.closed.duration = median(leavers$median.closed.duration)))
  }
  projected$joiners.count <- projected$joiners.count - min(projected$joiners.count)

  sec_axis_scale <- 0.2
  print(ggplot(projected, aes(x = date)) +
          geom_vline(aes(xintercept = as.Date("2019-03-01"), colour = "#444444"), linetype = 2) +
          geom_ribbon(alpha = 0.15, aes(fill = colours[1], ymin = joiners.count / sec_axis_scale, ymax = (joiners.count + cic.count) / sec_axis_scale, x = date)) +
          geom_line(aes(x = date, y = cic.count, colour = colours[1])) +
          geom_line(aes(x = date, y = median.open.duration,  colour = colours[2])) +
          geom_line(aes(x = date, y = median.closed.duration, colour = colours[3])) +
          geom_ma(aes(x = date, y = median.closed.duration, colour = colours[4]), n = 12) +

          labs(title = chart_title(test.placement), color = "Counted", fill = "Shaded", x = "Date") +
          scale_color_manual(values = c("#444444","#4E79A7", "#A0CBE8", "#FFBE7D", "#F28E2B"),
                             labels = c("Projection start", "CiC Count", "Median Open Duration", "Closed Duration (Prior Month Average)", "Closed Duration Moving Average")) +
          scale_fill_manual(values = c(colours[1], "#CCCCCC"),
                            labels = c("Total CiC", "Projected Period")) +
          scale_y_continuous("Counted", sec.axis = sec_axis(~ . * sec_axis_scale, name = "Shaded")) +
          theme_mastodon)

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
  by_age$variable <- factor(by_age$variable, levels = rev(c("Joined", 0:18)))
  n_factors <- length(unique(by_age$variable))
  cols <- colours
  cols[n_factors] <- NA
  ggplot(by_age, aes(x = date, y = value, fill = variable)) +
    geom_vline(aes(xintercept = as.Date("2019-01-01")), colour = "black", linetype = 2) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = cols) +
    theme_mastodon +
    labs(fill = "Age", title = chart_title(test.placement), x = "Date", y = "Cumulative Count")
}

# plot_summary(input_dir, 3, 2, "K2")

