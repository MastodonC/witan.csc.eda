library(dplyr)
library(lubridate)

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
dev.off()


train_yrs <- 1
project_yrs <- 2
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

output_all_charts(1)
output_all_charts(2)
output_all_charts(3)
output_all_charts(4)
output_all_charts(5)

