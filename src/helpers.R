
library(ggplot2)

chart_title <- function(title){
  paste(la_label, "-", title)
}

chart_path <- function(path) {
  file.path(output_dir, "charts", paste0(Sys.Date(),"-",basename(path)))
}

year_start <- function(year) {
  as.Date(paste0(year, "-01-01"))
}

year_end <- function(year) {
  year_start(year + 1) - 1
}

month_start <- function(month) {
  as.Date(paste0(month, "-01"))
}

month_end <- function(month) {
  as.Date(paste0(month, "-01")) + months(1) - days(1)
}

years_before <- function(date, ys) {
  date - years(ys)
}

date_between <- function(start, end) {
  out <- numeric(length = length(start))
  for(i in seq_along(start)) {
    out[i] <- sample(seq(min(start[i], end[i]),
                         max(start[i], end[i]),
                         by = "day"), 1)
  }
  as.Date(out)
}


birthday_before_date <- function(birth_date, other_date) {
  yr <- year(other_date)
  a <- birth_date
  year(a) <- yr
  b <- birth_date
  year(b) <- (yr - 1)
  if (a > other_date){
    b
  } else {
    a
  }
}

imputed_birthday <- function(birth_month, min_start, max_cease) {
  earliest_possible <- max(max_cease - days(floor(18 * 365.25)) + 1, month_start(birth_month))
  latest_possible <- min(min_start, month_end(birth_month))
  date_between(earliest_possible, latest_possible)
}

year_diff <- function(start, stop) {
  as.numeric(difftime(stop, start, units = "days")) %/% 365.25
}

month_diff <- function(start, stop) {
  as.numeric(difftime(stop, start, units = "days")) %/% 30
}

day_diff <- function(start, stop) {
  as.numeric(difftime(stop, start, units = "days"))
}

theme_mastodon <- theme(plot.title = element_text(family = "Open Sans SemiBold",
                                                  hjust = 0.5, size = 20,
                                                  margin = margin(0,0,15,0)),
                        axis.title = element_text(family = "Open Sans SemiBold",
                                                  hjust = 0.5, size = 16),
                        axis.text = element_text(family = "Open Sans",
                                                 hjust = 0.5, size = 10),
                        axis.text.x = element_text(angle = -45),
                        axis.title.x = element_text(margin = margin(15,0,0,0)),
                        axis.title.y = element_text(margin = margin(0,10,0,0)),
                        plot.margin = margin(10,20,10,10),
                        panel.grid = element_line(color = "#FFFFFF"))

impute.quantiles <- function(df) {
  res <- df %>% as.data.frame %>% mutate(`100` = coalesce(`100`, 18:1 * 365))
  res <- t(na.approx(t(res))) %>% as.data.frame
  res <- cbind(age = str_replace(rownames(df),"admission_age=", ""), res)
  colnames(res) <- c("age", 0:100)
  res
}

assoc.period.id <- function(episodes) {
  episodes <- episodes %>% arrange(ID, report_date)
  new_periods <- coalesce(episodes$ID == lag(episodes$ID) & episodes$report_date > lag(episodes$ceased), FALSE)
  episodes$period_id <- paste0(episodes$ID, "-", ave(ifelse(new_periods, 1.0, 0.0), episodes$ID, FUN = cumsum) + 1)
  episodes
}

