library(survminer)
library(survival)

input <- "/var/folders/tw/tzkjfqd11md5hdjcyjxgmf740000gn/T/file6614522820192472178.csv"
script_path <- "../witan.cic/src/close-open-cases.R"
project_from <- as.Date("2020-01-24")
feature_tiers <- 3
algo <- "euclidean_scaled"

output_csv <- paste0("data/cluster_out_", algo, "_", feature_tiers, ".csv")

create_clusters <- function(script_path, input_csv, output_csv, project_from, algo, feature_tiers) {
  system(paste("RScript", "--vanilla", script_path, input_csv, output_csv, project_from, algo, feature_tiers, "42"),
         intern = FALSE,
         ignore.stdout = TRUE,
         ignore.stderr = FALSE,
         wait = TRUE)
}

create_clusters(script_path, input_csv, output_csv, project_from, algo, feature_tiers)

clustered <- read.csv(output_csv) %>%
  group_by(open) %>%
  arrange(k) %>%
  mutate(n = 1:length(k)) %>%
  ungroup

clustered %>% filter(open == "2526-1")

episodes <- read.csv(input, na.strings = "") %>%
  mutate(open = open == "true") %>%
  group_by(period_id) %>%
  arrange(period_id, report_date) %>%
  mutate(ceased = ifelse(is.na(lead(report_date)) & !open, end, lead(report_date))) %>%
  ungroup %>%
  as.data.frame

episodes$report_date <- ymd(episodes$report_date)
episodes$ceased <- ymd(episodes$ceased)
episodes$birthday <- ymd(episodes$birthday)
max_days <- 18 * 365.25

periods <- episodes %>%
  group_by(period_id) %>%
  dplyr::summarise(event = ifelse(open[1], 0, 1),
            open = open[1],
            duration = day_diff(min(report_date), coalesce(max(ceased), project_from)),
            age_of_entry = day_diff(birthday[1], min(report_date)))

open.periods <- periods %>% filter(open)
closed.periods <- periods %>% filter(!open)

projected.closed <- open.periods %>%
  inner_join(clustered %>% filter(n <= 10), by = c("period_id" = "open")) %>%
  inner_join(closed.periods, by = c("closed" = "period_id"), suffix = c(".open", ".closing.sample")) %>%
  mutate(duration.projected = duration.open + (duration.closing.sample - offset)) %>%
  mutate(duration.projected = ifelse(duration.projected >= max_days, max_days, duration.projected)) %>%
  select(period_id, duration.open, duration.closing.sample, duration.projected)

# projected.closed %>%
#   melt(id.vars = "period_id") %>%
#   rbind(periods %>% filter(!open) %>% mutate(variable = "duration.closed") %>% select(period_id, variable, value = duration)) %>%
#   ggplot(aes(value)) + geom_histogram(bins = 200) + facet_grid(rows = vars(variable)) +
#   theme_mastodon +
#   labs(title = paste("Comparison of open, closed and projected durations, SCC:", algo),
#        y = "CiC", x = "Duration (days)")


fit <- survfit(Surv(duration, event) ~ 1, data = periods)
surv.quantiles <- quantile(fit, probs = seq(0,1,length.out = 100))$quantile

open.closed.duration <- c(closed.periods$duration, projected.closed$duration.projected)
model.quantiles <- quantile(ecdf(open.closed.duration), probs = seq(0,1,length.out = 100))

both.quantiles <- data.frame(quantile = seq(0,1,length.out = 100), survival.q = surv.quantiles, model.q = model.quantiles)

melt(both.quantiles, id.vars = "quantile") %>%
  ggplot(aes(value, quantile, colour = variable)) +
  geom_line() +
  labs(title = paste("SCC cumulative projected and model durations:", algo, feature_tiers, "k = 10")) +
  theme_mastodon




