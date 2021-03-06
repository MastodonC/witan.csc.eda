library(survminer)
library(survival)

Executing src/close-open-cases.R   2019-01-24 euclidean_scaled 3 1416938423

Executing src/close-open-cases.R   2019-01-24 euclidean_scaled 3 1416938423


input <- "/var/folders/tw/tzkjfqd11md5hdjcyjxgmf740000gn/T/file9047937759879621107.csv"
script_path <- "../witan.cic/src/close-open-cases.R"
project_from <- as.Date("2020-01-24")
feature_tiers <- 1
algo <- "euclidean_scaled"

output_csv <- paste0("data/cluster_out_", algo, "_", feature_tiers, ".csv")


create_clusters <- function(script_path, input_csv, output_csv, project_from, algo, feature_tiers) {
  system(paste("RScript", "--vanilla", script_path, input_csv, output_csv, project_from, algo, feature_tiers, "42"),
         intern = FALSE,
         ignore.stdout = TRUE,
         ignore.stderr = FALSE,
         wait = TRUE)
}

create_clusters(script_path, input, output_csv, project_from, algo, feature_tiers)


output_csv <- "/var/folders/tw/tzkjfqd11md5hdjcyjxgmf740000gn/T/file2882096293486238497.csv"
clustered <- read.csv(output_csv) %>%
  group_by(open) %>%
  arrange(k) %>%
  mutate(n = 1:length(k)) %>%
  ungroup

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
episodes$end <- ymd(episodes$end)
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
  inner_join(clustered %>% filter(n <= 1), by = c("period_id" = "open")) %>%
  inner_join(closed.periods, by = c("closed" = "period_id"), suffix = c(".open", ".closing.sample")) %>%
  mutate(duration.projected = duration.open + (duration.closing.sample - offset)) %>%
  mutate(duration.projected = ifelse(duration.projected >= max_days, max_days, duration.projected)) %>%
  select(period_id, duration.open, duration.closing.sample, duration.projected)

projected.closed %>%
  melt(id.vars = "period_id") %>%
  rbind(periods %>% filter(!open) %>% mutate(variable = "duration.closed") %>% select(period_id, variable, value = duration)) %>%
  ggplot(aes(value)) + geom_histogram(bins = 200) + facet_grid(rows = vars(variable)) +
  theme_mastodon +
  labs(title = paste("Comparison of open, closed and projected durations, SCC:", algo),
       y = "CiC", x = "Duration (days)")


fit <- survfit(Surv(duration, event) ~ 1, data = periods)
surv.quantiles <- quantile(fit, probs = seq(0,1,length.out = 100))$quantile

open.closed.duration <- c(closed.periods$duration, projected.closed$duration.projected)
model.quantiles <- quantile(ecdf(open.closed.duration), probs = seq(0,1,length.out = 100))

both.quantiles <- data.frame(quantile = seq(0,1,length.out = 100), survival.q = surv.quantiles, model.q = model.quantiles)

melt(both.quantiles, id.vars = "quantile") %>%
  ggplot(aes(value, quantile, colour = variable)) +
  geom_line() +
  labs(title = paste("SCC cumulative projected and model durations:", algo, feature_tiers, "k = 1")) +
  theme_mastodon


##

top_candidate <- clustered %>%
  filter(n == 1) %>%
  group_by(closed, offset) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  dplyr::select(closed, offset, n) %>%
  first
  
candidates <- clustered %>%
  inner_join(top_candidate, by = c("closed", "offset"))

max_duration <- (episodes %>% group_by(period_id) %>%
  mutate(period_duration = day_diff(min(birthday), coalesce(max(end), max(report_date)))) %>%
  arrange(desc(period_duration)) %>% first)$period_duration

projection_start <- max(max(episodes$report_date), max(episodes$ceased, na.rm = TRUE))

offsets <- seq(0, max_duration, 7)

episodes.offsets <- episodes %>%
  group_by(period_id) %>%
  mutate(offset = day_diff(birthday, report_date),
         offset.end = day_diff(birthday, coalesce(end, projection_start)))

episodes.weekly <- data.frame(offset = offsets) %>%
  inner_join(episodes.offsets, by = character()) %>%
  filter(offset.x >= offset.y & offset.x <= offset.end)

all.placements <- c("A3", "A4", "A5", "A6", "H5", "K1", "K2", "M2", "M3", "P1", "P2",
                    "Q1","Q2", "R1", "R2", "R3", "R5", "S1", "T0", "T4", "Z1",
                    'Splice')

my.colours <- tableau_color_pal("Tableau 20")(20)
my.colours <- c(my.colours, "#888888", "#FFFFFF")

all.colours <- c(my.colours, brightness(my.colours, 1.2))
all.placements <- c(all.placements, paste0(all.placements, ".P"))
names(all.colours) <- all.placements

episodes.filtered <- episodes.weekly %>%
  mutate(label = if_else(period_id %in% candidates$open, "Open", if_else(period_id %in% candidates$closed, "Closed", NULL))) %>%
  filter(!is.na(label)) %>%
  left_join(top_candidate, by = c("period_id" = "closed")) %>%
  mutate(placement = if_else(!is.na(offset) & abs(offset - offset.x + day_diff(birthday,beginning)) <= 7, "Splice", placement))

  print(episodes.filtered %>%
          ggplot(aes(offset.x, period_id, fill = placement)) +
          geom_tile() +
          scale_fill_manual(values = all.colours) +
          theme_mastodon +
          theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()) +
          facet_wrap(vars(label)) +
          labs(x = "Days in care", y = "Period in care"))

