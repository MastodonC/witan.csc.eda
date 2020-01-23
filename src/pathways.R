library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(ggplot2)
library(glm2)
library(NHPoisson)
library(ggthemes)
library(MASS)
library(survival)
library(survminer)
library(zoo)
library(networkD3)

## Update with name of local authority
la_label <- "Your_LA_Here"
districts <- c("LA", "Districts", "Here")

chart_title <- function(title){
  paste(la_label, "-", title)
}

chart_path <- function(path) {
  file.path(dirname(path), paste0(Sys.Date(),"-",basename(path)))
}

## If you need to install Open Sans for Mastodon theme. Make sure Open Sans is downloaded and installed.
install.packages("extrafont")
library(extrafont)
font_import()
## End font import

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

placement.pathways <- function(episodes) {
  tmp <- episodes %>%  group_by(period_id, phase_number, placement) %>% summarise() %>% as.data.frame
  tmp <- dcast(period_id ~ phase_number, value.var = "placement", data = tmp)
  periods <- tmp[[1]]
  tmp <- tmp[,-1]
  pathways <- data.frame(period_id = periods, pathway = sub("-$", "", gsub('(.{2})', '\\1-', as.character(tmp %>% apply(1, function(...) gsub('NA', '', paste(...)), collapse="")))))
  pathways$period_id <- as.character(pathways$period_id)
  pathways
}

year_start <- function(year) {
  as.Date(paste0(year, "-01-01"))
}

year_end <- function(year) {
  year_start(year + 1) - 1
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




imputed_birthday <- function(birth_year, min_start, max_cease) {
  earliest_possible <- max(max_cease - days(floor(18 * 365.25)) + 1, year_start(birth_year))
  latest_possible <- min(min_start - 1, year_end(birth_year))
  date_between(earliest_possible, latest_possible)
}


year_diff <- function(start, stop) {
  as.numeric(difftime(stop, start, units = "days")) %/% 365.25
}


episodes2periods <- function(episodes) {
  latest_cease <- max(episodes[!is.na(episodes$ceased),]$ceased)
  periods <- episodes %>%
    group_by(period_id) %>%
    summarise(birth_year = DOB[1],
              birthday = birthday[1],
              duration = as.integer(dplyr::coalesce(max(ceased), latest_cease) - min(report_date)),
              open = is.na(max(ceased)),
              first_placement = placement[1],
              beginning = min(report_date),
              end = max(ceased)) %>%
    mutate(admission_age = year_diff(birthday, beginning),
           exit_age = year_diff(birthday, end),
           event = ifelse(open, 0, 1)) %>%
    as.data.frame
}

date_after <- function(date) {
  # Create an arbitrary date in the same year as date, but falling on or after it
  year <- year(date)
  next_year <- ymd(paste0(year + 1, "-01-01"))
  date + runif(length(date), min = 0, max = interval(date, next_year) / days(1))
}

episodes <- read.csv("./data/episodes.scrubbed.csv", header = TRUE, stringsAsFactors = FALSE, na.strings ="NA")
episodes$report_date <- ymd(episodes$report_date)
episodes$ceased <- ymd(episodes$ceased)

end_date <- max(max(episodes$report_date), max(episodes$ceased, na.rm = TRUE))

# Do we appear to have anyone over 18?

birthdays <- episodes %>%
  group_by(ID) %>%
  summarise(birthday = imputed_birthday(DOB[1], min(report_date), coalesce(max(ceased), end_date)))

episodes <- episodes %>% inner_join(birthdays)

episodes %>% group_by(ID) %>%
  summarise(age = ifelse(is.na(max(ceased)), year_diff(min(birthday), end_date), year_diff(min(birthday), max(ceased)))) %>%
  filter(age > 17)

episodes <- episodes %>% filter(age < 18)

periods <- episodes2periods(episodes)

## Analyse common pathways

dat1 <- episodes %>% dplyr::select(period_id, placement) %>% unique
dat1 %>% inner_join(dat1, by = "period_id")  %>%
  group_by(placement.x, placement.y) %>%
  summarise(n = n()) %>%
  ggplot(aes(placement.y, n, fill = placement.y)) +
  geom_bar(stat = "identity") +
  facet_wrap(vars(placement.x), scales = "free_y") +
  theme_mastodon +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20))

periods %>%
  group_by(admission_age, first_placement) %>%
  summarise(n = n()) %>%
  ggplot(aes(admission_age, n, fill = first_placement)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_mastodon +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  labs(title = "First placement is highly age-dependent")


episodes %>%
  filter(period_id %in% (episodes %>% filter(phase_number == 1 & placement == "Q2"))$period_id) %>%
  filter(phase_number == 2) %>%
  dplyr::select(period_id, placement, birthday, report_date) %>%
  unique %>%
  mutate(age = year_diff(birthday, report_date)) %>%
  group_by(age, placement) %>%
  summarise(n = n()) %>%
  ggplot(aes(age, n, fill = placement)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_mastodon +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  labs(title = "Second placement after Q2 is highly age-dependent (A5 vs K2 vs H5)")

## Leaver joiner mover rate by age

ages <-  data.frame(age = 0:17)

period_birthdays <- episodes %>% dplyr::select(period_id, birthday) %>% unique %>% mutate(join = "x")
ages <- data.frame(age = 0:17, join = "x")
period_ages <- period_birthdays %>% inner_join(ages, by = "join") %>%
  mutate(date = birthday + years(age)) %>%
  filter(date < as.Date("2019-03-31"))

placements_plus <- rbind(episodes %>% dplyr::select(period_id, report_date, ceased, placement),
                         periods %>% filter(event == 1) %>% dplyr::select(period_id, end) %>% rename(report_date = end) %>% mutate(ceased = as.Date("2050-03-31"), placement = "OUT"))

period_age_placements <- period_ages %>%
  inner_join(placements_plus, by = "period_id") %>%
  filter(date >= report_date & (is.na(ceased) | ceased > date))


transitions <- period_age_placements %>% filter(date < as.Date("2018-03-31")) %>% dplyr::select(period_id, age, placement) %>% 
  inner_join(period_age_placements %>% dplyr::select(period_id, age, placement), by = "period_id") %>%
  filter(age.x == age.y - 1) %>%
  mutate(next_placement = ifelse(placement.y == "OUT", "Cease", ifelse(placement.y == placement.x, "Remain", "Move"))) %>%
  group_by(placement.x, age.x, next_placement) %>%
  summarise(n = n())

transitions %>%
  filter(placement.x %in% c("Q2")) %>%
  ggplot(aes(age.x, n, fill = next_placement)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_mastodon +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  labs(title = "Likelihood of moving, ceasing, remaining within year, Q2 only")

transitions %>%
  filter(placement.x %in% c("Q1", "Q2", "A6", "K2")) %>%
  ggplot(aes(age.x, n, fill = next_placement)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(vars(placement.x), scales = "free_y") +
  theme_mastodon +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  labs(title = "Likelihood of moving, ceasing, remaining within year")

period_age_placements %>%
  filter(placement != "OUT") %>%
  group_by(age, placement) %>%
  summarise(n = n()) %>%
  ggplot(aes(age, n, fill = placement)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_mastodon +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  labs(title = "Placement mix of all children in care")

## Use only initial letters

ages <-  data.frame(age = 0:17)

period_birthdays <- episodes %>% dplyr::select(period_id, birthday) %>% unique %>% mutate(join = "x")
ages <- data.frame(age = 0:17, join = "x")
period_ages <- period_birthdays %>% inner_join(ages, by = "join") %>%
  mutate(date = birthday + years(age)) %>%
  filter(date < as.Date("2019-03-31"))

placements_plus <- rbind(periods %>% dplyr::select(period_id, birthday, beginning) %>% mutate(placement = "OUT") %>% rename(report_date = birthday, ceased = beginning),
                         episodes %>% dplyr::select(period_id, report_date, ceased, placement) %>% mutate(placement = substr(placement, 1,1)),
                         periods %>% filter(event == 1) %>% dplyr::select(period_id, end) %>% rename(report_date = end) %>% mutate(ceased = as.Date("2050-03-31"), placement = "OUT"))

period_age_placements <- period_ages %>%
  inner_join(placements_plus, by = "period_id") %>%
  filter(date >= report_date & (is.na(ceased) | ceased > date))


transitions <- period_age_placements %>% filter(date < as.Date("2018-03-31")) %>% dplyr::select(period_id, age, placement) %>% 
  inner_join(period_age_placements %>% dplyr::select(period_id, age, placement), by = "period_id") %>%
  filter(age.x == age.y - 1) %>%
  mutate(next_placement = ifelse(placement.y == "OUT", "Cease", ifelse(placement.y == placement.x, "Remain", "Move"))) %>%
  group_by(placement.x, age.x, next_placement) %>%
  summarise(n = n())

transitions %>%
  filter(placement.x != "OUT") %>%
  ggplot(aes(age.x, n, fill = next_placement)) +
  geom_bar(stat = "identity",
           position = position_dodge2(preserve = "single")) +
  facet_wrap(vars(placement.x), scales = "free_y") +
  theme_mastodon +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  labs(title = "Likelihood of moving, ceasing, remaining within year, placement1")

## Sankey....

1150+1253
500 + 1253

period_birthdays <- periods %>% filter(!open) %>% dplyr::select(period_id, birthday) %>% mutate(join = "x")
ages <- data.frame(age = 0:18, join = "x")
period_ages <- period_birthdays %>% inner_join(ages, by = "join") %>%
  mutate(date = birthday + years(age))
placements_plus <- rbind(periods %>% dplyr::select(period_id, birthday, beginning) %>% mutate(placement = "IN") %>% rename(report_date = birthday, ceased = beginning),
                         episodes %>% dplyr::select(period_id, report_date, ceased, placement) %>% mutate(placement = substr(placement, 1,1)),
                         periods %>% filter(event == 1) %>% dplyr::select(period_id, end) %>% rename(report_date = end) %>% mutate(ceased = as.Date("2050-03-31"), placement = "OUT"))
period_age_placements <- period_ages %>%
  inner_join(placements_plus, by = "period_id") %>%
  filter(date >= report_date & (is.na(ceased) | ceased > date))

sankey.transitions <- period_age_placements %>%
  inner_join(period_age_placements, by = "period_id") %>%
  filter(age.x == age.y - 1) %>%
  group_by(age.y, placement.x, placement.y) %>%
  summarise(n = n()) %>%
  rename(age = age.y, placement = placement.x, next_placement = placement.y) %>%
  ungroup %>%
  filter(!(placement %in% c("IN","OUT")) |
           !(next_placement %in% c("IN", "OUT")))

sankey.placements <- function(transitions, level) {
  if (level == 1) {
    transitions %>%
      filter(age == 1) %>%
      dplyr::select(placement) %>%
      unique %>%
      arrange(placement) %>%
      mutate(id = 1:length(placement) - 1) %>%
      as.data.frame
  } else {
    rbind(transitions %>%
      filter(age == level - 1) %>%
      dplyr::select(next_placement) %>%
      rename(placement = next_placement),
      transitions %>%
        filter(age == level) %>%
        dplyr::select(placement)) %>%
      unique %>%
      arrange(placement) %>%
      mutate(id = 1:length(placement) - 1) %>%
      as.data.frame
  }
}

nodes <- data.frame(placement = c(), id = c(), level = c())
start <- sankey.placements(sankey.transitions, 1)
nodes <- cbind(start, level = 1)
for (level in 2:19) {
  new_nodes <- cbind(sankey.placements(sankey.transitions, level), level = level)
  new_nodes$id <- new_nodes$id + nrow(nodes)
  nodes <- rbind(nodes, new_nodes)
}

links <- sankey.transitions %>%
  inner_join(nodes, by = c("age" = "level", "placement" = "placement")) %>%
  inner_join(nodes %>% mutate(level = level - 1), by = c("age" = "level", "next_placement" = "placement")) %>%
  rename(source = id.x, target = id.y, value = n) %>%
  as.data.frame

print(sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                    Target = "target", Value = "value", NodeID = "placement",
                    NodeGroup = "placement",
                    units = "TWh", fontSize = 12, nodeWidth = 30,
                    sinksRight = FALSE,
                    colourScale = JS('d3.scaleOrdinal()
                    .domain(["A3","A4","A5","A6","H5","K1","K2","M2","M3","P1","P2","Q1","Q2","R1","R2","R3","R5","S1", "T1", "Z1","OUT"])
                                     .range(["#4E79A7","#A0CBE8","#F28E2B","#FFBE7D","#59A14F","#8CD17D","#B6992D","#F1CE63","#499894","#86BCB6",
                                     "#E15759","#FF9D9A","#79706E","#BAB0AC","#D37295","#FABFD2","#B07AA1","#D4A6C8","#9D7660","#D7B5A6", "#FFFFFF"])')
))

?sankeyNetwork


## Volatility

assoc.phase1.id <- function(episodes) {
  episodes <- episodes %>% arrange(period_id, report_date)
  episodes$placement1 <- substr(episodes$placement, 1,1)
  new_phases <- coalesce(episodes$period_id == lag(episodes$period_id) & episodes$placement1 != lag(episodes$placement1), FALSE)
  episodes$phase1_number <- ave(ifelse(new_phases, 1.0, 0.0), episodes$period_id, FUN = cumsum) + 1
  episodes$phase1_id <- paste0(episodes$period_id, "-", episodes$phase1_number)
  episodes
}

episodes <- assoc.phase1.id(episodes)

phase1 <- episodes %>%
  group_by(phase1_id) %>%
  summarise(birthday = birthday[1],
            placement1 = placement1[1],
            begin = min(report_date),
            end = max(ceased))
end <- max(phase1$end, na.rm = TRUE)
birthdays <- phase1[,c("phase1_id", "birthday")] %>% unique %>% mutate(join = "x")

ages <- data.frame(age = 0:18, join = "x")

grid <- birthdays %>% inner_join(ages, by = "join") %>%
  mutate(date = birthday + years(age)) %>%
  filter(date <= end) %>%
  inner_join(phase1, by = "phase1_id") %>%
  filter(begin <= date & (is.na(end) | end > date)) %>%
    mutate(yrs = year_diff(begin, date))

volatility <- grid %>%
  inner_join(episodes, by = c("phase1_id" = "phase1_id")) %>%
  filter(report_date >= date & report_date < date + years(1)) %>%
  group_by(phase1_id) %>%
  summarise(volatility = n())

grid %>%
  inner_join(volatility, by = "phase1_id") %>%
  group_by(placement1, age, yrs) %>%
  summarise(volatility = mean(volatility),
            n = n()) %>%
  filter(n > 3) %>%
  ggplot(aes(yrs, volatility)) +
  geom_bar(stat = "identity") +
  facet_wrap(vars(age)) +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  theme_mastodon +
  labs(title = "Volatility by age and years in placement1")

model.dat <- grid %>%
  inner_join(volatility, by = "phase1_id")

model.1 <- lm(volatility ~ 1 + age + yrs, data = model.dat)
summary(model.1)

model.2 <- lm(volatility ~ 1 + age + yrs + placement1, data = model.dat)
summary(model.2)

grid %>%
  inner_join(volatility, by = "phase1_id") %>%
  filter(age == 6 & yrs == 3)
