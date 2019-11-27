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

episodes2periods <- function(episodes) {
  latest_cease <- max(episodes[!is.na(episodes$ceased),]$ceased)
  periods <- episodes %>% group_by(period_id, DOB) %>%
    summarise(duration = as.integer(dplyr::coalesce(max(ceased), latest_cease) - min(report_date)),
              open = is.na(max(ceased)),
              first_placement = placement[1],
              beginning = min(report_date),
              end = max(ceased)) %>%
    mutate(admission_age = as.character(year(beginning) - DOB),
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

periods <- episodes2periods(episodes)
periods$admission_age = factor(periods$admission_age)

## Look for policy changes

## We'll create a new summary dataset which includes report_year, DOB, and phase before and after
max_transition_year <- year(max(episodes$report_date)) - 1 # Ignore most recent unfinished year
placement.transitions <- episodes %>% group_by(period_id, phase_number, placement) %>%
  summarise(DOB = min(DOB), beginning = min(report_date), end = max(ceased), CIN = care_status[1], legal_status = legal_status[1]) %>%
  as.data.frame %>% arrange(period_id, phase_number) %>%
  group_by(period_id) %>% mutate(next_placement = lead(placement)) %>%
  filter(!is.na(next_placement) & year(end) <= max_transition_year) %>%
  mutate(transition_year = year(end))

placement.transitions.grouped <- placement.transitions %>% mutate(admission_age = transition_year - DOB) %>%
  group_by(transition_year, admission_age, placement, next_placement, CIN, legal_status) %>%
  summarise(n = n())

min_year <- min(placement.transitions.grouped$transition_year)
max_year <- max(placement.transitions.grouped$transition_year) - 1

for (year in max_year:min_year) {
  print(paste("Testing year ", year))
  tab <- xtabs(n ~ transition_year + placement + next_placement + admission_age, placement.transitions.grouped %>%
                 filter(transition_year >= year))
  m1 <- loglm(~ transition_year + placement * next_placement * admission_age, tab)
  m2 <- loglm(~ placement * next_placement * admission_age, tab)
  res <- anova(m2, m1)
  if (res[[14]] < 0.05) {
    print(paste("Significant difference detected at year", year))
    print(paste("Consider episodes input range from", year + 1))
    mosaicplot(tab, color = TRUE, shade = TRUE)
    break;
  }
  if (year == min_year) {
    mosaicplot(tab, color = TRUE, shade = TRUE)
  }
}

## Estimate trend in arrivals by age

joiner.projection <- function(diffs, from) {
  to <- as.Date('2023-03-31')
  grid<- expand.grid(admission_age = factor(as.character(0:17), levels = as.character(0:17)), beginning = seq(from,to,'weeks'))
  joiners.model <- glm2(diff ~ beginning * admission_age, data = diffs %>% filter(beginning >= from), family=Gamma(link = log))
  family <- family(joiners.model)
  ilink <- family$linkinv
  grid <- bind_cols(grid, setNames(as_tibble(predict(joiners.model, grid, se.fit = TRUE)[1:2]), c('fit_link','se_link'))) %>%
    mutate(projection  = ilink(fit_link), upper_ci = ilink(fit_link + (1.96 * se_link)), lower_ci = ilink(fit_link - (1.96 * se_link)))
  grid
}

diffs <- periods %>%
  arrange(admission_age, beginning) %>%
  group_by(admission_age) %>%
  mutate(diff = interval(lag(beginning), beginning) / days(1), n = n()) %>%
  ungroup %>%
  filter(!is.na(diff) & n >= 3) %>% # We need at least 3 data points for each age to generate 2 diffs
  dplyr::select(admission_age, diff, beginning) %>%
  mutate(diff = diff + 0.01) %>% # Diff must always be greater than zero
  as.data.frame

grid3 <- joiner.projection(diffs, as.Date("2015-03-31"))
grid4 <- joiner.projection(diffs, as.Date("2014-03-31"))
grid.all <- rbind(cbind(grid3, input = "3 years"), cbind(grid4, input = "4 years"))

ggplot(grid.all, aes(x = beginning, y = projection, color = admission_age)) +
  geom_line(aes(linetype = input)) +
  facet_wrap(vars(admission_age), scale = "free") +
  scale_color_manual(values = tableau_color_pal("Tableau 20")(20), guide = "none") +
  labs(x = "Date", y = "Inter-arrival time (days)", title = "Projected mean inter-arrival time by age of entry (3 & 4 years historic data)",
       linetype = "Input history")

ggplot(grid.all, aes(x = beginning, y = projection)) +
  geom_line(aes(linetype = input)) +
  geom_ribbon(aes(x = beginning, ymin = lower_ci, ymax = upper_ci, color = NA, fill = input), alpha = 0.25) +
  facet_wrap(vars(admission_age), scale = "free") +
  coord_cartesian(ylim = c(0, 200)) +
  scale_color_manual(values = tableau_color_pal("Tableau 20")(20), guide = "none") +
  labs(x = "Date", y = "Inter-arrival time (days)", title = "Projected mean inter-arrival time by age of entry (3 & 4 years historic data)",
       linetype = "Input history", fill = "Input history")

## NHpoisson

perday <- diffs %>% group_by(beginning, admission_age) %>% summarise(perday = n())
perday <- diffs %>% inner_join(perday, by = c("admission_age", "beginning")) %>% filter(beginning > ymd("2010-03-31"))
perday$tiebreak <- as.POSIXct(perday$beginning + hours(ave(ifelse(perday$perday == 1, 0, as.integer(24 / (perday$perday + 1))), perday$admission_age, perday$beginning, FUN = cumsum)))
index_start <- perday %>% filter(beginning > ymd("2010-03-31")) %>% group_by(admission_age) %>% summarise(start = min(beginning))
perday <- perday %>% inner_join(index_start, by = "admission_age") %>% mutate(index = interval(start, tiebreak) / hours(1))

for (age in 0:17) {
  indices <- (perday %>% filter(admission_age == age))$index
  max_index <- max(indices)
  all.hours<-seq(1,max_index)
  fitPP.fun(tind=TRUE,covariates=cbind(all.hours),posE= indices, n = max_index,start=list(b0=0,b1=0), modSim = TRUE,
            tit = paste("Poisson rate for age of admission", age))
}

test.admission.age <- function(age) {
  indices <- (perday %>% filter(admission_age == age))$index
  max_index <- max(indices)
  all.hours<-seq(1,max_index)
  aux<-fitPP.fun(tind=TRUE,covariates=cbind(all.hours),posE= indices, n = max_index,start=list(b0=0,b1=0), modSim = TRUE)
  
  posEHB <- transfH.fun(aux)$posEH
  resB <- unifres.fun(posEHB)
  graphresU.fun(unires = resB$unires, posE = aux@posE,
                Xvariables = cbind(1.0, all.hours),
                addlow = FALSE)
  
  ResDB <- CalcResD.fun(mlePP = aux, lint = 1000)
  qqnorm(ResDB$RawRes)
  graphrate.fun(ResDB)
  
  res <- globalval.fun(mlePP=aux,lint=1000,resqqplot=FALSE,Xvar = cbind(1.0, 1.0, all.hours))
}

min_year <- 2010
max_year <- year(max(diffs$beginning)) - 1


for (age in as.character(0:17)) {
  # print(paste("Testing age", age))
  for (year in max_year:min_year) {
    # print(paste("Testing year ", year))
    pop <- diffs %>% filter(admission_age == age & year(beginning) >= year)
    sample <- diffs %>% filter(admission_age == age & year(beginning) == year)
    res <- ks.test(sample$diff, pop$diff)
    if (res$p.value < 0.05) {
      print(paste("Significant difference detected at year", year, "for age", age))
      print(paste("*** Consider episodes input range from", year + 1, "for age", age, "***"))
      break
    }
    if (year == min_year) {
      print(paste("No significant differences observed for age", age))
    }
  }
}


## Estimate underlying bimodal survival curves from censored data

  result <- 0
  n.times <- 1000
  d <- 0.5
  for (i in 1:n.times){
    data <- sample_n(periods, nrow(periods), replace = TRUE)
    data <- data %>% mutate(birthday = date_after(if_else(year(beginning) == DOB, beginning, ymd(paste0(DOB, "-01-01")))))  %>%
      mutate(AOA = floor(interval(birthday, beginning) / years(1)),
             duration_yrs = duration / 365.0)
    data$AOA <- factor(data$AOA)
    fit <- survfit(Surv(duration_yrs, event) ~ AOA, data = data)
    long.quantile <- melt(quantile(fit, probs = seq(0,1,length.out = 1001))$quantile) %>%
      mutate(AOA = as.integer(str_replace(Var1, "AOA=", "")),
             age = value + as.integer(str_replace(Var1, "AOA=", "")))
    long.cdf <- long.quantile %>%
      group_by(AOA, year = floor(value / d) * d) %>%
      summarise(value = Var2[1]) %>%
      mutate(age = AOA + year) %>%
      as.data.frame
    long.cdf <- expand.grid(AOA = 0:17, age = seq(0, 18, by = d)) %>% left_join(long.cdf, by = c("AOA", "age"))
    wide.cdf <- dcast(AOA ~ age, data = long.cdf, drop = FALSE)
    wide.cdf <- cbind(wide.cdf, 100)
    rownames(wide.cdf) <- wide.cdf[,1]
    wide.cdf <- wide.cdf[,-1]
    wide.cdf.imputed <- t(na.approx(t(wide.cdf)))
    wide.cdf.imputed[is.na(wide.cdf.imputed)] <- 0
    wide.pdf <- wide.cdf.imputed - cbind(0,wide.cdf.imputed[,1:ncol(wide.cdf.imputed)-1])
    result <- result + wide.pdf
  }
  result <- result / n.times
  colnames(result) <- seq(0,(18 + d),by=d)
  long.pdf <- melt(result) %>% filter(value > 0) %>% mutate(Var1 = factor(Var1), Var2 = factor(Var2))
  mVar2 <- max(as.integer(long.pdf$Var2))
  breaks <- seq(1, mVar2, length.out = mVar2 * d)
  age.labels <- paste("Admission age:", seq(0,17))
  names(age.labels) <- seq(0,17)
  ggplot(long.pdf, aes(Var2, value)) +
    geom_bar(stat = "identity", fill = tableau_color_pal("Tableau 20")(1)) +
    scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
    scale_x_discrete(breaks = as.character(seq(0,18, by = 2))) +
    scale_y_continuous(breaks = seq(0,40, by = 10), labels = paste0(seq(0,40, by = 10), "%")) +
    theme(axis.text.x = element_text(angle = -45, hjust = 0, size = 7),
          axis.text.y = element_text(size = 7)) +
    facet_wrap(vars(Var1), ncol = 3, labeller = labeller(Var1 = age.labels)) +
    labs(y = "Proportion exiting", x = "Age at exit", title = "Distribution of exit ages by admission age") +
    theme_mastodon

ggplot(periods, aes(as.integer(as.character(admission_age)), duration / 365)) +
  geom_point(position = "jitter", color = tableau_color_pal("Tableau 20")(1), alpha = 0.5) +
  stat_density2d(color = tableau_color_pal("Tableau 20")(3)[3], size = 1) +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  labs(y = "Years in care", x = "Age at exit", title = "Comparing age at exit with years in care") +
  theme_mastodon

## For comparison - from raw data without survival analysis

to_nearest <- function(x, y) {
  round(x / y) * y
}

periods %>%
  filter(event == 1, as.integer(as.character(admission_age)) < 18) %>%
  mutate(duration_yrs = duration / 365.0, admission_age = factor(admission_age, levels = 0:18)) %>%
  mutate(exit_age = to_nearest(as.integer(as.character(admission_age)) + duration_yrs, 0.5)) %>%
  group_by(admission_age, exit_age) %>%
  summarise(n = n()) %>%
  mutate(p = n / sum(n) * 100.0) %>%
  ggplot(aes(exit_age, p)) +
  geom_bar(stat = "identity", fill = tableau_color_pal("Tableau 20")(1)) +
  scale_x_continuous(lim = c(0,18), breaks = seq(0,18, by = 2)) +
  scale_y_continuous(breaks = seq(0,30, by = 10), labels = paste0(seq(0,30, by = 10), "%")) +
  facet_wrap(vars(admission_age),ncol = 3, labeller = labeller(admission_age = age.labels)) +
  labs(y = "Years in care", x = "Age at exit", title = "Distribution of exit ages by admission age (unadjusted)") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0, size = 7),
        axis.text.y = element_text(size = 7)) +
  theme_mastodon

## Sankey charts

sankey.transitions <- episodes %>%
  dplyr::select(period_id, phase_number, placement) %>%
  filter(period_id %in% (periods %>% filter(!is.na(end)))$period_id) %>%
  unique %>%
  arrange(period_id, phase_number) %>%
  group_by(period_id) %>%
  mutate(next_placement = coalesce(lead(placement),"[OUT]")) %>%
  group_by(phase_number, placement, next_placement) %>%
  summarise(n = n()) %>%
  ungroup

sankey.placements <- function(transitions, level) {
  if (level == 1) {
    transitions %>%
      filter(phase_number == 1) %>%
      dplyr::select(placement) %>%
      unique %>%
      arrange(placement) %>%
      mutate(id = 1:length(placement) - 1) %>%
      as.data.frame
  } else {
    transitions %>%
      filter(phase_number == level - 1) %>%
      dplyr::select(next_placement) %>%
      unique %>%
      arrange(next_placement) %>%
      mutate(id = 1:length(next_placement) - 1) %>%
      rename(placement = next_placement) %>%
      as.data.frame
  }
}

nodes <- data.frame(placement = c(), id = c(), level = c())
start <- sankey.placements(sankey.transitions, 1)
nodes <- cbind(start, level = 1)
for (level in 2:3) {
  new_nodes <- cbind(sankey.placements(sankey.transitions, level), level = level)
  new_nodes$id <- new_nodes$id + nrow(nodes)
  nodes <- rbind(nodes, new_nodes)
}

links <- sankey.transitions %>%
  inner_join(nodes, by = c("phase_number" = "level", "placement" = "placement")) %>%
  inner_join(nodes %>% mutate(level = level - 1), by = c("phase_number" = "level", "next_placement" = "placement")) %>%
  rename(source = id.x, target = id.y, value = n) %>%
  as.data.frame

print(sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                    Target = "target", Value = "value", NodeID = "placement",
                    units = "TWh", fontSize = 12, nodeWidth = 30,
                    sinksRight = FALSE))

## Individual historic sequences

placement.offsets <- episodes %>%
  group_by(period_id) %>%
  mutate(start = as.double(report_date - min(report_date)),
         end = as.double(ceased - min(report_date)) - 1,
         open = is.na(max(ceased))) %>%
  filter(!open) %>%
  dplyr::select(period_id, start, end, placement) %>%
  as.data.frame

periods %>% arrange(duration) %>% filter(duration > 365)

offsets <- data.frame(offset = as.integer(seq(0,6000, by = 28)))

setDT(placement.offsets)
setDT(offsets)

results <- placement.offsets[offsets, on = .(start <= offset, end >= offset), nomatch = 0, allow.cartesian=TRUE,
                  .(period_id, placement, offset)]
setDF(results)
results$offset <- factor(results$offset, levels = offsets$offset)
results$placement <- factor(results$placement, levels = sort(unique(results$placement)))

my.colours <- tableau_color_pal("Tableau 20")(length(levels(results$placement)))
names(my.colours) <- levels(results$placement)

gtyears <- 2
candidates <- (periods %>% filter(event == 1) %>% arrange(duration) %>% filter(duration > 365*gtyears))$period_id[1:500]
ggplot(results %>% filter(period_id %in% candidates), aes(offset, factor(period_id, levels = rev(candidates)))) +
  geom_tile(aes(fill = placement)) +
  scale_fill_manual(values = my.colours) +
  scale_x_discrete(breaks = seq(0,6000, by = 28 * 13), labels = as.character(0:16), drop = FALSE) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray", size = 0.2),
        panel.background = element_blank()) +
  labs(fill = "Placement", y = "Placement sequence", x = "Years in care",
       title = paste("Children admitted aged 0-3 in care for > 3 years")) +
  theme_mastodon


## Area charts & total CiC
min_date <- min(episodes$ceased, na.rm = TRUE)
max_date <- max(episodes$ceased, na.rm = TRUE)

dates <- data.table(date = seq(min_date, max_date, by = 7))
episodes_table <- as.data.table(episodes %>% mutate(ceased = ifelse(is.na(ceased), as.Date("2050-01-01"), episodes$ceased)))
results <- episodes_table[dates, on = .(report_date <= date, ceased > date), nomatch = 0, allow.cartesian=TRUE,
                             .(date, care_status, legal_status, placement)]
setDF(results)

results %>% group_by(date) %>% summarise(n = n()) %>%
  ggplot(aes(date, n)) +
  geom_line(color = tableau_color_pal("Tableau 20")(1), size = 0.6) +
  scale_y_continuous(limits = c(0,800)) +
  labs(title = "CiC - total count", x = "Date", y = "CiC") +
  theme_mastodon

ggplot(results, aes(date, fill = care_status)) +
  geom_area(stat = "count") +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  labs(title = "CiC grouped by care status - proportion of total") +
  theme_mastodon

ggplot(results, aes(date, fill = care_status)) +
  geom_area(stat = "count", position = "fill") +
  scale_y_continuous(breaks = seq(0,1,by=0.2), labels = paste(seq(0,100,by=20),"%")) +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  labs(title = "CiC grouped by care status - proportion of total",
       x = "Date", y = "Proportion", fill = "Care status") +
  theme_mastodon

ggplot(results, aes(date, fill = legal_status)) +
  geom_area(stat = "count", position = "fill") +
  scale_y_continuous(breaks = seq(0,1,by=0.2), labels = paste(seq(0,100,by=20),"%")) +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  labs(title = "CiC grouped by legal status - proportion of total",
       x = "Date", y = "Proportion", fill = "Legal status") +
  theme_mastodon

ggplot(results, aes(date, fill = placement)) +
  geom_area(stat = "count", position = "fill") +
  scale_y_continuous(breaks = seq(0,1,by=0.2), labels = paste(seq(0,100,by=20),"%")) +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  labs(title = "CiC grouped by placement - proportion of total",
       x = "Date", y = "Proportion", fill = "Placement") +
  theme_mastodon
