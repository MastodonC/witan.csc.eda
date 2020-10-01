library(dplyr)
library(lubridate)
library(ggplot2)
library(fitdistrplus)
library(reshape)

day_diff <- function(start, stop) {
  as.numeric(difftime(stop, start, units = "days"))
}

year_diff <- function(start, stop) {
  day_diff(start, stop) %/% 365.25
}

year_diff_frac <- function(start, stop) {
  day_diff(start, stop) / 365.25
}

add.derived.cols <- function(episodes) {
  episodes %>%
  mutate(report_date = ymd(report_date), ceased = ymd(ceased)) %>%
  mutate(birthday = as.Date(paste0(DOB, "-01"))) %>%
  group_by(ID) %>%
  mutate(admission_age = year_diff(birthday, min(report_date))) %>%
  mutate(placement_duration = day_diff(report_date, ceased)) %>%
  mutate(leaving_age = year_diff_frac(birthday, ceased)) %>%
  mutate(is_leaver = !is.na(max(ceased)) & max(ceased) == ceased) %>%
  mutate(event = ifelse(is.na(ceased), 0, 1)) %>%
  as.data.frame
}


scc <- read.csv("../witan.cic/data/scc/2020-07-16/episodes.scrubbed.csv", na.strings = "") %>%
  add.derived.cols

ncc <- read.csv("../witan.cic/data/ncc/2020-06-09/episodes.scrubbed.csv", na.strings = "") %>%
  add.derived.cols

ccc <- read.csv("../witan.cic/data/ccc/2020-06-09/episodes.scrubbed.csv", na.strings = "NA") %>%
  add.derived.cols

scc[,c("report_date", "ceased", "placement_duration")]


ggplot(scc, aes(placement_duration)) + geom_histogram(bins = 50) + facet_wrap(vars(placement), scales = "free_y")
ggplot(ncc, aes(placement_duration)) + geom_histogram(bins = 50) + facet_wrap(vars(placement), scales = "free_y")
ggplot(ccc, aes(placement_duration)) + geom_histogram(bins = 50) + facet_wrap(vars(placement), scales = "free_y")


summary(aov(placement_duration ~ placement, data = scc %>% filter(!is.na(placement_duration))))
summary(aov(placement_duration ~ placement, data = ncc %>% filter(!is.na(placement_duration))))
summary(aov(placement_duration ~ placement, data = ccc %>% filter(!is.na(placement_duration))))

## Beta dist ... i.e. overdispersed, even for Q2
descdist(scc$placement_duration[!is.na(scc$placement_duration) & scc$placement == "Q2"])
descdist(scc$placement_duration[!is.na(scc$placement_duration) & scc$placement == "Q2" & scc$admission_age == 0])
descdist(log(scc$placement_duration[!is.na(scc$placement_duration) & scc$placement == "Q2" & scc$admission_age == 0]))
descdist(scc$placement_duration[!is.na(scc$placement_duration) & scc$placement == "Q2" & scc$admission_age == 1])
descdist(log(scc$placement_duration[!is.na(scc$placement_duration) & scc$placement == "Q2" & scc$admission_age == 1]))

ggplot(scc, aes(placement_duration)) + geom_histogram(bins = 50) + facet_wrap(vars(admission_age), scales = "free_y")

ggplot(scc %>% filter(placement == "Q2"), aes(placement_duration)) + geom_histogram(bins = 50) + facet_wrap(vars(admission_age), scales = "free_y")
ggplot(scc %>% filter(placement == "Q1"), aes(placement_duration)) + geom_histogram(bins = 50) + facet_wrap(vars(admission_age), scales = "free_y")
ggplot(scc %>% filter(placement == "K2"), aes(placement_duration)) + geom_histogram(bins = 50) + facet_wrap(vars(admission_age), scales = "free_y")


descdist(scc$placement_duration[!is.na(scc$placement_duration) & scc$placement == "Q2" & scc$admission_age == 0], discrete = TRUE)
fit <- fitdist(scc$placement_duration[!is.na(scc$placement_duration) & scc$placement == "Q2" & scc$admission_age == 0], distr = "nbinom")
plot(fit)

descdist(scc$placement_duration[!is.na(scc$placement_duration) & scc$placement == "Q2" & scc$admission_age == 0 & scc$placement_duration < 2000], discrete = TRUE)
fit <- fitdist(scc$placement_duration[!is.na(scc$placement_duration) & scc$placement == "Q2" & scc$admission_age == 0 & scc$placement_duration < 2000], distr = "nbinom")
plot(fit)

descdist(scc$placement_duration[!is.na(scc$placement_duration) & scc$placement == "Q2" & scc$admission_age == 1], discrete = TRUE)
fit <- fitdist(scc$placement_duration[!is.na(scc$placement_duration) & scc$placement == "Q2" & scc$admission_age == 1], distr = "nbinom")
plot(fit)

ggplot(scc, aes(leaving_age, fill = is_leaver)) + geom_histogram(bins = 50) + facet_wrap(vars(placement))


## NCC

ggplot(ncc, aes(leaving_age, fill = is_leaver)) + geom_histogram(bins = 50) + facet_wrap(vars(placement))


## Leaver probability

install.packages("epiR")
library(epiR)
library(survival)

fit <- survfit(Surv(placement_duration, event) ~ admission_age, data = scc)
plot(fit)
haz0 <- epi.insthaz(fit) %>% filter(strata == 0)
ggplot(haz0, aes(time, est, color = strata)) + geom_line() + facet_wrap(vars(strata), scales = "free_y")
loess.haz0 <- data.frame(
  time = lowess(haz0$time, haz0$lower, f = 0.20)$x,
  est =  lowess(haz0$time, haz0$est, f = 0.20)$y,
  low =  lowess(haz0$time, haz0$lower, f = 0.20)$y,
  upp =  lowess(haz0$time, haz0$upper, f = 0.20)$y)

ggplot(loess.haz0, aes(time, est)) + geom_line()

?epi.insthaz

km <- survfit(Surv(placement_duration, event)~ admission_age, data=scc %>% filter(admission_age == 0))
survest <- stepfun(km$time, c(1, km$surv))
ggplot(data.frame(x = 0:6000, y = 1 - survest(0:6000)), aes(x, y)) + geom_line()

ggplot(data.frame(surv = km$surv), aes(surv)) + geom_histogram()
density(km$surv)

## Clustering


placements <- scc %>%
  group_by(ID) %>%
  mutate(placement_seq = row_number()) %>%
  group_by(ID, placement) %>%
  summarise(duration = sum(placement_duration)) %>%
  as.data.frame

placement_seqs <- scc %>%
  group_by(ID) %>%
  mutate(placement = paste0(placement, row_number())) %>%
  group_by(ID, placement) %>%
  summarise(duration = sum(placement_duration)) %>%
  as.data.frame

features <- rbind(placements, placement_seqs) %>%
  as.data.frame %>%
  cast(ID ~ placement, fill = 0)

cosine.dist <- function(df) {
  mat <- as.matrix(df)
  sim <- mat / sqrt(rowSums(mat * mat))
  sim <- sim %*% t(sim)
  sim[sim > 1.0] <- 1.0
  sim[sim < 0.0] <- 0.0
  dist <- (2 * acos(sim)) / pi
  dist[is.na(dist)] <- 1.0
  as.dist(dist)
}



library(cluster)
install.packages("factoextra")
library(NbClust)
library(factoextra)

ids <- features$ID
feats <- features[,2:ncol(features)]
rownames(feats) <- ids
dist <- cosine.dist(feats)



hc <- hclust(dist, method = "ward.D2")
# ag <- agnes(dist, diss = TRUE, method = "ward")

fviz_nbclust(features, FUN = hcut, method = "silhouette", diss = dist)

# 
# nbclust <- NbClust(distance = NULL, diss = dist, method = "ward.D2", index = "silhouette")
# fviz_nbclust(nbclust, method = "silhouette")

k <- 8
sub_grp <- cutree(hc, k = k)
feature_clusters8 <- feats %>%
  mutate(cluster = sub_grp)

scc_clusters <- scc %>%
  inner_join(data.frame(ID = ids, cluster3 = cutree(hc, 3))) %>%
  inner_join(data.frame(ID = ids, cluster8 = cutree(hc, 8)))

library(ggthemes)

ggplot(scc_clusters, aes(placement_duration, fill = placement)) + geom_histogram(bins = 50) + facet_grid(vars(admission_age), vars(cluster3), scales = "free_y") +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(21))

ggplot(scc_clusters, aes(placement_duration, fill = placement)) + geom_histogram(bins = 50) + facet_grid(vars(admission_age), vars(cluster8), scales = "free_y") +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(21))

ggplot(scc_clusters %>% group_by(ID, admission_age, cluster3) %>% summarise(duration = sum(placement_duration)), aes(duration)) + geom_histogram(bins = 50) + facet_grid(vars(admission_age), vars(cluster3), scales = "free_y") +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(21))

ggplot(scc_clusters %>% group_by(ID, admission_age, cluster8) %>% summarise(duration = sum(placement_duration)), aes(duration)) + geom_histogram(bins = 50) + facet_grid(vars(admission_age), vars(cluster8), scales = "free_y") +
  scale_fill_manual(values = tableau_color_pal("Tableau 20")(21))

scc_clusters %>%
  group_by(ID, cluster8) %>%
  summarise(n = n_distinct(phase_id)) %>%
  mutate(cluster = factor(cluster8)) %>%
  ggplot(aes(n, fill = cluster)) + geom_histogram(bins = 20) + scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)) +
  facet_grid(rows = vars(cluster), scales = "free_y")

grouped8 <- scc_clusters %>%
  group_by(ID, cluster8) %>%
  summarise(n = n_distinct(phase_id)) %>%
  mutate(cluster = factor(cluster8))

scc_clusters <- scc_clusters %>%
  group_by(ID)%>%
  mutate(placement_seq = row_number()) %>%
  as.data.frame

pdf("data/clusterplacementdurations3.pdf")
for (mycluster in 1:8) {
  for (myplacement in 1:3) {
    print(ggplot(scc_clusters %>% filter(cluster8 == mycluster & placement_seq == myplacement), aes(placement_duration, fill = placement)) +
      geom_histogram(bins = 50) +
      facet_wrap(vars(admission_age)) +
      labs(title = paste("Cluster", mycluster, "placement", myplacement)) +
      scale_fill_manual(values = tableau_color_pal("Tableau 20")(20)))
  }
}
dev.off()

getmode <- function(v) {
  uniqv <- unique(v)
  print(uniqv)
  print(match(v, uniqv))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



getmodal <- function(v, i = 1) {
  uniqv <- unique(v)
  print(uniqv)
  print(match(v, uniqv))
  print(tabulate(match(v, uniqv)))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

?tabulate
tabulate(c(1,2,3,4,3,3,2,3,4,5,5,6,8,12))

frequency(c("a"))

frequency(c('a','b','c','b','b', 'c'))


scc_clusters %>%
  group_by(cluster8, ID) %>%
  summarise(admission_age = admission_age[1],
            duration = day_diff(min(report_date), max(ceased, na.rm = TRUE)),
            placement_sequence = paste0(placement, collapse = "-")) %>%
  summarise(median_admission_age = median(admission_age),
            median_duration = median(duration)) %>%
  as.data.frame

scc_clusters %>%
  group_by(cluster8, ID) %>%
  summarise(placement_sequence = paste0(placement, collapse = "-")) %>%
  group_by(cluster8, placement_sequence) %>%
  summarise(n = n()) %>%
  top_n(2) %>%
  as.data.frame

write.csv(scc_clusters, "data/scc_clusters.csv")

paste0(1:4, collapse = '-')

descdist(grouped8[grouped8$cluster == 8,]$n, discrete = TRUE)

scc_clusters %>% filter(cluster8 == 8)

for (i in 1:k) {
  print(plotActivities(a2.empower %>% filter(user_id %in% rownames(a2.sample[a2.sample.clusters$cluster == i,]))) +
          ggtitle(paste("A2: count of completed activities, cluster", i)))
}


