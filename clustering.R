library(dplyr)
library(lubridate)
library(ggplot2)
library(fitdistrplus)
library(reshape2)

library(cluster)
library(NbClust)
library(factoextra)
library(stringi)
library(tidyverse)

library(ggthemes)


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
    mutate(ID = as.character(ID)) %>%
    mutate(report_date = ymd(report_date), ceased = ymd(ceased)) %>%
    mutate(birthday = as.Date(paste0(DOB, "-01"))) %>%
    group_by(period_id) %>%
    mutate(admission_age = year_diff(birthday, min(report_date)),
           beginning = min(report_date),
           end = max(ceased)) %>%
    mutate(placement_duration = day_diff(report_date, ceased)) %>%
    mutate(leaving_age = year_diff_frac(birthday, ceased)) %>%
    mutate(is_leaver = !is.na(max(ceased)) & max(ceased) == ceased) %>%
    mutate(event = ifelse(is.na(max(ceased)), 0, 1)) %>%
    as.data.frame
}

assoc.period.id <- function(episodes) {
  episodes <- episodes %>% arrange(ID, report_date)
  new_periods <- coalesce(episodes$ID == lag(episodes$ID) & episodes$report_date > lag(episodes$ceased), FALSE)
  episodes$period_id <- paste0(episodes$ID, "-", ave(ifelse(new_periods, 1.0, 0.0), episodes$ID, FUN = cumsum) + 1)
  episodes
}

assoc.phase.id <- function(episodes) {
  episodes <- episodes %>% arrange(period_id, report_date)
  new_phases <- coalesce(episodes$period_id == lag(episodes$period_id) & episodes$placement != lag(episodes$placement), FALSE)
  episodes$phase_number <- ave(ifelse(new_phases, 1.0, 0.0), episodes$period_id, FUN = cumsum) + 1
  episodes$phase_id <- paste0(episodes$period_id, "-", episodes$phase_number)
  episodes
}

scc <- read.csv("../witan.cic/data/scc/2020-07-16/episodes.scrubbed.csv", na.strings = "") %>%
  add.derived.cols %>%
  assoc.period.id %>%
  assoc.phase.id

ncc <- read.csv("../witan.cic/data/ncc/2020-06-09/episodes.scrubbed.csv", na.strings = "") %>%
  add.derived.cols %>%
  assoc.period.id %>%
  assoc.phase.id

ccc <- read.csv("../witan.cic/data/ccc/2020-06-09/episodes.scrubbed.csv", na.strings = "NA") %>%
  add.derived.cols %>%
  assoc.period.id %>%
  assoc.phase.id



learner.features <- function(episodes) {
  placement_counts <- episodes %>%
    group_by(ID, placement) %>%
    summarise(duration = sum(placement_duration)) %>%
    as.data.frame
  placement_seq_counts <- episodes %>%
    group_by(ID, phase_number) %>%
    mutate(placement = paste0(placement, ".", phase_number)) %>%
    group_by(ID, placement) %>%
    summarise(duration = sum(placement_duration)) %>%
    as.data.frame
  features <- rbind(placement_counts, placement_seq_counts) %>%
      as.data.frame %>%
      cast(ID ~ placement, fill = 0)
  names <- features$ID
  features <- features[-1]
  rownames(features) <- names
  features
}

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


scc.features <- learner.features(scc)
ncc.features <- learner.features(ncc)
ccc.features <- learner.features(ccc)

scc.dist <- cosine.dist(scc.features)
ncc.dist <- cosine.dist(ncc.features)
ccc.dist <- cosine.dist(ccc.features)

fviz_nbclust(scc.features, FUN = hcut, method = "silhouette", diss = scc.dist, k.max = 25)
fviz_nbclust(ncc.features, FUN = hcut, method = "silhouette", diss = ncc.dist, k.max = 25)
fviz_nbclust(ccc.features, FUN = hcut, method = "silhouette", diss = ccc.dist, k.max = 25)

fviz_nbclust(scc.features, FUN = hcut, method = "wss", diss = scc.dist, k.max = 25)
fviz_nbclust(ncc.features, FUN = hcut, method = "wss", diss = ncc.dist, k.max = 25)
fviz_nbclust(ccc.features, FUN = hcut, method = "wss", diss = ccc.dist, k.max = 25)


scc.features.closed <- learner.features(scc %>% filter(event == 1))
ncc.features.closed <- learner.features(ncc %>% filter(event == 1))
ccc.features.closed <- learner.features(ccc %>% filter(event == 1))

scc.dist.closed <- cosine.dist(scc.features.closed)
ncc.dist.closed <- cosine.dist(ncc.features.closed)
ccc.dist.closed <- cosine.dist(ccc.features.closed)

fviz_nbclust(scc.features.closed, FUN = hcut, method = "silhouette", diss = scc.dist.closed, k.max = 25)
fviz_nbclust(ncc.features.closed, FUN = hcut, method = "silhouette", diss = ncc.dist.closed, k.max = 25)
fviz_nbclust(ccc.features.closed, FUN = hcut, method = "silhouette", diss = ccc.dist.closed, k.max = 25)

plot.clusters <- function(episodes, features, distance, n.clusters) {
  hc <- hclust(distance, method = "ward.D2")
  clusters <- data.frame(ID = rownames(features), cluster = cutree(hc, k = n.clusters))
  ggplot(episodes %>% inner_join(clusters), aes(placement_duration, fill = placement)) +
    geom_histogram(bins = 50) +
    facet_grid(vars(admission_age), vars(cluster), scales = "free_y") +
    scale_fill_manual(values = tableau_color_pal("Tableau 20")(21))
}

plot.clusters(scc %>% filter(event == 1), scc.features.closed, scc.dist.closed, 7)
plot.clusters(ncc %>% filter(event == 1), ncc.features.closed, ncc.dist.closed, 6)
plot.clusters(ccc %>% filter(event == 1), ccc.features.closed, ccc.dist.closed, 4)

library(class)

cluster.all <- function(features, features.closed, dist.closed, n.clusters) {
  new_cols <- setdiff(colnames(features), colnames(features.closed))
  features.closed[new_cols] <- 0
  features.closed <- features.closed[, colnames(features)]
  labels = cutree(hclust(dist.closed, method = "ward.D2"), k = n.clusters)
  clustering <- knn(train = features.closed, test = features, k = 1, cl = labels)
  clusters <- data.frame(ID = rownames(features), cluster = clustering)
}

setdiff(1:5, 3:9)

loadfonts(device = "pdf")

theme_mastodon <- theme(# plot.title = element_text(family = "OpenSans-Bold", hjust = 0.5, size = 20,
  #                          margin = margin(0,0,15,0)),
  # axis.title = element_text(family = "OpenSans-SemiBold", hjust = 0.5, size = 16),
  # axis.text = element_text(family = "OpenSans-Regular", hjust = 0.5, size = 10),
  axis.text.x = element_text(angle = -45),
  axis.title.x = element_text(margin = margin(15,0,0,0)),
  axis.title.y = element_text(margin = margin(0,10,0,0)),
  plot.margin = margin(10,20,10,10),
  panel.background = element_blank(),
  panel.grid = element_line(color = "#eeeeee"))



plot_pdf <- function(scc, clusters, name) {
  max_date <- max(scc$report_date)
  scc.durations <- scc %>% group_by(ID) %>%
    summarise(event = ifelse(is.na(max(ceased)), 0, 1),
              duration = day_diff(min(report_date), coalesce(max(ceased), max_date)),
              phases = n_distinct(phase_id)) %>%
    inner_join(clusters)
  week_offsets <- data.frame(offset = seq(0, 18 * 52),
                             join = TRUE)
  scc.weeks <- scc %>%
    inner_join(clusters) %>%
    mutate(join = TRUE) %>%
    group_by(ID) %>%
    mutate(beginning = min(report_date)) %>%
    inner_join(week_offsets) %>%
    mutate(week = beginning + weeks(offset),
           age = year_diff(birthday, week)) %>%
    filter(report_date <= week & (is.na(ceased) | ceased > week)) %>%
    filter(age < 18 | week <= max_date)
  
  all.placements <- c("A3", "A4", "A5", "A6", "H5", "K1", "K2", "M2", "M3", "P1", "P2",
                      "Q1","Q2", "R1", "R2", "R3", "R5", "S1", "T0", "T4", "Z1", "OUT")
  
  # Define the number of colors you want
  nb.cols <- 21
  my.colours <- colorRampPalette(tableau_color_pal("Tableau 20")(11))(nb.cols)
  my.colours <- c(my.colours, "#888888")
  names(my.colours) <- all.placements
  
  pdf(paste0("data/cluster-tapestry-", name ,".pdf"))
  for (cluster.id in 1:length(unique(scc.weeks$cluster))) {
    for (age.id in 0:17) {
      dat <- scc.weeks %>% filter(admission_age == age.id & cluster == cluster.id) %>%
        mutate(open = ifelse(event == 0, "Y", "N"))
      alphas <- c(1.0, 0.7)
      ids <- dat %>% group_by(ID) %>% summarise(max_offset = max(offset)) %>% arrange(max_offset)
      dat$ID <- factor(dat$ID, levels = ids$ID)
      print(ggplot(dat, aes(offset, ID, fill = placement, alpha = open)) + geom_tile(color = NA) +
              scale_fill_manual(values = my.colours) +
              scale_alpha_discrete(range = alphas) +
              theme_mastodon +
              theme(axis.title.y=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks.y=element_blank()) +
              labs(title = paste("Cluster:", cluster.id, "Age:", age.id), x = "Offset", fill = "Placement",
                   alpha = "Open?"))
    }
  }
  dev.off()
}

clusters <- cluster.all(scc.features, scc.features.closed, scc.dist.closed, 7)

read.csv("../witan.cic/data/scc/2020-07-16/episodes.scrubbed.csv", na.strings = "") %>%
  mutate(ID = as.character(ID)) %>%
  inner_join(clusters) %>%
  write.csv("../witan.cic/data/scc/2020-07-16/episodes.scrubbed.clustered.csv", na = "")

plot_pdf(scc, clusters, "scc")


clusters <- cluster.all(ncc.features, ncc.features.closed, ncc.dist.closed, 6)

read.csv("../witan.cic/data/ncc/2020-06-09/episodes.scrubbed.csv", na.strings = "") %>%
  mutate(ID = as.character(ID)) %>%
  inner_join(clusters) %>%
  write.csv("../witan.cic/data/ncc/2020-06-09/episodes.scrubbed.clustered.csv", na = "")

plot_pdf(ncc, clusters, "ncc")

clusters <- cluster.all(ccc.features, ccc.features.closed, ccc.dist.closed, 4)

read.csv("../witan.cic/data/ccc/2020-06-09/episodes.scrubbed.csv", na.strings = "") %>%
  mutate(ID = as.character(ID)) %>%
  inner_join(clusters) %>%
  write.csv("../witan.cic/data/ccc/2020-06-09/episodes.scrubbed.clustered.csv", na = "")

plot_pdf(ccc, clusters, "ccc")



# Use KNN to attempt to close open cases

# Let's assume we can simply store learner age in days

#


library(reshape2)
closed_episodes <- scc %>% filter(!is.na(end))
learner.features.closed <- function(closed_episodes) {
  # We want to create a feature vector for each month of a closed case
  # We express the offset in days using an interval of 28
  # We start 1 month after their first report date, and keep going until their last cease date
  # We also add a feature at the beginning of each placement
  # We include their age in days
  # Their time in care in days
  # Their total time in each placement in days
  # Their total time in each ordered placement in sequence in days
  
  feature_episodes <- closed_episodes %>%
    group_by(period_id) %>% mutate(period_duration = day_diff(min(report_date), coalesce(max(ceased), max_date))) %>% ungroup %>%
    mutate(join = TRUE) %>%
    inner_join(data.frame(day_offset = seq(7, 18 * 365, by = 28),
                          join = TRUE)) %>%
    mutate(feature_date = beginning + days(day_offset)) %>%
    filter(report_date < feature_date & feature_date < end) %>%
    mutate(episode_days = ifelse(feature_date >= report_date & feature_date <= ceased,
                                 day_diff(report_date, feature_date),
                                 day_diff(report_date, ceased)))
  
  features_1 <- feature_episodes %>%
    group_by(period_id, day_offset) %>%
    arrange(report_date) %>%
    summarise(age = day_diff(birthday[1], feature_date[1]),
              entry = day_diff(birthday[1], beginning[1]),
              care_days = day_diff(beginning[1], feature_date[1]),
              current = last(placement),
              period_duration = last(period_duration)) %>%
    reshape2::melt(id.vars = c("period_id", "day_offset"))
  
  features_2 <- feature_episodes %>%
    group_by(period_id, day_offset, placement) %>%
    summarise(value = sum(episode_days)) %>%
    dplyr::rename(variable = placement)
    
  features_3 <- feature_episodes %>%
    mutate(placement_seq = paste0(placement, phase_number)) %>%
    group_by(period_id, day_offset, placement_seq) %>%
    summarise(value = sum(episode_days)) %>%
    dplyr::rename(variable = placement_seq)

  features <- rbind(features_1,
       features_2,
        features_3
        ) %>%
    dcast(period_id + day_offset ~ variable, value.var = 'value', fill = 0)
  
  ids <- (features[,1:2] %>% mutate(name = paste0(period_id, ':', day_offset)))$name
  features <- features[c(-2,-1)]
  rownames(features) <- ids
  features
}

open_episodes <- scc %>% filter(is.na(end))
learner.features.open <- function(open_episodes) {
  feature_date <- max(max(open_episodes$report_date), max(open_episodes$ceased, na.rm = TRUE))
  feature_episodes <- open_episodes %>%
    mutate(episode_days = day_diff(report_date, coalesce(ceased, feature_date)))
  
  features_1 <- feature_episodes %>%
    group_by(period_id) %>%
    summarise(age = day_diff(birthday[1], feature_date),
              entry = day_diff(birthday[1], beginning[1]),
              care_days = day_diff(beginning[1], feature_date),
              current = last(placement)) %>%
    melt(id.vars = c("period_id"))
  
  features_2 <- feature_episodes %>%
    group_by(period_id, placement) %>%
    summarise(value = sum(episode_days)) %>%
    dplyr::rename(variable = placement)
  
  features_3 <- feature_episodes %>%
    mutate(placement_seq = paste0(placement, phase_number)) %>%
    group_by(period_id, placement_seq) %>%
    summarise(value = sum(episode_days)) %>%
    dplyr::rename(variable = placement_seq)
  
  features <- rbind(features_1,
                    features_2,
                    features_3
                    ) %>%
    dcast(period_id ~ variable, value.var = 'value', fill = 0)
  
  ids <- features[[1]]
  features <- features[c(-1)]
  rownames(features) <- ids
  features[is.na(features)] <- 0
  features
}

add.zero.features <- function(target, reference) {
  all_cols <- sort(union(colnames(reference), colnames(target)))
  new_cols <- setdiff(all_cols, colnames(target))
  target[new_cols] <- 0
  target <- target[, all_cols]
  target
}

is.nan.data.frame <- function(x)do.call(cbind, lapply(x, is.nan))

normalise_cols <- function(df, denominator) {
  res <- as.data.frame(sweep(df, 2, sapply(denominator, max, na.rm = TRUE), FUN = "/"))
  res[is.nan(res)] <- 0
  res
}

cluster_cases <- function(scc) {
  scc.closed.features <- learner.features.closed(scc %>% filter(!is.na(end)))
  scc.open.features <- learner.features.open(scc %>% filter(is.na(end)))
  scc.closed.features <- add.zero.features(scc.closed.features, scc.open.features)
  scc.open.features <- add.zero.features(scc.open.features, scc.closed.features)
  # scc.closed.features <- normalise_cols(scc.closed.features, rbind(scc.closed.features, scc.open.features))
  # scc.open.features <- normalise_cols(scc.open.features, rbind(scc.closed.features, scc.open.features))
  
  stddev <- apply(rbind(scc.closed.features, scc.open.features), 2, function(x) sd(as.numeric(x)))
  means <- apply(rbind(scc.closed.features, scc.open.features), 2, function(x) mean(as.numeric(x)))
  
  M <- scc.closed.features[,c("current", "period_duration", "care_days", "entry")]
  X <- scc.open.features[1:nrow(scc.open.features),c("current", "care_days", "entry")]
  X$entry <- (as.numeric(X$entry) - means["entry"]) / stddev["entry"]
  X$care_days <- (as.numeric(X$care_days) - means["care_days"]) / stddev["care_days"]
  M$entry <- (as.numeric(M$entry) - means["entry"]) / stddev["entry"]
  M$care_days <- (as.numeric(M$care_days) - means["care_days"]) / stddev["care_days"]
  
  res <- apply(X, 1, function(v) {
    placement <- v[["current"]]
    print(v)
    vxx <- as.numeric(v[!v == placement])
    vx <- vxx
    Mxx <- M[M$current == placement & M$period_duration > v["care_days"], !(names(M) %in% c("current", "period_duration"))]
    Mx <- Mxx
    Mx[] <- lapply(Mxx, as.numeric)
    Mx <- as.matrix(Mx)
    # sim <- ( Mx %*% vx ) / sqrt( sum(vx*vx) * rowSums(Mx*Mx) )
    # names(sim[which.max(sim),])
    d <- rowSums((Mx - vx) ^ 2)
    names(d[which.min(d)])
  })
  resmat <- as.matrix(res)
  clusters <- data.frame(closed = resmat, open = rownames(resmat)) %>%
    mutate(offset =  as.numeric(sub(".*:", "", closed)),
           closed = sub(":.*", "", closed))
}

scc_clusters <- cluster_cases(scc)
write.csv(scc_clusters, "scc-knn-closed-cases.csv", row.names = FALSE)

ncc_clusters <- cluster_cases(ncc)
write.csv(ncc_clusters, "ncc-knn-closed-cases.csv", row.names = FALSE)

ccc_clusters <- cluster_cases(ccc)
write.csv(ccc_clusters, "ccc-knn-closed-cases.csv", row.names = FALSE)

max_date <- max(ccc$report_date)

plot_distribution <- function(df, clusters) {
  period_durations <- df %>%
    group_by(period_id, event) %>%
    dplyr::summarise(period_duration = day_diff(min(report_date), coalesce(max(ceased), max_date))) %>%
    as.data.frame
  
  
  cluster_durations <- clusters %>%
    inner_join(period_durations, by = c("closed" = "period_id")) %>%
    rename(matched_closed_duration = period_duration) %>%
    inner_join(period_durations, by = c("open" = "period_id")) %>%
    rename(open_duration = period_duration) %>%
    mutate(inferred_eventual_duration = open_duration + (matched_closed_duration - offset))
  
  melt(cluster_durations[,c("matched_closed_duration", "inferred_eventual_duration")]) %>%
    rbind(period_durations %>% filter(event == 1) %>% mutate(variable = "closed_case_distribution") %>% dplyr::select(variable, period_duration) %>% rename(value = period_duration)) %>%
    ggplot(aes(value, fill = variable)) +
    geom_histogram(position = "dodge", alpha = 0.2) +
    labs(title = "KNN(1) on age, duration in care and current placement only")
}

plot_distribution(scc, scc_clusters) + labs(title = "SCC distribution")
plot_distribution(ncc, ncc_clusters) + labs(title = "NCC distribution")
plot_distribution(ccc, ccc_clusters) + labs(title = "CCC distribution")

clusters_long <- clusters %>%
  sample_n(25) %>%
  mutate(cluster = row_number()) %>%
  melt(id.vars = c("cluster", "offset")) %>%
  dplyr::rename(case_type = variable, period_id = value) %>%
  mutate(period_id = as.character(period_id))

day_offsets <- seq(0, 365 * 18, by = 7)

nb.cols <- 21

all.placements <- c("A3", "A4", "A5", "A6", "H5", "K1", "K2", "M2", "M3", "P1", "P2",
                    "Q1","Q2", "R1", "R2", "R3", "R5", "S1", "T0", "T4", "Z1", "OUT")
my.colours <- colorRampPalette(tableau_color_pal("Tableau 20")(11))(nb.cols)
my.colours <- c(my.colours, "#888888")
names(my.colours) <- all.placements

scc %>%
  inner_join(clusters_long) %>%
  mutate(join = TRUE) %>%
  inner_join(data.frame(day_offset = day_offsets, join = TRUE)) %>%
  filter(birthday + days(day_offset) >= report_date & (birthday + days(day_offset) <= coalesce(ceased, max_date))) %>%
  ggplot(aes(day_offset, ID, fill = placement)) + geom_tile() +
  facet_grid(cols = vars(case_type), rows = vars(cluster), scales = "free_y") +
  scale_fill_manual(values = my.colours) +
  geom_vline(aes(xintercept = day_diff(birthday, beginning + as.integer(offset))), linetype = 2) +
  labs(x = "Day offset", y = "ID", title = "Random sample of 1-nearest neighbours")


# Investigate open cases

Can't close open case 1008-1, ignoring
Can't close open case 2570-1, ignoring
Can't close open case 3370-1, ignoring
Can't close open case 1011-1, ignoring
Can't close open case 3371-1, ignoring
Can't close open case 2574-1, ignoring
Can't close open case 2572-1, ignoring
Can't close open case 3125-1, ignoring


scc %>% filter(period_id == "1008-1")

clusters %>% filter(open == "1008-1")

scc %>% filter(period_id == "437-1")
