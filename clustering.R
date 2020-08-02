library(dplyr)
library(lubridate)
library(ggplot2)
library(fitdistrplus)
library(reshape)

library(cluster)
library(NbClust)
library(factoextra)

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
    group_by(ID) %>%
    mutate(admission_age = year_diff(birthday, min(report_date)),
           beginning = min(report_date),
           end = max(ceased)) %>%
    mutate(placement_duration = day_diff(report_date, ceased)) %>%
    mutate(leaving_age = year_diff_frac(birthday, ceased)) %>%
    mutate(is_leaver = !is.na(max(ceased)) & max(ceased) == ceased) %>%
    mutate(event = ifelse(is.na(max(ceased)), 0, 1)) %>%
    as.data.frame
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
  assoc.phase.id

ncc <- read.csv("../witan.cic/data/ncc/2020-06-09/episodes.scrubbed.csv", na.strings = "") %>%
  add.derived.cols %>%
  assoc.phase.id

ccc <- read.csv("../witan.cic/data/ccc/2020-06-09/episodes.scrubbed.csv", na.strings = "NA") %>%
  add.derived.cols %>%
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
  # The maximum feature vectors for one child is 12 * 18 = 216 months
  # We start 1 month after their first report date, and keep going until their last cease date
  # We include their age in days
  # Their time in care in days
  # Their total time in each placement in days
  # Their total time in each ordered placement in sequence in days
  
  feature_episodes <- closed_episodes %>%
    mutate(join = TRUE) %>%
    inner_join(data.frame(month_offset = seq(1,216),
                          join = TRUE)) %>%
    mutate(feature_date = beginning + months(month_offset)) %>%
    filter(report_date < feature_date & feature_date < end) %>%
    mutate(episode_days = ifelse(feature_date >= report_date & feature_date <= ceased,
                                 day_diff(report_date, feature_date),
                                 day_diff(report_date, ceased)))
  
  features_1 <- feature_episodes %>%
    group_by(ID, month_offset) %>%
    summarise(age = day_diff(birthday[1], feature_date[1]),
              care_days = day_diff(beginning[1], feature_date[1])) %>%
    reshape2::melt(id.vars = c("ID", "month_offset"))
  
  features_2 <- feature_episodes %>%
    group_by(ID, month_offset, placement) %>%
    summarise(value = sum(episode_days)) %>%
    dplyr::rename(variable = placement)
    
  # features_3 <- feature_episodes %>%
  #   mutate(placement_seq = paste0(placement, phase_number)) %>%
  #   group_by(ID, month_offset, placement_seq) %>%
  #   summarise(value = sum(episode_days)) %>%
  #   dplyr::rename(variable = placement_seq)

  features <- rbind(features_1,
        features_2
        # features_3
        ) %>%
    dcast(ID + month_offset ~ variable, value.var = 'value', fill = 0)
  
  ids <- (features[,1:2] %>% mutate(name = paste0(ID, '-', month_offset)))$name
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
    group_by(ID) %>%
    summarise(age = day_diff(birthday[1], feature_date),
              care_days = day_diff(beginning[1], feature_date)) %>%
    melt(id.vars = c("ID"))
  
  features_2 <- feature_episodes %>%
    group_by(ID, placement) %>%
    summarise(value = sum(episode_days)) %>%
    dplyr::rename(variable = placement)
  
  # features_3 <- feature_episodes %>%
  #   mutate(placement_seq = paste0(placement, phase_number)) %>%
  #   group_by(ID, placement_seq) %>%
  #   summarise(value = sum(episode_days)) %>%
  #   dplyr::rename(variable = placement_seq)
  
  features <- rbind(features_1,
                    features_2
                    # features_3
                    ) %>%
    dcast(ID ~ variable, value.var = 'value', fill = 0)
  
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

scc.closed.features <- learner.features.closed(scc %>% filter(!is.na(end)))
scc.open.features <- learner.features.open(scc %>% filter(is.na(end)))
scc.closed.features <- add.zero.features(scc.closed.features, scc.open.features)
scc.open.features <- add.zero.features(scc.open.features, scc.closed.features)

M <- as.matrix(scc.closed.features)
X <- as.matrix(scc.open.features[1:10,])
res <- apply(X, 1, function(v) {
  sim <- ( M %*% v ) / sqrt( sum(v*v) * rowSums(M*M) )
  sub("\\-.*", "", names(sim[which.max(sim),]))
  names(sim[which.max(sim),])
})

scc.open.features[rownames(scc.open.features) == 1124,]
scc.closed.features[rownames(scc.closed.features) == "1352-23" ,]
