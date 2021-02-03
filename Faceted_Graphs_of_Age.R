projected_episodes_file <- 'P:\\scc-episodes-2019-08-13-rewind-1yr-train-3yr-project-5yr-runs-100-seed-42-20201203-no-reject-sampling.csv'
projected_episodes <- read.csv(projected_episodes_file)

periods <- projected_episodes %>%
  group_by(Simulation, ID) %>%
  slice(1) %>%
  ungroup

periods$Period.End <- ymd(periods$Period.End)

i=mean(periods$Admission.Age)

df<-data.frame(periods$Admission.Age,periods$Period.Duration)

df1<-aggregate(.~periods$Admission.Age,data = ,mean)
i=max(df1$periods.Period.Duration)
i
df2<-aggregate(.~periods$Admission.Age,data = df,median)
j=max(df2$periods.Period.Duration)
j

chart_data <- periods %>%
  mutate(sample_label = year(Period.End - months(7) - days(12)) - 2019) %>%
  filter(sample_label %in% -5:4) %>%
  # mutate(sample_label = if_else(sample_label < 0, "Historic", "Projected")) %>%
  mutate(sample_label = factor(paste(sample_label), levels = as.character(-5:4))) %>%
  # mutate(Period.Duration = Period.Duration + rnorm(1, sd = 7)) %>%
  group_by(sample_label, Admission.Age) %>%
  mutate(cdf = ecdf(Period.Duration)(Period.Duration)) %>%
  ungroup

pdf(output_file_test)

ggplot(chart_data, aes(Period.Duration, cdf, group = sample_label, colour = sample_label)) +
  facet_grid(sample_label~Admission.Age,scales = 'free')+
  theme(panel.grid.major = element_line(size = 0.1, linetype = "solid",colour = "darkgrey"))+
  theme(panel.grid.minor = element_line(size = 0.1, linetype = "solid",colour = "darkgrey"))+
  geom_vline(data = chart_data %>% group_by(sample_label, Admission.Age) %>% summarise(intercept = mean(Period.Duration)),
             aes(xintercept=intercept,colour ="mean")) +
  geom_vline(data = chart_data %>% group_by(sample_label, Admission.Age) %>% summarise(intercept = median(Period.Duration)),
             aes(xintercept=intercept,colour ="median")) +
  geom_line() +
  scale_color_manual(values = tableau_color_pal("Tableau 20")(20)) +
  labs(title = "CDF for joiners") +
  theme_mastodon 

dev.off() 
