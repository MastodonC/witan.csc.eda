projected_episodes_file <- 'P:\\scc-episodes-2019-08-13-rewind-1yr-train-3yr-project-5yr-runs-100-seed-42-20201203-no-reject-sampling.csv'
projected_episodes <- read.csv(projected_episodes_file)

output_file_test <- 'P:\\output-5.pdf'

periods <- projected_episodes %>%
  group_by(Simulation, ID) %>%
  slice(1) %>%
  ungroup

periods$Period.End <- ymd(periods$Period.End)


pdf(output_file_test)
for (i in 0:17){
  print(
    periods %>%
      filter(Admission.Age == i) %>%
      mutate(sample_label = year(Period.End - months(7) - days(12)) - 2019) %>%
      filter(sample_label %in% -5:5) %>%
      # mutate(sample_label = if_else(sample_label < 0, "Historic", "Projected")) %>%
      mutate(sample_label = paste(sample_label)) %>%
      # mutate(Period.Duration = Period.Duration + rnorm(1, sd = 7)) %>%
      group_by(sample_label) %>%
      mutate(cdf = ecdf(Period.Duration)(Period.Duration)) %>%
      ungroup %>%
      ggplot(aes(Period.Duration, cdf, group = sample_label, colour = sample_label)) +
      geom_line() +
      scale_color_manual(values = tableau_color_pal("Tableau 20")(20)) +
      labs(title = (i)) +
      theme_mastodon )
  i=i+1
}
dev.off

periods %>% 
  filter(Admission.Age==7) %>%
  group_by(Simulation) %>%
  summarise(max_period=max(Period.Duration)) %>%
  ggplot(aes(max_period))+ geom_histogram()

 
