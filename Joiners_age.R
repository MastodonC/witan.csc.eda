projected_episodes_file <- 'P:\\scc-episodes-2019-08-13-rewind-1yr-train-3yr-project-5yr-runs-100-seed-42-20201203-no-reject-sampling.csv'
projected_episodes <- read.csv(projected_episodes_file)

output_file_test <- 'P:\\output-5.pdf'

periods <- projected_episodes %>%
  group_by(Simulation, ID) %>%
  slice(1) %>%
  ungroup

periods$Period.End <- ymd(periods$Period.End)
j=-5
i=0

pdf(output_file_test)
for (i in 0:17){
  for (j in -5:5){
    print(
      periods %>%
        filter(Admission.Age == i) %>%
        mutate(sample_label = year(Period.End - months(7) - days(12)) - 2019) %>%
        filter(sample_label == j) %>%
        # mutate(sample_label = if_else(sample_label < 0, "Historic", "Projected")) %>%
        mutate(sample_label = paste(sample_label)) %>%
        # mutate(Period.Duration = Period.Duration + rnorm(1, sd = 7)) %>%
        group_by(sample_label) %>%
        mutate(cdf = ecdf(Period.Duration)(Period.Duration)) %>%
        ungroup %>%
        ggplot(aes(Period.Duration, cdf, group = sample_label, colour = sample_label)) +
        theme(panel.grid.major = element_line(size = 0.1, linetype = "solid",colour = "darkgrey"))+
        theme(panel.grid.minor = element_line(size = 0.1, linetype = "solid",colour = "darkgrey"))+
        geom_vline(aes(xintercept=mean(Period.Duration),colour ="mean")) +
        geom_vline(aes(xintercept=median(Period.Duration),colour ="median")) +
        geom_line() +
        geom_text(aes(x=6000,label=toString(print(mean(Period.Duration))),y=0.45),colour="darkblue") +
        geom_text(aes(x=6000,label=toString(print(median(Period.Duration))),y=0.4),colour="orange") +
        scale_color_manual(values = tableau_color_pal("Tableau 20")(20)) +
        labs(title = (i)) +
        theme_mastodon +
        coord_cartesian(xlim=c(0,18*365),ylim=c(0,1)))
      j=j+1}
  i=i+1
}

dev.off()



 
