install.packages("tidyverse")

library(tidyverse)

i="3549-1"
df <- projected_episodes%>%
  filter(ID==i)
output_file_test <- 'P:\\output-5.pdf'
pdf(output_file_test)
ggplot(df, aes(reorder(Simulation,Period.Duration), Period.Duration))+
  geom_bar(stat='identity',aes(fill=Placement),width=0.5)+
  theme(axis.text.x=element_blank(),
        text=element_text(size = 15),
        strip.text.x = element_text(size = 8, angle = 90)) +
  scale_fill_manual(values=my.colours)+
  facet_grid(~Placement.Pathway,scales='free_x',space='free_x')+
  labs(x= "Simulations",y='Period Duration', title = i)
 dev.off()
 
 library(ggthemes)
 all.placements <- c("A3", "A4", "A5", "A6", "H5", "K1", "K2", "M2", "M3", "P1", "P2",
                     "Q1","Q2", "R1", "R2", "R3", "R5", "S1", "T0", "T4", "Z1", "OUT")
 # Define the number of colors you want
 nb.cols <- 21
 my.colours <- colorRampPalette(tableau_color_pal("Tableau 20")(11))(nb.cols)
 my.colours <- c(my.colours, "#888888")
 names(my.colours) <- all.placements
