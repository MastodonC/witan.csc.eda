install.packages("tidyverse")

library(tidyverse)

i="3549-1"
df <- periods%>%
  filter(ID==i)%>%
  group_by(Placement.Pathway)
pdf(output_file_test)
ggplot(df, aes(reorder(Simulation,Period.Duration), Period.Duration))+
          geom_bar(stat='identity',width = 0.05)+
          facet_wrap(~Placement.Pathway,scale = "free_x")+
          labs(x= "Simulations",title = i)

dev.off()
