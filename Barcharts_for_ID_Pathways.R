install.packages("tidyverse")

library(tidyverse)

i="2526-1"
df <- periods%>%
  filter(ID==i)

pdf(output_file_test)
ggplot(df, aes(reorder(Simulation,Period.Duration), Period.Duration))+
          geom_bar(stat='identity',position = position_dodge(width = 1),width=0.1)+
          theme(axis.text.x=element_blank(),
                text=element_text(size = 15)) +
          facet_wrap(~Placement.Pathway,shrink=FALSE,scales="free_x")+
          labs(x= "Simulations",y='Period Duration', title = i)

 dev.off()

