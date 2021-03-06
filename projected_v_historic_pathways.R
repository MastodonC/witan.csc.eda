library(data.table)
library(gt)


projected_episodes_historic <- projected_episodes%>%
  filter(Provenance=='H')

projected_episodes_projected<- projected_episodes%>%
  filter(Provenance=='P'|Provenance=='S')

historic_table<-formattable(projected_episodes_historic  %>%
              filter(Admission.Age == 5) %>%
              # filter(Period.End>=as.Date("2016/01/01"))%>%
              # filter(Period.End<=as.Date("2016/12/31"))%>%
              filter(Simulation==1)%>%
              filter(Episode==1)%>%
              # filter(ID==3549-1)%>%
              # filter(Placement.Pathway=="A4")%>%
              group_by(Placement.Pathway)%>%
              summarise(number_in_pathway=n(), mean(Period.Duration),median(Period.Duration),unique(Provenance),unique(Admission.Age)))
arrange(historic_table,desc(historic_table$number_in_pathway))

projected_table<-formattable(projected_episodes_projected %>%
              filter(Admission.Age == 5) %>%
              #filter(Period.End>=as.Date("2016/01/01"))%>%
              #filter(Period.End<=as.Date("2016/12/31"))%>%
              #filter(Simulation==1)%>%
              filter(Episode==1)%>%
              # filter(ID==3549-1)%>%
              # filter(Placement.Pathway=="A4")%>%
              group_by(Placement.Pathway)%>%
              summarise( number_in_pathway=n(), mean(Period.Duration),median(Period.Duration), unique(Admission.Age)))
arrange(projected_table,desc(projected_table$number_in_pathway))

