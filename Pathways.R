library(dplyr)
library(tidyr)
library(formattable)
projected_episodes_file <- 'P:\\scc-episodes-2019-08-13-rewind-1yr-train-3yr-project-5yr-runs-100-seed-42-20201203-no-reject-sampling.csv'
projected_episodes <- read.csv(projected_episodes_file)
project_from <- as.Date("2019-08-13")


formattable(projected_episodes %>%
              filter(Admission.Age == 5) %>%
              filter(Period.End>=as.Date("2016/01/01"))%>%
              filter(Period.End<=as.Date("2016/12/31"))%>%
              filter(Simulation==1)%>%
              filter(Episode==1)%>%
              # filter(ID==3549-1)%>%
              # filter(Placement.Pathway=="A4")%>%
              group_by(Placement.Pathway)%>%
              summarise(ID,Placement.Pathway, Admission.Age, Period.Start,Period.End, Period.Duration, mean(Period.Duration)))


formattable(projected_episodes %>%
              # filter(Admission.Age == 5) %>%
              # filter(Period.End>=as.Date("2016/01/01"))%>%
              # filter(Period.End<=as.Date("2016/12/31"))%>%
              # filter(Simulation==1)%>%
              # filter(Episode==1)%>%
              filter(ID=="3549-1")%>%
              # filter(Placement.Pathway=="A4")%>%
              group_by(Placement.Pathway)%>%
              summarise(ID,Placement.Pathway, Admission.Age, Period.Start,Period.End, Period.Duration, mean(Period.Duration)))


