library(readxl)
library(janitor)
library(tidyverse)
cic_data_file <- 'P:\\2021-02-12 CiC Data for Chris Feb 20 sent1.xlsx'
cic_data <- read_xlsx(cic_data_file)
cic_data <- slice(cic_data, c(3, 44:61))

colnames(cic_data)<-c("?..", "30.04.18","31.05.18", "30.06.18", "31.07.18", "31.08.18", "30.09.18", "31.10.18", "30.11.18", "31.12.18", "31.01.19", "28.02.19", "31.03.19", "30.04.19", "31.05.19", "30.06.19", "31.07.19", "31.08.19", "30.09.19", "31.10.19", "30.11.19", "31.12.19", "31.01.20", "29.02.20", "31.03.20", "30.04.20", "31.05.20", "30.06.20", "31.07.20", "31.08.20", "30.09.20", "31.10.20", "30.11.20", "31.12.20", "31.01.21")
nn<-names(cic_data)[-1]
month_period<-as.Date(nn, format = "%d.%m.%y")

cic_transpose<-as.data.frame(t(as.matrix(cic_data)))
cic_transpose <- cic_transpose %>% row_to_names(row_number = 1)
rownames(cic_transpose) <- NULL

cic_transpose <- cic_transpose %>% mutate(date = month_period) %>% rename(Total = `Total CiC exl UASC`)
cic_transpose <- cic_transpose %>%rename(`Age 0` = `Age 0 Non UASC`)
i=7
ggplot() +
  geom_line(stat = 'identity', aes(x=cic_transpose$date, y= cic_transpose$`Age toString(i)`,group=1,colour='Total Feb 20'))

  colnames(cic_transpose)








