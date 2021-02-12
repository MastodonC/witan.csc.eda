simulated_joined_at_1 <- periods %>%
  filter(Period.Start >= ymd("2019-02-01") & Admission.Age == 1)

real_joined_at_1 <- episodes %>%
  filter(Period.Start >= ymd("2019-02-01") & admission_age == 1)

simulated_joined_at_0 <- periods %>%
  filter(Period.Start >= ymd("2019-02-01") & Admission.Age == 0)

real_joined_at_0 <- episodes %>%
  filter(Period.Start >= ymd("2019-02-01") & admission_age == 0)

# need to aggregate all the people real and simulated who would be 1yr old in 2019-2020
# then look at when they joined in simulated data to ascertain where they come from

periods$Birthday <- ymd(periods$Birthday)

period_age_1_2019 <- periods %>%
  filter(Birthday >= ymd("2018-01-01") & Birthday <= ymd("2018-12-31") & Start <= max(episodes_age_1_2019$report_date))

episodes_age_1_2019 <- episodes %>%
  filter(birthday >= ymd("2018-01-01") & birthday <= ymd("2018-12-31"))

period_age_1_2019 %>% group_by(Simulation, Admission.Age) %>% summarise(n = n()) %>% group_by(Admission.Age) %>% summarise(mean(n))

episodes_age_1_2019 %>% group_by(admission_age) %>% summarise(n = n())

mean(period_age_1_2019$Period.Duration, na.rm = TRUE)

mean(episodes_age_1_2019$Period.Duration, na.rm = TRUE)

quantile(filter(period_age_1_2019, Period.Duration > 0)$Period.Duration, na.rm = TRUE)

quantile(episodes_age_1_2019$Period.Duration, na.rm = TRUE)

stayer_simulated_joined_0 <- filter(simulated_joined_at_0, (Period.Duration >= 365 | is.na(Period.Duration)) & Simulation == 1) %>% group_by(ID)

stayers_real_joined_0 <- filter(real_joined_at_0, Period.Duration >= 365 | is.na(Period.Duration)) %>% group_by(ID)

quantile(episodes$Period.Duration, na.rm = TRUE)

real_joined_at_0 <- episodes %>%
  filter(birthday >= ymd("2018-01-01") & admission_age == 0 & birthday <= ymd("2019-01-01")) %>%
  group_by(ID)

simulated_joined_at_0 <- periods %>%
  filter(Birthday >= ymd("2018-01-01") & Admission.Age == 0 & Birthday <= ymd("2019-01-01")) %>%
  group_by(ID)

stayers_real_joined_0 <- filter(real_joined_at_0, Period.Duration >= 365 | is.na(Period.Duration)) %>% group_by(ID)

stayer_simulated_joined_0 <- filter(simulated_joined_at_0, (Period.Duration >= 365 | is.na(Period.Duration)) & Simulation == 1) %>% group_by(ID)

quantile(stayers_real_joined_0$Period.Duration, na.rm = TRUE)

quantile(stayer_simulated_joined_0$Period.Duration, na.rm = TRUE)

quantile(episodes$Period.Duration, na.rm = TRUE)

stayers_real_joined_0 %>% group_by(ID, Period.Duration) %>% summarise(n = n()) %>% group_by(Period.Duration) %>% summarise(n = n())

stayer_simulated_joined_0 %>% group_by(ID, Period.Duration) %>% summarise(n = n()) %>% group_by(Period.Duration) %>% summarise(n = n())

durations_0_real <- real_joined_at_0 %>% group_by(ID, Period.Duration) %>% summarise(n = n()) %>% group_by(Period.Duration) %>% summarise(n = n())

durations_0_sim <- simulated_joined_at_0 %>% filter(Simulation == 1) %>% group_by(ID, Period.Duration) %>% summarise(n = n()) %>% group_by(Period.Duration) %>% summarise(n = n())








real_joined_at_0_2017 <- episodes %>%
  filter(birthday >= ymd("2017-01-01") & admission_age == 0 & birthday <= ymd("2018-01-01")) %>%
  group_by(ID)

durations_0_real_2017 <- real_joined_at_0_2017 %>% group_by(ID, Period.Duration) %>% summarise(n = n()) %>% group_by(Period.Duration) %>% summarise(n = n())


simulated_joined_at_0_2017 <- periods %>%
  filter(Birthday >= ymd("2017-01-01") & Admission.Age == 0 & Birthday <= ymd("2018-01-01")) %>%
  group_by(ID)

durations_0_sim_2017 <- simulated_joined_at_0_2017 %>% filter(Simulation == 1) %>% group_by(ID, Period.Duration) %>% summarise(n = n()) %>% group_by(Period.Duration) %>% summarise(n = n())

