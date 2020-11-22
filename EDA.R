library(tidyverse)
library(ggthemes)

theme_set(theme_fivethirtyeight)

testing <- read_csv("data/statewide_testing.csv")
cases <- read_csv("data/statewide_cases.csv")
hosp <- read_csv("data/hospitals_by_county.csv", 
                 col_types = cols(
                   county = col_character(),
                   todays_date = col_date(format = ""),
                   hospitalized_covid_confirmed_patients = col_double(),
                   hospitalized_suspected_covid_patients = col_double(),
                   hospitalized_covid_patients = col_double(),
                   all_hospital_beds = col_double(),
                   icu_covid_confirmed_patients = col_double(),
                   icu_suspected_covid_patients = col_double(),
                   icu_available_beds = col_double()
                 ))

daily_test <- testing %>% 
  mutate(
    daily_test = tested - lag(tested), 
    daily_test = ifelse(daily_test < 0, 0, daily_test)
  ) 

hosp_daily <- hosp %>% 
  group_by(date = todays_date) %>% 
  summarise(
    total_hosp = sum(hospitalized_covid_confirmed_patients, na.rm = TRUE), 
    total_sus_hosp = sum(hospitalized_suspected_covid_patients, na.rm = TRUE)
  )

testing %>% 
  ggplot(aes(date, tested)) + geom_line() + geom_point()

cases_statewide <- cases %>% 
  group_by(date) %>% 
  summarise(
    total_confirmed = sum(totalcountconfirmed), 
    total_deaths = sum(totalcountdeaths), 
    new_count_confirmed = sum(newcountconfirmed), 
    new_count_deaths = sum(newcountdeaths)
  )

hosp


cases_statewide %>% 
  ggplot(aes(date, new_count_confirmed)) + geom_line()


cases_and_test <- cases_statewide %>% 
  left_join(daily_test) %>% 
  left_join(hosp_daily)



cases_and_test %>% 
  ggplot(aes(daily_test, new_count_confirmed)) + geom_point() + 
  scale_x_log10()


cases_and_test %>% 
  ggplot(aes(date, daily_test)) + geom_point() + scale_y_log10(labels = scales::comma)


cases_and_test %>% 
  mutate(month = factor(month.abb[month(date)], levels =month.abb)) %>% 
  filter(month %in% month.abb[6:12], daily_test < 3000000) %>% 
  ggplot(aes(daily_test, new_count_confirmed, color = month)) + geom_point() + 
  scale_x_log10(labels = scales::comma) + 
  geom_smooth(aes(color = month), method = "lm", se = F)


cases_and_test %>% 
  ggplot(aes(new_count_confirmed, total_hosp)) + geom_point() + geom_smooth(method = "lm")

cases_and_test_since_jul <- 
  cases_and_test %>% 
  mutate(month = factor(month.abb[month(date)], levels =month.abb)) %>% 
  filter(month %in% month.abb[6:12], daily_test < 3000000) %>% 
  mutate(log_test = log10(daily_test)) %>% 
  filter(!is.nan(log_test))
  

m <- lm(new_count_confirmed ~ log_test, data = cases_and_test_since_jul)


