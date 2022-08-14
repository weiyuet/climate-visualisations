# Load from local-weather-data.R
source("code/local-weather-data.R")

this_year <- year(today())

local_weather %>% 
  select(date, tmax) %>% 
  mutate(year = year(date)) %>% 
  filter(year != 1891 & year != this_year) %>% 
  group_by(year) %>% 
  summarise(tmax = mean(tmax)) %>% 
  mutate(normalise_range = (year >= 1951 & year <= 1980),
         normalise_mean = sum(tmax * normalise_range)/sum(normalise_range),
         t_diff = tmax - normalise_mean) %>%
  ggplot(aes(x = year, y = t_diff)) +
  geom_line() +
  geom_smooth()

