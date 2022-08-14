# Load libraries
library(tidyverse)
library(glue)
library(lubridate)

# Load data
inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

inventory <- read_table(inventory_url,
                        col_names = c("station", "lat", "lon", "variable", "start", "end"))

# My latitude and longitude from Google Maps, in radians
# Coordinates for Singapore
my_lat <- 1.3948861111545423 * 2 * pi/360
my_lon <- 103.87667968136833 * 2 * pi/360

# Find weather station nearest to me
my_station <- inventory %>% 
  mutate(lat_r = lat * 2 * pi/360,
         lon_r = lon * 2 * pi/360,
         d = 3963 * acos((sin(lat_r) * sin(my_lat)) + cos(lat_r) * cos(my_lat) * cos(my_lon -lon_r))
         ) %>% 
  filter(start < 1960 & end > 2020) %>% 
  slice_head() %>% 
  distinct(station) %>% 
  pull(station)

station_daily <- glue("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/{my_station}.csv.gz")

local_weather <- read_csv(station_daily,
         col_names = c("station", "date", "variable", "value", "a", "b", "c", "d")) %>% 
  select(date, variable, value) %>% 
  pivot_wider(names_from = "variable", values_from = "value") %>% 
  select(date, TMAX, PRCP) %>% 
  mutate(date = ymd(date),
         TMAX = TMAX / 10,
         PRCP = PRCP / 10) %>% 
  rename_all(tolower)

# Wrangle data and data cleaning
local_weather %>% 
  ggplot(aes(x = date, y = tmax)) +
  geom_line()

local_weather %>% 
  ggplot(aes(x = tmax)) +
  geom_histogram(binwidth = 2)
