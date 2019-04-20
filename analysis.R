library(tidyverse)
library(lubridate)
x <- c("ID", 
       "trip_start", 
       "trip_end", 
       "trip_dur", 
       "trip_len", 
       "pickup_tract", 
       "dropoff_tract", 
       "pickup_CA", 
       "dropoff_CA",
       "fare",
       "tip",
       "add_charge",
       "total_cost",
       "share_auth",
       "pooled",
       "pickup_lat",
       "pickup_lon",
       "pickup_loc",
       "dropoff_lat",
       "dropoff_lon",
       "dropoff_loc")
data <- read_csv("data/Transportation_Network_Providers_-_Trips.csv", col_names = x, skip = 1)

data2 <- sample_n(data, size = 1000000)

#fix format for time columns
data$trip_start <- mdy_hms(data$trip_start)
data$trip_end <- mdy_hms(data$trip_end)

#trips by hour of the day
data %>%
  mutate(trip_hour = hour(trip_start)) %>%
    arrange(trip_hour) %>%
      ggplot(aes(trip_hour)) +
      geom_histogram(bins = 24) +
      scale_y_continuous(name="Number of trips", labels = scales::comma) + # fix scientific notation on y scale
      xlab("Trip start time") +
      ggtitle("Number of trips by time of day")

data %>% 
  filter(wday(trip_start) != 1 & wday(trip_start) != 7) %>%
  mutate(trip_hour = hour(trip_start)) %>%
  filter(trip_hour == 0)

data %>% 
  filter(wday(trip_start) != 1 & wday(trip_start) != 7) %>%
  mutate(trip_hour = hour(trip_start)) %>%
           ggplot(aes(trip_hour)) +
           geom_histogram(bins = 24) +
           scale_y_continuous(name="Number of trips", labels = scales::comma) + # fix scientific notation on y scale
           xlab("Trip start time") +
  ggtitle("Number of trips by time of day (weekday only)")

data %>% 
  filter(wday(trip_start) == 1 | wday(trip_start) == 7) %>%
  mutate(trip_hour = hour(trip_start)) %>%
  ggplot(aes(trip_hour)) +
  geom_histogram(bins = 24) +
  scale_y_continuous(name="Number of trips", labels = scales::comma) + # fix scientific notation on y scale
  xlab("Trip start time") +
  ggtitle("Trips by time of day (weekend only)")
                          
  
  

wday(data2$trip_start[1])

data %>%
  mutate(trip_dist = ifelse(trip_len > 30, 30, trip_len)) %>%
  ggplot(aes(trip_dist)) +
  geom_histogram(bins = 30) +
  scale_y_continuous(name="Number of trips", labels = scales::comma) + # fix scientific notation on y scale
  xlab("Trip distance (miles)")


