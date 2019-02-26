library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

housing <- read_csv("datathon_propattributes.csv") %>%
  mutate(transaction_date = mdy(transaction_date)) %>%
  mutate(day = day(transaction_date), 
         month = month(transaction_date), 
         year = year(transaction_date)) %>%
  mutate(year = ifelse(year <= 2018, year, year - 100 ),
         price_category = cut(sale_amt,
                              c(0,
                                1000,
                                5000, 
                                10000, 
                                50000, 
                                100000,
                                200000, 
                                300000,
                                600000, 
                                1000000,
                                10000000),
                              dig.lab=10)) %>%
  arrange(desc(year))

annoying %>% filter(prop_city == "PHILADELPHIA") %>% view()

us <- map_data("state")
annoying <- read_csv("annoying_final.csv")

x <- housing  %>% filter(dwelling_type == "Single Family Residential") %>%
  select(prop_city, 
         prop_state, 
         apn, 
         geocode_latitude,
         geocode_longitude) %>%
  filter(!is.na(prop_city), 
         !is.na(prop_state),
         !is.na(geocode_longitude),
         !is.na(geocode_latitude), !is.na(apn)) %>% 
  group_by(prop_city, 
           prop_state,apn, 
           geocode_latitude, 
           geocode_longitude) %>%
  summarize(n = n()) %>% arrange(desc(n))


f %>% view()
ggplot() +
  geom_polygon(data = filter(us, region %in% c("massachusetts", 
                                               "rhode island", 
                                               "pennsylvania", 
                                               "new york", 
                                               "conneticut",
                                               "new hampshire",
                                               "new jersey",
                                               "vermont")), 
               aes(long, lat, group = group), 
               fill = "white", colour = "black") +
  geom_point(data = f %>% head(61) %>%
               filter(prop_state %in% c("PA")) %>%  
               mutate(traded_times = cut(n, 
                                         c(10,
                                            50, 100))),
             aes(geocode_longitude, 
                 geocode_latitude, color = traded_times, alpha = 0.3, size = n)) +
  coord_quickmap() +
  ylim(39, 42.5) +
  xlim(-78, -74) + 
  labs(title = "The houses that have been traded more than 30 times",
       subtitle = "For the state of Pennsylvania")

f %>% head(61) %>%
  filter(prop_state %in% c("PA")) %>%  
  mutate(traded_times = cut(n, 
                            c(10,
                              50, 100))) %>% filter(n >= 10) %>% nrow() 
  


