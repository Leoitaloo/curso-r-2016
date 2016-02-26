install.packages("nycflights13")
install.packages("tidyr")
library(nycflights13)
library(dplyr)
library(tidyr)

flights %>% tbl_df

#Filter

flights %>% 
  filter(month==1, year==2013)

flights %>% 
  filter(month==1|month==2, year==2013)

flights %>% 
  filter(distance>1000)

#Select

flights %>% 
  select(dep_delay, month)

flights %>% 
  select(atraso=dep_delay, mes=month)

flights %>% 
  select(-c(tailnum, origin, dest))

#Mutate

flights %>% 
  mutate(ganho_de_tempo = dep_delay-arr_delay, velocidade = distance / air_time * 60) %>% 
  select(ganho_de_tempo, velocidade)

flights %>% 
  mutate(hour2 = (air_time+hour*60+minute)%/%60, minute2 = ((air_time+hour*60+minute)%%60)) %>% 
  select(hour2, minute2)

#Summarise

flights %>% 
  summarise(dist_mean = mean(distance))

flights %>%
  group_by(month) %>% 
  summarise(dist_mean = mean(distance))

flights %>% 
  group_by(month) %>% 
  summarise(statistics = mean(air_time, na.rm = T))

#Arrange

flights %>% 
  arrange(dep_delay)

flights %>% 
  arrange(desc(dep_delay))

#Spread

flights %>%
  group_by(month, day) %>% 
  summarise(dep_delay_month = mean(dep_delay, na.rm = TRUE)) %>% 
  spread(month, dep_delay_month)

flights %>%
  group_by(day, hour) %>% 
  summarise(dep_delay_day = mean(dep_delay, na.rm = TRUE)) %>% 
  spread(day, dep_delay_day)
  
#Gather

flights %>%
  group_by(month, day) %>% 
  summarise(dep_delay_month = mean(dep_delay, na.rm = TRUE)) %>% 
  gather(day, dep_delay_month)
  
  
  

  

