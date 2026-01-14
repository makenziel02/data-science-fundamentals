library(mdsr)
library(tidyverse)
library(datasets)
library(nycflights13)

#2
View(LakeHuron)
LakeHuron <- data.frame(LakeHuron)
ggplot(data=LakeHuron) +
  geom_histogram(mapping = aes(x=LakeHuron), binwidth=1)

#3
View(datasets::HairEyeColor)
HairEyeColor <- data.frame(HairEyeColor)
HEC_wider <- pivot_wider(data=HairEyeColor, 
            names_from = c(Hair, Eye),
            values_from = Freq) |> rowwise() |> mutate(overall = sum())

#correct answer 
hec <- data.frame(HairEyeColor)
ggplot(hec, aes(weight = Freq)) +
  geom_bar(mapping=aes(x=Hair)) +
  facet_wrap(~Eye, nrow=1)


#4
View(flights)
View(planes)
View(airlines)

test <- inner_join(flights, airlines, by=c("carrier" = "carrier"))
test2 <- inner_join(test, planes, by=c("tailnum")) 

test2 |> select(name, flight, air_time, distance, manufacturer, model) |>
  mutate(air_time_hours = air_time/60) |> mutate(mph = distance/air_time_hours) |>
  select(name, flight, mph, manufacturer, model) |>
  arrange(desc(mph)) |>
  head(5)


#5
View(airports)
flights |>
mutate(weekday=weekdays(time_hour)) |>
  filter(weekday == "Saturday") |>
  filter(arr_time - sched_arr_time > 30) |>
  select(dest, weekday, arr_time, sched_arr_time) |>
  inner_join(airports, by = c("dest" = "faa")) |>
  select(name, dest, weekday, arr_time, sched_arr_time)

#correct answer
SaturdayFlights <- flights |>
  mutate(dow=wday(time_hour)) |>
  filter(dow==7)

SaturdayDest <- SaturdayFlights |>
  group_by(dest) |>
  summarize(n=n())

SaturdayLate <- SaturdayFlights |>
  filter(arr_delay >30) |>
  group_by(dest) |>
  summarize(lateN=n())

SaturdayPctLate <- inner_join(SaturdayDest, SaturdayLate, by=c("dest")) |>
  mutate(latePct = lateN/n) |>
  arrange(desc(latePct)) |>
  left_join(airports, by = c("dest" = "faa")) 

answer <- SaturdayPctLate$name[1]

