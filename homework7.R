library(mdsr)
library(tidyverse)
library(nycflights13)
library(datasets)
library(lubridate)


#1A
View(flights)
flights |>
  filter(origin=="JFK", !is.na(dep_time)) |> #include only rows that have origin==JFK & have a number for departure time
  group_by(month, day) |> #apply the function to the month and day columns
  summarize(N=n()) |> #count up the number of departure times
  arrange(desc(N)) #show in descending order 

#1B
flights |>
  filter(origin=="JFK", !is.na(dep_time)) |> ##include only rows that have origin==JFK & have a number for departure time
  mutate(weekday=weekdays(time_hour)) |> #create new column that shows the day of week for departures
  group_by(month, day, weekday) |> 
  summarize(N=n()) |> #tally up the number of flights for each month, day and weekday
  group_by(weekday) |> 
  summarize(Cnt=n(), ave=mean(N)) |> #count up then take the average of the flights on each weekday
  arrange(desc(ave)) #put into descending order 

#2
View(airports)
flights %>%
  inner_join(airports, by=c("dest" = "faa")) |> #create a table that combines airports and flights based on destination code
  ggplot() + geom_bar(mapping=aes(x=tzone)) #create bargraph with x axis as time zone

#3

ggplot(data=ChickWeight) +
  geom_line(mapping=aes(x=Time, y=weight, color = Chick)) + #create linegraph with each chick number shown by color
  facet_wrap(~Diet, nrow=1) #create graphs denoted by each of the 4 diets

#4
df <- data_frame(subject = c(1,2,3,1,2,3),
                 group = c("Treatment", "Treatment", "Treatment", "Control",
                           "Control", "Control"),
                 vals = c(4, 6, 8, 5, 6, 10)
                )
df |>
  pivot_wider(names_from = group, values_from = vals) |> #make the names in the group row into columns
  mutate (difference = Treatment - Control) #create new column 




