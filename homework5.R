# CS370 Data Science Fundamentals, Homework 5

## Makenzie LaNier
## On my honor, as an Eckerd College student, 
## I pledge not to lie, cheat, or steal, 
## nor to tolerate these behaviors in others.

library(tidyverse)
library(ggrepel)
library(Lahman)
library(nycflights13)


statenames <- tibble(names = state.name, twoletter = state.abb)
glimpse(statenames)

statedata <- tibble(
  names = state.name, 
  income = state.x77[, 2], 
  illiteracy = state.x77[, 3]
)

statesdata <- inner_join(statenames, statedata, by=c("names" = "names"))

#scatterplot showing illiteracy and income with labels and a smoother
#i've noticed that there are a lot of states crowded around under 1 illiteracy
#the ones crowded around less than 1 illiteracy also have the same range of income
#LA seems like an outlier 
ggplot(data=statesdata, mapping=aes(illiteracy, income)) +
  geom_text_repel(mapping = aes(x=illiteracy, y=income, label=twoletter), size=3)+
  geom_point() +
  geom_smooth()


#every player who accumulated at least 300 HR and SB
View(Batting)
View(People)
names <- inner_join(Batting, People, by=c("playerID" = "playerID"))
stats <- select(names, nameGiven, HR, SB)
stats1 <- stats |> group_by(nameGiven) |>
  summarize(
    totalHR = sum(HR),
    totalSB = sum(SB)
  )
playertotals <- filter(stats1, totalHR >= 300, totalSB >= 300)

fulldataset <- inner_join(names, playertotals, by=c("nameGiven" = "nameGiven"))

#every pitcher who accumulated 300 wins and 3000 strikeouts
View(Pitching)
names2 <- inner_join(Pitching, People, by=c("playerID" = "playerID"))
stats2 <- select(names2, nameGiven, W, SO)
stats3 <- stats2 |> group_by(nameGiven) |>
  summarize(
    totalW = sum(W),
    totalSO= sum(SO)
  )
pitchertotals <- filter(stats3, totalW >= 300, totalSO >= 3000)

#name and year of players with at least 50 homeruns in one season
test3 <- names |>
  group_by(yearID) |>
  filter(HR >= 50) |>
  select(nameGiven, yearID, HR)
#print player with lowest batting average 
print(min(test3$HR))
filter(test3, HR==50)

#planes with missing date of manufacture
View(planes)
filter(planes, is.na(year)) |>  nrow()


#5 most common manufacturers 
planes |> 
  select(manufacturer) |> 
  count(manufacturer) |>
  arrange(desc(n)) |>
  head(5)

#oldest plane that flew from nyc in 2013
View(flights)
flights |>
  select(tailnum, year, dep_time) |>
  arrange(year) |>
  head(1)


#number of planes that flew from nyc included in planes table
included <- inner_join(flights, planes, by = c("tailnum" = "tailnum"))
included |> 
  group_by(tailnum) |>
  nrow()


