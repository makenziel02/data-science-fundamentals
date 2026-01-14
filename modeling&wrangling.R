library(nycflights13)
library(tidyverse)

daily <- 
  flights |>
  mutate(date=make_date(year, month, day)) |>
  group_by(date) |>
  summarize(n = n())

ggplot(data=daily) +
  geom_line(mapping = aes(x=date, y=n)) 

daily <- 
  daily |>
  mutate(wday = wday(date, label = TRUE))

ggplot(data = daily) + 
  geom_boxplot(mapping = aes(x=wday, y=n))

dailyModel <- lm(n ~ wday, data = daily)
predictions <- fitted(dailyModel)

ggplot(data = daily) + 
  geom_boxplot(mapping = aes(x=wday, y=n)) +
  geom_point(aes(x=wday, y=predictions), color='red', size=4)

daily$residuals <- resid(dailyModel)
ggplot(data = daily) +
  geom_line(mapping =aes(x=date, y= residuals)) +
  geom_hline(yintercept = 0)

daily |>
  filter(residuals < -100)


