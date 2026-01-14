library(mosaicData)
library(tidyverse)
library(tidyr)
library(dplyr)


View(HELPfull)
#1: create a table using pivot_wider
basedata <- HELPfull |> select(ID, TIME, DRUGRISK, SEXRISK) |> 
  filter(TIME==0 | TIME==6)
  
pivot_wider(
  data = basedata,
  names_from = TIME,
  values_from = c(DRUGRISK, SEXRISK)
)


#2: change how the dataframe looks

thisData= data.frame(
  grp = c("A", "A", "B", "B"),
  sex = c("F", "M", "F", "M"),
  meanL = c(0.22, 0.47, 0.33, 0.55),
  sdL = c(0.11, 0.33, 0.11, 0.31),
  meanR = c(0.34, 0.57, 0.40, 0.65),
  sdR = c(0.08, 0.33, 0.07, 0.27)
)

p2<-thisData |>
  pivot_longer(
    cols = meanL:sdR,
    names_to = "temp",
    values_to = "vals"
  ) |>
  #combined the sex column with the column containing the mean,sdr,sdl
  unite(col = "temp1", sex, temp, sep = ".") |>
  pivot_wider(names_from = temp1, values_from = vals) #make names in temp column, rows




#3: create tables containing country, year, rate10k
View(table1)
table1 |> mutate(rate10K = (cases / population) * 10000) |> select(-cases, -population)


View(table2)
Table2 <- pivot_wider(
  data=table2, 
  names_from = type,
  values_from = count
)
table2 <- Table2 |> mutate(rate10K = (cases / population) * 10000) |> select(-cases, -population)


t4a <-
  table4a |> 
  pivot_longer(cols = c("1999", "2000"), names_to ="year", values_to = "cases")

t4b <-
  table4b |> 
  pivot_longer(cols = c("1999", "2000"), names_to ="year", values_to = "population") 


Table4 <-
  left_join(t4a, t4b) |> 
  mutate(rate10K = (cases / population) * 10000) |> 
  select(-cases, -population)






#4: create a narrow dataset
Homework6_Problem4 |>
  separate(Address, c("Street", "City", "State"), sep="/") |>
  pivot_longer(`1Q`:`4Q`, names_to = "Quarter", values_to ="Taxes") |>
  arrange(Quarter)


