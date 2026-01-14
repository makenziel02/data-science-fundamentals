# Prepare animal radio tracking data for analysis
# CS370 Data Science Fundamentals, Homework 4

## Makenzie LaNier
## On my honor, as an Eckerd College student, 
## I pledge not to lie, cheat, or steal, nor to tolerate these behaviors in others.

library(readr)
library(tidyverse)

# be sure to set the current working directory in R Studio to the folder containing this file:
logFile = "tortoiseA_20200710-110936.csv"

# read the data from the log file
data <- readr::read_delim(
  logFile,
  delim = ',',
  col_names = c("tag", "datetime", "ID", "signal1", "noise1", "signal2", "noise2", "signal3", "noise3", "signal4", "noise4"),
  col_types = cols(
    tag = col_character(),
    datetime = col_datetime(format = "%Y%m%d-%H%M%S"),
    id = col_character(),
    signal1 = col_double(),
    noise1 = col_double(),
    signal2 = col_double(),
    noise2 = col_double(),
    signal3 = col_double(),
    noise3 = col_double(),
    signal4 = col_double(),
    noise4 = col_double()
  )
)

#eliminate rows that don't start with "data"
data <- filter(data, tag== "DATA")

#remove the tag column
data <- select(data, -tag)

#calculate total signal power
mydata <- data |> mutate(total_signal = signal1 +signal2 + signal3 +signal4)

#calculate signal to noise ratio for each row
mydata1 <- mydata|> mutate(snr1 = 10*log10(signal1/noise1)
                    ,snr2 = 10*log10(signal2/noise2), 
                    snr3 = 10*log10(signal3/noise3),
                    snr4= 10*log10(signal4/noise4))

#find maximum snr ratio for each row
mydata2 <- mydata1 |> rowwise() |> mutate(max = max(c(snr1, snr2, snr3, snr4)))

#eliminate rows with a max snr of less than 5
mydata3 <- filter(mydata2, max>5.0)

#create plots
ggplot(data=mydata3) +
  geom_point(mapping=aes(x=datetime, y=total_signal)) +
  facet_wrap(~ID, nrow=4)




