library(mosaic)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(grid)
library(gridExtra)
library(class)

load("Hwk8.RData")

#1
randomDF <- sample_frac(df, size=1.0, replace=FALSE)

trainDF <- randomDF[1:1316,]   # 80% of the data
testDF <- randomDF[1317:1645,]  # 20% of the data

testPregnant <- select(testDF, PregnantNow)

df_dt <- rpart(PregnantNow ~ Age + Education + MaritalStatus + Poverty
               + Work + BMI + HealthGen + nPregnancies + PhysActive +
                 TVHrsDay + AlcoholYear + Smoke100 + 
                 Marijuana + HardDrugs + SexAge + SexOrientation + SexNumPartnLife,
               data = trainDF, method = "class", control=rpart.control(minsplit=1))
df_dt
rpart.plot(df_dt)

confusion <- predict(df_dt, testDF, type="class")
tally(confusion ~ PregnantNow, data=testDF)

#2
randomDF <- sample_frac(df, size=1.0, replace=FALSE)

trainDF2 <- randomDF[1:1316,]   # 80% of the data
testDF2 <- randomDF[1317:1645,]  # 20% of the data
testPregnant <- select(testDF2, PregnantNow)

df_dt2 <- rpart(PregnantNow ~ Age + Education + MaritalStatus + Poverty
               + Work + BMI + HealthGen + nPregnancies + PhysActive +
                 TVHrsDay + AlcoholYear + Smoke100 + 
                 Marijuana + HardDrugs + SexAge + SexOrientation + SexNumPartnLife,
               data = trainDF2, method = "class", control=rpart.control(minsplit=1))
df_dt2
rpart.plot(df_dt2)

confusion <- predict(df_dt2, testDF2, type="class")
tally(confusion ~ PregnantNow, data=testDF2)

#3
df_knn <- df |>
  mutate(Education_code = case_when(Education == "8th Grade" ~ "1", 
                                    Education == "9 - 11th Grade" ~ "2",
                                    Education == "High School" ~ "3",
                                    Education == "Some College" ~ "4",
                                    Education == "College Grad" ~ "5",
                       ))


df_knn <- df_knn |> select(BMI, Age, Education_code, PregnantNow) 

# make all numbers integers
df_knn <- type.convert(df_knn, as.is =TRUE) |> na.omit()



random_dfknn <- sample_frac(df_knn, size=1.0, replace=FALSE)

n <- nrow(random_dfknn)
split_index = 0.8*n
train <- random_dfknn[1:split_index,]
test <- random_dfknn[268:n,]


trainPreg <- select(train, PregnantNow)
train <- select(train, -PregnantNow)
testPreg <- select(test, PregnantNow)
test <- select(test, -PregnantNow)

# create a kNN classifier
df2_knn <- knn(train=train, cl=trainPreg$PregnantNow, test=test, k=3)

confusion <- tally(df2_knn ~ PregnantNow, data=testPreg)
confusion
sum(diag(confusion)) / nrow(test) 

