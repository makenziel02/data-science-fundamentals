library(tidyverse)
library(class)
library(mosaic)
library(nycflights13)


load("Exam2.RData")

#6
select(q6data, -ncontrols) |> 
  pivot_wider(#data = q6data, 
            names_from = alcgp,
            values_from = ncases) |>
  separate_wider_delim(agegp, "-", names = c("lower_age", "upper_age"))

#7
View(iris)
virginica_petal <- iris |> filter(Species == "virginica") |> select(Petal.Length)

bstrap <- do(1000) *
  max(~Petal.Length, data = sample_n(virginica_petal, size= 200, replace=TRUE))

SE = sd(bstrap$max, na.rm=TRUE)

rangeUpper <- mean(bstrap$max, na.rm=TRUE) + 1.96 * SE
rangeUpper
rangeLower <- mean(bstrap$max, na.rm=TRUE) - 1.96 * SE
rangeLower


#8
# a 
sales_model <- lm(Sales ~ ., data = q8data)
msummary(sales_model)

# b
sales_model2 <- lm(Sales ~ CompPrice + Advertising + Price + Age + ShelveLoc, data=q8data)
msummary(sales_model2)

# g 
ggplot(q8data) +
  geom_point(aes(x=CompPrice, y=resid(sales_model2))) +
  geom_hline(yintercept =0)


#9
# b
set.seed(517)
random_q9 <- sample_frac(q9data, size = 1.0, replace = FALSE) 
random_q9 <- na.omit(random_q9)

nrow <- nrow(random_q9)
n <- as.integer(nrow*0.8)

train <- random_q9[1:n,] # 80% of the data
test <- random_q9[(n+1):nrow,] # 20% of the data

# c
trainDiabetes <- select(train, Diabetes)
train <- select(train, -Diabetes)
testDiabetes <- select(test, Diabetes)
test <- select(test, -Diabetes)

diabetes_knn <- knn(train=train, cl=trainDiabetes$Diabetes, test=test, k=5)
confusion <- tally(diabetes_knn ~ Diabetes, data=testDiabetes)
confusion
sum(diag(confusion)) / nrow(test) 


diabetes_knn2 <- knn(train=train, cl=trainDiabetes$Diabetes, test=test, k=15)
confusion2 <- tally(diabetes_knn2 ~ Diabetes, data=testDiabetes)
confusion2
sum(diag(confusion2)) / nrow(test) 




#10
View(flights)
flights |> mutate(weekday= weekdays(time_hour)) |>
  select(month,day,sched_arr_time,weekday) |> group_by(month) |> 
  #tally up the number of flights each month
  #weekday/number of flights per month
  #ran out of time
  
  
  











