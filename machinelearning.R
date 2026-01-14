library(tidyverse)
library(mosaic)
library(class)
View(iris)

#split data into training and testing data
randomIris <- sample_frac(iris, size=1.0, replace = FALSE) #randomize set
train <- randomIris[1:120,] #first 80% of data set
test <- randomIris[121:150,] #remaining 20%

#see if there are even amounts of each species in the new sets
tally(~Species, data=train)
tally(~Species, data=test)

# split sets into two parts for knn function
trainSpecies <- select(train, Species)
train <- select(train, -Species)
testSpecies <- select(test, Species)
test <- select(test, -Species)

iris_knn <- knn(train= train, cl = trainSpecies$Species, test=test, k=3)

confusion <- tally(iris_knn ~ Species, data = testSpecies)


