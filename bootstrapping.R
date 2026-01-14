library(tidyverse)

load("possum.Rdata")

bootstrapPlot <- ggplot(data=possum) +
  geom_point(mapping = aes(x=totalL, y=headL)) +
  geom_smooth(mapping = aes(x=totalL, y=headL), method = lm)

# create 20 bootstrap samples & fit lm to each of these samples

for (i in 1:20) {
  #create a model from bootstrap sample
  bootstrapModel <- 
    lm(headL ~ totalL, 
       slice_sample(possum, n = 50, replace = TRUE)) #bootstrap with 50 samples
  # predict the head length at the min (75) and max (97) range of total length
  bootstrapPrediction <-
    predict(bootstrapModel, 
            data.frame(totalL = c(75, 97)))
  # add line to the plot
  bootstrapPlot <- bootstrapPlot +
    geom_line(data = data.frame(totalL = c(75, 97), headL = bootstrapPrediction),
              aes(x=totalL, y=headL))
}
bootstrapPlot



