library(mdsr)
library(tidyverse)
library(mosaic)


load("possum.Rdata")
ggplot(data=possum, aes(x=totalL, y=headL)) +
  geom_point() +
  geom_smooth(method=lm)

possum_model <- lm(headL ~ totalL, possum)

residuals <- resid(possum_model)
ggplot() +
  geom_point(aes(x=possum$totalL, y=residuals)) +
  geom_hline(yintercept = 0)

ggplot() +
  geom_histogram(aes(x=residuals))


load("electronics.Rdata")
ggplot(data=electro, aes(x=temperature, y=lifetime)) +
  geom_point() +
  geom_smooth(method = lm)
  
electronics_model <- lm(lifetime ~ temperature, data=electro)

residuals <- resid(electro)
ggplot() +
  geom_point(aes(x=temperature, y=residuals)) +
  geom_hline(yintercept = 0)





View(SAT_2010)

SAT_2010 <- mutate(SAT_2010, salary = salary/1000) |> filter(sat_pct = )
SAT_plot <- ggplot(SAT_2010, aes(x=salary, y=total)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ylab("Average total score on SAT") +
  xlab("Average teacher salary(thousands of USD")
SAT_plot
SAT_model1 <- lm(total ~ salary, data = SAT_2010)
msummary(SAT_model1)

median(SAT_2010$sat_pct)
SAT_2010 <- mutate(SAT_2010, SAT_group = ifelse(sat_pct <= 27, "Low", "High"))


SAT_plot <- ggplot(SAT_2010, aes(x=salary, y=total, color = SAT_group)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ylab("Average total score on SAT") +
  xlab("Average teacher salary(thousands of USD")
SAT_plot

SAT_model2 <- lm(total ~ salary + sat_pct, data = SAT_2010)
msummary(SAT_model2)



