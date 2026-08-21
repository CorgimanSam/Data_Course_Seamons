# Practice Exam question 39
p_hat.1 = 0.52
p_hat.2 = 0.48
SD = 0.01
CON_INT.1 = p_hat.1 + c(-1,1)*SD
CON_INT.2 = p_hat.2 + c(-1,1)*SD
ninty_five.1 <- p_hat.1 + c(-1,1)*2*SD
ninty_five.2 <-p_hat.2 + c(-1,1)*2*SD
# Practice Exam question 40
IQStates = read.csv("USStates.csv")
mean(IQStates[IQStates$Region == "S","IQ"])
mean(IQStates[IQStates$Region == "W","IQ"])
mean(IQStates[IQStates$Region == "NE","IQ"])
mean(IQStates[IQStates$Region == "MW","IQ"])

library(dplyr)
IQStates %>%
  group_by(Region)
  summarize(mean = mean(IQStates$Region),median = median(IQStates$Region),
            s = sd(IQStates$Region), n=n())
z=(IQStates$IQ - mean(IQStates$IQ))/sd(IQStates$IQ)
z
z[abs(z)>2]
IQStates$IQ[z[abs(z)>2]]
quantile(IQStates$IQ)
IQStates %>%
  top_n(IQ, n=5)
