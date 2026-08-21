# Question 13, Part 1
xbar= 70
sd = 3
prob.1 <- pnorm(67.2, xbar, sd, lower.tail = FALSE)
prob.1a <- pnorm(72.8, xbar, sd, lower.tail = FALSE)
prob.1b <- prob.1 - prob.1a
prob.1b
# Question 13, Part 2
height <- qnorm(0.12, xbar,sd)
round(height,1)
# Question 14, Part a
xbar.1 = 55.5
stand_dev = 2.7
prob.2a = pnorm(52.5, xbar.1, stand_dev, lower.tail = TRUE)
Prob.2b = pnorm(60.3,xbar.1, stand_dev, lower.tail = TRUE)
prob.2c = Prob.2b - prob.2a
round(prob.2c, 3)
# Question 14, Part b
hieght.1 = qnorm(0.95, xbar.1, stand_dev, lower.tail = TRUE)
round(hieght.1, 2)