#LC 3.3-3.4 Homework problem 8
phat1 = replicate(1000,mean(sample(c(0,1),size = 536,
                                       replace = TRUE,
                                       prob = c(150/536,386/536))))
hist(phat1, main = "Teen in households below $75000")
phat2 = replicate(1000,mean(sample(c(0,1),size = 384,
                                   replace = TRUE,
                                   prob = c(126/384,258/384))))
sd(phat1)
hist(phat2, main = "Teen in households above $75000")
diff = phat1-phat2
sd(diff)

ninty_five_conf. = (386/536-258/384) + c(-1,1)*2*sd(diff)
ninty_five_conf.
#LC 3.3-3.4 Homework problem 12
tea = c(5,11,13,18,20,47,48,52,55,58)
coffee = c(0,0,3,11,15,16,21,21,38,52)
A=mean(tea)-mean(coffee)
xbar_Tea = replicate(5000, mean(sample(tea, size = length(tea),
                                       replace = TRUE)))
xbar_coffee = replicate(5000, mean(sample(coffee, size = length(coffee),
                                          replace = TRUE)))
d = xbar_Tea - xbar_coffee
SE = sd(d)
SE
ninty_five_conf.2 = A+c(-1,1)*2*SE
round(ninty_five_conf.2, 3)
