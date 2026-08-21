# Q12

Tea=c(5,    11,    13,    18,    20,    47,48,    52,    55,    56,    58)    

Coffee=c(0,    0,    3,    11,    15,    16, 21,    21,    38,    52)     

mean(Tea)-mean(Coffee) # Estimate the difference in means 

xbar_Tea= replicate(5000, mean(sample( Tea, size= length(Tea), 
                                       replace=TRUE)))


xbar_Coffee= replicate(5000, mean(sample( Coffee, size= length(Coffee),
                                          replace=TRUE)))
d=xbar_Tea-xbar_Coffee
SE=sd(d)
SE
hist(d)

(mean(Tea)-mean(Coffee))+c(-1,1)*2*SE # 95% confidence interval for  difference in means 


# Q13
CD=read.csv("CarDepreciation.csv")
names(CD)

mean(CD$Depreciation) # mean
sd(CD$Depreciation) # standard deviation 

xbar_Dep= replicate(5000, mean(sample(CD$Depreciation, 
                                      size=length(CD$Depreciation), 
                                      replace=TRUE)))


hist(xbar_Dep, main = "Histogram of Cars Depreciating")
SE=sd(xbar_Dep) # SE 
SE

a=mean(CD$Depreciation)+c(-1,1)*2*SE # 95% C.I for mean depreciation 
a
b=mean(CD$Depreciation)+c(-1,1)*3*SE # 99.7% C.I for mean depreciation 
b
abline(v=a, col="red", lty=2)
abline(v=b, col="green", lty=2)