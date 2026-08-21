# Hypothesis checking
light.1 <- c(9.17,6.94,4.99,1.71,5.43,10.26,4.67,11.67,7.15,5.33)
dark.1 <- c(2.83,4.60,6.52,2.27,5.95,4.21,4.00,2.53)
mean(light.1)
mean(dark.1)
boxplot(light.1, dark.1, horizontal = TRUE)
diff_mean <- mean(light.1)-mean(dark.1) # Statistics = difference in means(est.)
diff_mean

# make the group means to be equal, mix the data and randomly select the observ.
#for the first group.
data = c(light.1,dark.1)
DIFF = NULL
for(i in 1:1000){
data
id = sample(1:18, size = 10)
id
LL = data[id]
LL
DD = data[-id]
DD
DIFF = mean(LL)-mean(DD)
d <- c(d, DIFF)
}

d
hist(d)
# Part 2 of class
#1
Arsenic_ppm = 
c(0.119,0.118,0.099,0.118,0.275, 0.358,0.080,0.158, 0.310, 0.105, 0.073, 0.832, 0.517, 0.850)
boxplot(Arsenic_ppm, horizontal = TRUE,
        main = "Amount of Arsenic in private well water")
mean(Arsenic_ppm)
sd(Arsenic_ppm)
max(Arsenic_ppm)
z=(0.85-0.29)/0.27
z
quantile(Arsenic_ppm)
IQR(Arsenic_ppm)
range(Arsenic_ppm)
diff(range(Arsenic_ppm))
#2  
A=matrix(c(38,97,206,337), nrow=2,ncol = 2, byrow = TRUE)
plot(as.table(A))
prop.table(A,1)
A[1,1]
A[1,2]
A[2,1]
A[2,2]
#3
library(ggplot2)
ICU = read.csv("ICUAdmissions.csv")
hist(ICU$Systolic)
mean(ICU$Systolic)
sd(ICU$Systolic)
132.28+c(-1,1)*2*32.9521
sum(ICU$Systolic>66.38 & ICU$Systolic<198.18)
186/ length(ICU$Systolic)
pnorm(z)
qnorm(0.90)
#4
xbar = 528
SD = 118
z = (450-xbar)/SD
round(z,2)
