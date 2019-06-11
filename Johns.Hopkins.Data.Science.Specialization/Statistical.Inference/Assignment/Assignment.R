##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  Assignment.R
##  Date:       08JUNE2019
##
##  Assignment for Statistical Inference
##  
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Library
##----------------------------------------------------------------------------

library(ggplot2)
library(dplyr)

##----------------------------------------------------------------------------
## Simulation
##----------------------------------------------------------------------------

#The exponential distribution can be simulated in R with rexp(n, lambda) 
#lambda is the rate parameter, lambda = 0.2 for all of the simulations
#mean of exponential distribution is 1/lambda - population mean
#standard deviation is also 1/lambda - population standard deviation
#variance is standard deviation - (1/lambda)^2
#investigate the distribution of averages of 40 exponentials = n?
#need to do a thousand simulations.

lambda = 0.2
n <- 40

#calculate population parameters
pop.mean <- 1/lambda
pop.sd <- 1/lambda
pop.variance <- 1/lambda^2

# Question 1

#If CTL holds, the average of mean samples should be close to the population mean
#(5) and a variance close to the sd^2/n as size of sample (n) becomes sufficiently
#large

print(pop.mean)

#The theoretical mean should be 5
#Calculating the average of means in 1000 simulations
expo <- NULL
for (i in 1 : 1000) expo = c(expo, rexp(40, 0.2))
hist(expo, main = "1000 Simulated Exponential Distributions", xlab = "", breaks = 50)
abline(v =5, col = "green", lwd = 3)
legend(20, 3000, legend="Population Mean/SD",
       col="green", lty=1, cex = 0.8)

expo_mean <- NULL
for (i in 1 : 1000) expo_mean = c(expo_mean, mean(rexp(40, 0.2)))
hist(expo_mean, main = "1000 Simulated Means of Exponential Distributions", xlab = "")
abline(v = 5, col = "green", lwd = 3)
abline(v = mean(expo_mean), col = "red", lwd = 3, lty =2)
legend(6, 225, legend = c("Population Mean", "Sample Mean Average"),
       col=c("green","red"), lty=c(1,2), cex = 0.8)

print(mean(expo_mean))

#Question 2

#overlay a random uniform and see how closely they match
#then theoretically calculate the mean
#overlay variance of normal
#theoretically calculate variance and see how lines up


theor.variance <- pop.variance/n
print(theor.variance)

sam.variance <- var(expo_mean)
print(sam.variance)

# Question 3

# Overlay a normal distribution with a mean of 5, standard deviation of
# .79

#plot a normal distribution in R

h <- hist(expo_mean, breaks = 10, density = 20,
          col = "lightgray", ylim = c(0,350),
          main = "1000 Simulated Exponential Distributions Means \n vs Normal Distribution", 
          xlab = "") 
xfit <- seq(min(expo_mean), max(expo_mean), length = 40) 
yfit <- dnorm(xfit, mean = mean(expo_mean), sd = sd(expo_mean)) 
yfit <- yfit * diff(h$mids[1:2]) * length(expo_mean) 
lines(xfit, yfit, col = "black", lwd = 2)

##----------------------------------------------------------------------------
## Part 2
##----------------------------------------------------------------------------

#want to determine if each combination of dose and supp has significant impact on growth
#null hypothesis is there is no growth

#want to review context of data
#https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ToothGrowth.html
#The response is the length of odontoblasts (cells responsible for tooth growth) 
#in 60 guinea pigs. Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) 
#by one of two delivery methods, orange juice or ascorbic acid (a form of vitamin C and coded as VC).
#Does difference in supplement or dose change tooth growth?
#Ho OJ

df <- ToothGrowth

#review data frame
#min 4.2, max 33.9, median 19.25, mean 18.8, sd 7.65

print(head(df))
print(dim(df))
print(fivenum(df$len))
print(mean(df$len))
print(sd(df$len))

#find NAs - no NAs
num.NA.obs <- length(df[complete.cases(df) == FALSE,]$len)
#Observation - no missing or strange values

#aggregate to see differences
ag.mean.df <- aggregate(.~supp+dose, df, mean)
ag.mean.df <- ag.mean.df %>%
        rename(mean.length = len)
ag.mean.df$mean <- round(ag.mean.df$mean, digits = 2)

ag.sd.df <- aggregate(.~supp+dose, df, sd)
ag.sd.df <- ag.sd.df %>%
        rename(sd.length = len)
ag.sd.df$sd <- round(ag.sd.df$sd, digits = 2)

ag.mean.sd <- inner_join(ag.mean.df,ag.sd.df)

ag.mean.sd <- ag.mean.sd[with(ag.mean.sd, order(supp, dose)),]

#Observation - mean increases in all cases, standard deviation shrinks with OJ
# but increases with VC, no crazy outliers

#create a plot of different values
df$dose <- as.factor(df$dose)
a <- ggplot(df, aes(x=dose, y=len)) + geom_boxplot() +
        facet_grid(.~supp)

plot(a)
#Observation - VC has large range in values

#approach - t.test 
#level significance (alpha) - 0.05, one tailed ho = ha, ha > ho
#assumption iid, normal distribution, randomnized, not paired

#calculations
#treat each as separate on off
#Calculate CI to see if ho rejected, is 0 in range
#Calculate p-value, see if it is above alpha of .975

#create subsets




small.dose.len <- df %>%
        filter(len, dose == 0.5) 

med.dose.len <- df %>%
        filter(len, dose == 1) 

high.dose.len <- df %>%
        filter(len, dose == 2) 

#subset data into distinct groups so can compare
#1 oj vs ac
#2 control does oj vs ac per does
#3 time intervals .1 vs .5 etc

OJ.small.len <- small.dose.len %>%
        filter(len, supp == "OJ")
VC.small.len <- small.dose.len %>%
        filter(len, supp == "VC")
OJ.med.len <- med.dose.len %>%
        filter(len, supp == "OJ")
VC.med.len <- med.dose.len %>%
        filter(len, supp == "VC")
OJ.high.len <- high.dose.len %>%
        filter(len, supp == "OJ")
VC.high.len <- high.dose.len %>%
        filter(len, supp == "VC")
# OJ vs VC
print("OJ vs VC")
print(t.test(len ~ supp, data = df))

# OJ vs VC compared according to doses
print("OJ vs VC compared according to doses")
print(t.test(len ~ supp, data = rbind(OJ.small.len, VC.small.len)))
print(t.test(len ~ supp, data = rbind(OJ.med.len, VC.med.len)))
print(t.test(len ~ supp, data = rbind(OJ.high.len, VC.high.len)))

# Different dose
print("Different dose")
print(t.test(len ~ dose, data = rbind(small.dose.len, med.dose.len)))
print(t.test(len ~ dose, data = rbind(med.dose.len, high.dose.len)))
print(t.test(len ~ dose, data = rbind(small.dose.len, high.dose.len)))

# Dose compared according to supp
print("Dose compared according to supp")
print(t.test(len ~ dose, data = rbind(OJ.small.len, OJ.med.len)))
print(t.test(len ~ dose, data = rbind(OJ.med.len, OJ.high.len)))
print(t.test(len ~ dose, data = rbind(OJ.small.len, OJ.high.len)))
print(t.test(len ~ dose, data = rbind(VC.small.len, VC.med.len)))
print(t.test(len ~ dose, data = rbind(VC.med.len, VC.high.len)))
print(t.test(len ~ dose, data = rbind(VC.small.len, VC.high.len)))
