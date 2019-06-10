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

hist(rexp(1000))
abline(v =5, col = "green", lwd = 3)
legend(2, 300, legend="Population Mean/SD",
       col="green", lty=1)

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


expo_mean <- NULL
for (i in 1 : 1000) expo_mean = c(expo_mean, mean(rexp(40, 0.2)))
hist(expo_mean)
abline(v = 5, col = "green", lwd = 3)
abline(v = mean(expo_mean), col = "red", lwd = 3, lty =2)
legend(6, 250, legend = c("Population Mean", "Sample Mean Average"),
       col=c("green","red"), lty=c(1,2))

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

h <- hist(expo_mean, breaks = 10, density = 10,
          col = "lightgray", xlab = "Accuracy", main = "Overall") 
xfit <- seq(min(expo_mean), max(expo_mean), length = 40) 
yfit <- dnorm(xfit, mean = mean(expo_mean), sd = sd(expo_mean)) 
yfit <- yfit * diff(h$mids[1:2]) * length(expo_mean) 

lines(xfit, yfit, col = "black", lwd = 2)

