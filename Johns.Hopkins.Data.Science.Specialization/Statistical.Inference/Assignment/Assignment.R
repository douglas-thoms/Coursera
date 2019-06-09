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

#calculate population parameters
pop.mean <- 1/lambda
pop.sd <- 1/lambda
pop.variance <- pop.sd^2

hist(rexp(1000))
abline(v = 5, col = "green", lwd = 3)

expo_mean <- NULL
for (i in 1 : 1000) expo_mean = c(expo_mean, mean(rexp(40, 0.2)))
hist(expo_mean)
abline(v = 5, col = "green", lwd = 3)

