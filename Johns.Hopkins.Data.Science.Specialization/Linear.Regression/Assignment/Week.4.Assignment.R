##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  Week.4.Assignment.R
##  Date:       31JULY2019
##
##  Assignment for Linear Models
##  
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Background
##----------------------------------------------------------------------------

#QUESTION

#You work for Motor Trend, a magazine about the automobile industry. Looking 
#at a data set of a collection of cars, they are interested in exploring the 
#relationship between a set of variables and miles per gallon (MPG) (outcome). 
#They are particularly interested in the following two questions:
        
#“Is an automatic or manual transmission better for MPG”
#"Quantify the MPG difference between automatic and manual transmissions"

# Criteria
# 
#         Did the student interpret the coefficients correctly?
#         Did the student do some exploratory data analyses?
#         
#         Did the student fit multiple models and detail their strategy 
#         for model selection?
#         
#         Did the student answer the questions of interest or detail 
#         why the question(s) is (are) not answerable?
#         
#         Did the student do a residual plot and some diagnostics?
#         
#         Did the student quantify the uncertainty in their conclusions 
#         and/or perform an inference correctly?
#         
#         Was the report brief (about 2 pages long) for the main body of the report 
#         and no longer than 5 with supporting appendix of figures?
#         
#         Did the report include an executive summary?
#         Was the report done in Rmd (knitr)?

#Instructions
# Question
# 
# Take the \color{red}{\verb|mtcars|}mtcars data set and write up an analysis to 
# answer their question using regression models and exploratory data analyses.
# 
# Your report must be:
#         
# Written as a PDF printout of a compiled (using knitr) R markdown document.
# Brief. Roughly the equivalent of 2 pages or less for the main text. 
# Supporting figures in an appendix can be included up to 5 total pages including 
# the 2 for the main report. The appendix can only include figures.
# Include a first paragraph executive summary.

##----------------------------------------------------------------------------
## Library
##----------------------------------------------------------------------------

library(dplyr)
library(car)
library(olsrr)

##----------------------------------------------------------------------------
## Data Acquiring and Cleaning
##----------------------------------------------------------------------------

raw_data <- mtcars

print(summary(raw_data))
View(raw_data)
print(dim(raw_data))

#any unusual values, NA

print(length(complete.cases(raw_data)))

#Classify variables properly
#mpg is continuous num
#cyl is factor
#disp is continuous num
#hp is continuous num
#drat is continuous num
#wt is continuous num
#qsec is continuous num
#vs is factor
#am is factor
#gear is factor
#carb is factor

proc_data <- raw_data %>%
        mutate(cyl = as.factor(cyl)) %>%
        mutate(vs = as.factor(vs)) %>%
        mutate(am = as.factor(am)) %>%
        mutate(gear = as.factor(gear)) %>%
        mutate(carb = as.factor(carb))

##----------------------------------------------------------------------------
## Exploratory Analysis
##----------------------------------------------------------------------------

#fivenum on continuous
fivenum.values <- sapply(cont_proc_data,fivenum)

#scatterplot
pairs(proc_data)

#boxplot
boxplot(mpg~am, data=mtcars)
boxplot(mpg~wt, data=mtcars)
boxplot(mpg~cyl, data=mtcars)
boxplot(mpg~hp, data=mtcars)

boxplot(am~wt, data=mtcars)
boxplot(am~cyl, data=mtcars)
boxplot(am~hp, data=mtcars)

##----------------------------------------------------------------------------
## Linear Regression Analysis
##----------------------------------------------------------------------------

#model strategy
#exploratory suggests auto transmission increase mpg

#initially did a lm with all variables to see if which look more significant
#order of significance wt,hp,am,disp, cyl, carb, qsec, vs, gear, carb
fit.all <- lm (mpg ~ ., proc_data)
print(summary(fit.all))
#test cofounding vif - wt, dis and hp has high vig
vif.fit.all <- sqrt(vif(fit.all))
print(vif.fit.all)

#initial lm shows significant increase in mpg which is expected with automatic
fit.am <- lm(mpg ~ am, proc_data)

#try a model only wt as most significant variable
fit.wt <- lm(mpg ~ wt, proc_data)

#add cyl
fit.wt.cyl <- lm(mpg ~ wt + cyl, proc_data)

#add hp
fit.wt.cyl.hp <- lm(mpg ~ wt + cyl + hp, proc_data)

#add disp, no relevant

#too many variables, for parsimonous choose between cyl and hp
summary(fit.wt.cyl)
summary(fit.wt.hp)

#vif for fit.wt.hp.am much worse than fit.wt.cyl.am
vif.fit.wt.cyl <- sqrt(vif(fit.wt.cyl))
print(vif.fit.wt.cyl)
vif.fit.wt.hp <- sqrt(vif(fit.wt.hp))
print(vif.fit.wt.hp)

#cyl has lower residuals and better vif, so ideal model to determine
#mpg is mpg ~ wt + cyl

#see if am is significant in either model, it is not
fit.wt.cyl.am <- lm(mpg ~ wt + cyl + am, proc_data)

#anova check shows fit.wt.cyl.am not significant
anova.check <- anova(fit.wt,fit.wt.cyl,fit.wt.cyl.am)

fit.wt.hp.am <- lm(mpg ~ wt + hp + am, proc_data)
summary(fit.wt.cyl)
summary(fit.wt.cyl.am)               

#use influence.measures to determine if any unusual residuals have too much leverage
influence.measures(fit.wt.cyl)
influence.measures(fit.wt.cyl.am)

#use plot to determine any unusual residuals with too much leverage, lacking normality
plot(fit.wt.cyl)
plot(fit.wt.cyl.am)

#test ofr heteroskedascity
ols <- ols_test_breusch_pagan(fit.wt.cyl, rhs = TRUE, multiple = TRUE)
ols.am <- ols_test_breusch_pagan(fit.wt.cyl.am, rhs = TRUE, multiple = TRUE)
ols.hp <- ols_test_breusch_pagan(fit.wt.hp
                                 , rhs = TRUE, multiple = TRUE)
##----------------------------------------------------------------------------
## Finding
##----------------------------------------------------------------------------

#“Is an automatic or manual transmission better for MPG”
#The linear regression model cannot confidently determine a correlation between
#type of transmission and mpg as it fails p-value test.
#"Quantify the MPG difference between automatic and manual transmissions"
#Model suggsts an increase of 0.15 mpg which is small and not statistically significant
#My modelling suggest that the weight and cylinders are the strongest variables
#correlated to miles-per-gallon.
