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

cont_proc_data <- raw_data %>%
        select(mpg,disp,hp,drat,wt,qsec)
        


##----------------------------------------------------------------------------
## Exploratory Analysis
##----------------------------------------------------------------------------

#fivenum on continuous
sapply(cont_proc_data,fivenum)
#variance
sapply(cont_proc_data,sd)

#boxplot on continuous

#bar plot on factors

##----------------------------------------------------------------------------
## Linear Regression Analysis
##----------------------------------------------------------------------------

#initial lm shows significant increase in mpg which is expected, how much others involved
fit.am <- lm(mpg ~ am, mtcars)
#when bring in all variables, wt seems only significant, makes sense
fit.all <- lm (mpg ~ ., mtcars)
#try only weight and am
fit.wt.am <- lm(mpg ~am + wt, mtcars)
#wt,am,qsec,hp
fit.am.wt.qsec <- lm(mpg ~am + wt + qsec, mtcars)
#wt,hp,am,disp
fit.am.wt.qsec.hp <- lm(mpg ~am + wt + qsec + hp, mtcars)

anova.check <- anova(fit.am,fit.wt.am,fit.am.wt.qsec,fit.am.wt.qsec.hp)

#influence.measures
#dfbetas
#dffits

#plot fit
#check leverage

#first review impact of factors
#anova analysis

#vif
#review Heteroscedasticity of residuals
#review normaility of residuals