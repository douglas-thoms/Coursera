##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  Extra.Project.Week.3.R
##  Date:       08JUNE2019
##
##  Assignment for Linear Regression
##  
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Library
##----------------------------------------------------------------------------

#install.packages("devtools")
#devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)


#Based on your analysis, would you conclude that there is a significant association between college major category and income?

#explore clean data
#dimension
data.summary <- summary(college)
View(data.summary)
#each observation different major, 173 majors, how many groups?
college.major.number <- length(college$major)
college.major_category.list <- unique(college$major_category)
college.major_category.number <- length(college.major_category.list)
#general distribution check
#are ditributions similar

#remove sample samples
#any outliers
#what variables actually relate
#approach 1 which variables most likely cofound 
#gender could confound with wage gap
#approach 2 use nested approach
#check for outliers