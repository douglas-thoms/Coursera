##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  Assignment.2.R
##  Date:       18Apr2019
##
##  Assignment 2 for Reproducible Research course
##  
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Library
##----------------------------------------------------------------------------

library(dplyr)
library(stringr)

#Does the document have a title that briefly summarizes the data analysis?
#1 -3 figures
#Does the analysis start from the raw data file (i.e. the original .csv.bz2 file)?
#Does the analysis address the question of which types of events are most harmful 
#to population health?
#Does the analysis address the question of which types of events have 
#the greatest economic consequences?
#Do all the results of the analysis (i.e. figures, tables, numerical summaries) 
#appear to be reproducible?
#Do the figure(s) have descriptive captions (i.e. there is a description near 
#the figure of what is happening in the figure)?

#QUESTIONS

#Across the United States, which types of events (as indicated in the EVTYPE variable) 
#are most harmful with respect to population health?
#Across the United States, which types of events have the greatest economic consequences?

#You must show all your code for the work in your analysis document. This may 
#make the document a bit verbose, but that is okay. 
#In general, you should ensure that echo=TRUE for every code chunk 
#(this is the default setting in knitr).

#DOCUMENT TITLE

#DATA PROCESSING

if(!exists("raw.data"))
raw.data <- read.csv("repdata_data_StormData.csv.bz2", na.strings = c("",NA))
#remove columns that will not be needed
intermediate.data <- raw.data %>%
        select(EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,
               CROPDMG,CROPDMGEXP)

#change EXP values to exponents according to vaue system

#H,h,K,k,M,m,B,b,+,-,?,0,1,2,3,4,5,6,7,8, and blank-character
#H,h = hundreds = 100
#K,k = kilos = thousands = 1,000
#M,m = millions = 1,000,000
#B,b = billions = 1,000,000,000
#(+) = 1
#(-) = 0
#(?) = 0
#black/empty character = 0
#numeric 0..8 = 10

intermediate.data$PROPDMGEXP <- intermediate.data$PROPDMGEXP %>% 
        str_replace_na(replacement = "0") %>%         
         str_replace_all(c("\\+" = "1", "\\-" = "0", "\\?" = "0",
                         "[1-8]" = "10", "[hH]" = "100", "[kK]" = "1e+03", "[mM]" = "1e+06",
                         "[bB]" = "1e+09"))

intermediate.data$CROPDMGEXP <- intermediate.data$CROPDMGEXP %>% 
        str_replace_na(replacement = "0") %>%         
        str_replace_all(c("\\+" = "1", "\\-" = "0", "\\?" = "0",
                          "[1-8]" = "10", "[hH]" = "100", "[kK]" = "1e+03", "[mM]" = "1e+06",
                          "[bB]" = "1e+09"))

#multiply column with EXP

# need to use exp to determine multiple
#determine five num distribution, anything strange?
#need to see how many NA - none
#need to clean up EVTYPE
#weird symbols in propdmgexp



propdmgexp.range <- unique(raw.data$PROPDMGEXP)

#weird symbols in cropdmg exp



#RESULTS

#Across the United States, which types of events (as indicated in the EVTYPE variable) 
#are most harmful with respect to population health?

#What columns are relevant to population health? - injuries, fatalities


#Across the United States, which types of events have the greatest economic consequences?

#What columns are revelant to economic consequence? - PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP


