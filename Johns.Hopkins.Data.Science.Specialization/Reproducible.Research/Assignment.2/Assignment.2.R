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
        str_replace_all(c("\\+" = "1", "\\-" = "0", "\\?" = "0",
                         "[1-8]" = "10", "[hH]" = "100", "[kK]" = "1e+03", "[mM]" = "1e+06",
                         "[bB]" = "1e+09")) %>%
        as.numeric()

intermediate.data$PROPDMGEXP[is.na(intermediate.data$PROPDMGEXP)] <- 0

intermediate.data$CROPDMGEXP <- intermediate.data$CROPDMGEXP %>% 
        str_replace_all(c("\\+" = "1", "\\-" = "0", "\\?" = "0",
                          "[1-8]" = "10", "[hH]" = "100", "[kK]" = "1e+03", "[mM]" = "1e+06",
                          "[bB]" = "1e+09")) %>%
        as.numeric()

intermediate.data$CROPDMGEXP[is.na(intermediate.data$CROPDMGEXP)] <- 0

#multiply column with EXP

intermediate.data <- intermediate.data %>%
        transform(PROPDMG = PROPDMG * PROPDMGEXP) %>%
        transform(CROPDMG = CROPDMG * CROPDMGEXP) %>%
        mutate(TOTALDMG = CROPDMG + PROPDMG) %>%
        select(-PROPDMGEXP,-CROPDMGEXP)
        

#clean up EVTYPE entries
#subset EVTYPE =  summary entries, propdmg cropdmg values are zero - REMOVE
summary <- raw.data[grep("summary.*", raw.data$EVTYPE, ignore.case = TRUE),]

#rename all hurricane to hurrican, stripping names

hurricane <- raw.data[grep("hurricane.*", raw.data$EVTYPE, ignore.case = TRUE),]


#get count of each factor
#need to set up priority in naming to consolidate names
#first, standardize same name
#1 hurricane (involves storms and flooding)
#2 flood
#3 thunderstorms
#4 winds

distribution.EVTYPE <- aggregate(x = raw.data, by = list(raw.data$EVTYPE), FUN = length)


intermediate.data <- intermediate.data %>%
        filter(!grepl("summary.*|\\?|.*apache.*", EVTYPE, ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*hurricane.*|.*storm surge.*|.*tropical.*", 
                                "HURRICANE/STORM SURGE/TROPICAL STORM", 
                                EVTYPE, ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*flood.*", "FLOOD", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*TSTM.*|.*THUNDERSTORM.*", "THUNDERSTORM", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*cyclone.*|.*tornado.*|.*funnel cloud.*|torndao", 
                                "TORNADO/CYCLONE/FUNNEL CLOUD", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*wind.*", "WIND", EVTYPE, ignore.case = TRUE)) %>%        
        transform(EVTYPE = gsub(".*blizzard.*", "BLIZZARD", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*microburst.*", "MICROBURST", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*freez.*|.*frost.*", "FREEZE/FROST", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*hail.*", "HAIL", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*snow.*", "SNOW", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*light.*", "LIGHTNING", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*wint.*", "WINTER WEATHER", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*rain.*|.*shower.*|.*precipitation.*", "RAIN", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*cold.*", "COLD", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*wild.*fire.*", "WILD FIRE", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*flood.*", "FLOOD", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*mud.*|.*land.*", "FLOOD", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*heat.*", "FLOOD", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*surf.*|.*current.*|.*swell.*", "CURRENT/SURF", EVTYPE, 
                                ignore.case = TRUE)) %>%
        transform(EVTYPE = gsub(".*fire.*", "FIRE", EVTYPE, 
                                ignore.case = TRUE))

#Across the United States, which types of events (as indicated in the EVTYPE variable) 
#are most harmful with respect to population health?

health.harm.EVTYPE <- aggregate(cbind(FATALITIES,INJURIES)~EVTYPE, intermediate.data, sum)

health.harm.EVTYPE.cond <- health.harm.EVTYPE %>%
        mutate(fatalities.percent = round(FATALITIES/sum(FATALITIES),3)) %>%
        mutate(inuries.percent = round(INJURIES/sum(INJURIES),3)) %>%
        filter(FATALITIES >0) %>%
        filter(fatalities.percent >= 0.01)

#Across the United States, which types of events have the greatest economic consequences?

economic.EVTYPE <- aggregate(cbind(PROPDMG,CROPDMG,TOTALDMG)~EVTYPE, intermediate.data, sum)

economic.EVTYPE.cond <- economic.EVTYPE %>%
        mutate(percent = round(TOTALDMG/sum(TOTALDMG),3)) %>%
        filter(TOTALDMG >0) %>%
        filter(percent >= 0.01)

        
# need to use exp to determine multiple
#determine five num distribution, anything strange?
#need to see how many NA - none
#need to clean up EVTYPE
#weird symbols in propdmgexp

# partyid2 <- fct_collapse(intermediate.data$EVTYPE,
#                          missing = c("No answer", "Don't know"),
#                          other = "Other party",
#                          rep = c("Strong republican", "Not str republican"),
#                          ind = c("Ind,near rep", "Independent", "Ind,near dem"),
#                          dem = c("Not str democrat", "Strong democrat")
# )


EVTYPE.range <- unique(intermediate.data$EVTYPE)

#weird symbols in cropdmg exp



#RESULTS

#Across the United States, which types of events (as indicated in the EVTYPE variable) 
#are most harmful with respect to population health?

#What columns are relevant to population health? - injuries, fatalities


#Across the United States, which types of events have the greatest economic consequences?

#What columns are revelant to economic consequence? - PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP


