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
library(ggplot2)
library(forcats)
library(reshape2)
library(gridExtra)

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

##-----------------------------------------------------------------------------
## DATA PROCESSING
##-----------------------------------------------------------------------------

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

EVTYPE.range <- unique(intermediate.data$EVTYPE)

#Across the United States, which types of events (as indicated in the EVTYPE variable) 
#are most harmful with respect to population health?

#What columns are relevant to population health? - injuries, fatalities

health.harm.EVTYPE <- aggregate(cbind(FATALITIES,INJURIES)~EVTYPE, intermediate.data, sum)
health.harm.EVTYPE.freq <- aggregate(FATALITIES~EVTYPE, 
                                     intermediate.data, length)

health.harm.EVTYPE.freq <- health.harm.EVTYPE.freq %>%
        rename(EVENT.FREQUENCY = FATALITIES)
        
health.harm.EVTYPE <- inner_join(health.harm.EVTYPE, health.harm.EVTYPE.freq, 
                                 by = "EVTYPE")



health.harm.EVTYPE <- health.harm.EVTYPE %>%
        mutate(fatalities.percent = round(FATALITIES/sum(FATALITIES),3)) %>%
        mutate(injuries.percent = round(INJURIES/sum(INJURIES),3)) %>%
        arrange(desc(FATALITIES))
 
health.harm.EVTYPE$EVTYPE <- fct_other(health.harm.EVTYPE$EVTYPE,
                                       keep = health.harm.EVTYPE$EVTYPE[1:6],
                                       other_level = "OTHER")

health.harm.EVTYPE <- aggregate(.~EVTYPE, health.harm.EVTYPE, sum)

health.harm.EVTYPE <- health.harm.EVTYPE %>%
        mutate(fatalities.per.event = round(FATALITIES/EVENT.FREQUENCY,2)) %>%
        mutate(injuries.per.event = round(INJURIES/EVENT.FREQUENCY,2))

#What columns are revelant to economic consequence? - PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP

economic.EVTYPE <- aggregate(cbind(PROPDMG,CROPDMG,TOTALDMG)~EVTYPE, intermediate.data, sum)
economic.EVTYPE.freq <- aggregate(PROPDMG~EVTYPE, 
                                     intermediate.data, length)

economic.EVTYPE.freq <- economic.EVTYPE.freq %>%
        rename(EVENT.FREQUENCY = PROPDMG)

economic.EVTYPE <- inner_join(economic.EVTYPE, economic.EVTYPE.freq, 
                                 by = "EVTYPE")

economic.EVTYPE <- economic.EVTYPE %>%
        mutate(percent = round(TOTALDMG/sum(TOTALDMG),3)) %>%
        arrange(desc(TOTALDMG))

economic.EVTYPE$EVTYPE <- fct_other(economic.EVTYPE$EVTYPE,
                                       keep = economic.EVTYPE$EVTYPE[1:6],
                                       other_level = "OTHER")

economic.EVTYPE <- aggregate(.~EVTYPE, economic.EVTYPE, sum)

economic.EVTYPE <- economic.EVTYPE %>%
        mutate(TOTALDMG.per.event = round(TOTALDMG/EVENT.FREQUENCY,0)) %>%
        mutate(PROPDMG.per.event = round(PROPDMG/EVENT.FREQUENCY,0)) %>%
        mutate(CROPDMG.per.event = round(CROPDMG/EVENT.FREQUENCY,0))
##-----------------------------------------------------------------------------
## RESULTS
##-----------------------------------------------------------------------------

#Across the United States, which types of events (as indicated in the EVTYPE variable) 
#are most harmful with respect to population health?

#extra grid

health.harm.total <- health.harm.EVTYPE %>%
        select(EVTYPE,FATALITIES,INJURIES) %>%
        melt(id.vars = "EVTYPE", measure.vars = c("FATALITIES","INJURIES"))

g <- ggplot(data=health.harm.total, aes(x=EVTYPE, y=value, fill=factor(variable))) +
        geom_bar(colour="black", stat="identity",
                 position=position_dodge(),
                 size=.3) +                       
        scale_fill_hue(name="Harm") + xlab("") + ylab("") +
        ggtitle("Total Fatalities and Injuries") +    
        theme_bw()


        health.harm.per.event <- health.harm.EVTYPE %>%
                select(EVTYPE,fatalities.per.event,injuries.per.event) %>%
                melt(id.vars = "EVTYPE", measure.vars = c("fatalities.per.event",
                                                          "injuries.per.event"))
        
h <- ggplot(data=health.harm.per.event, aes(x=EVTYPE, y=value, fill=factor(variable))) +
                geom_bar(colour="black", stat="identity",
                         position=position_dodge(),
                         size=.3) +                       
                scale_fill_hue(name="Harm") + xlab("") + ylab("") +
        ggtitle("Fatalities and Injuries Per Event") +    
                theme_bw()



        grid.arrange(g,h, ncol=2)


        economic.total <- economic.EVTYPE %>%
                select(EVTYPE,PROPDMG,CROPDMG) %>%
                melt(id.vars = "EVTYPE", measure.vars = c("PROPDMG","CROPDMG"))
        
i <- ggplot(data=economic.total, aes(x=EVTYPE, y=value, fill=factor(variable))) +
                geom_bar(colour="black", stat="identity",
                         position="stack",
                         size=.3) +                       
                scale_fill_hue(name="Damage") + xlab("") + ylab("") +
        ggtitle("Total Crop and Property Damage") + theme_bw()
        
        
        economic.per.event <- economic.EVTYPE %>%
                select(EVTYPE,PROPDMG.per.event,CROPDMG.per.event) %>%
                melt(id.vars = "EVTYPE", measure.vars = c("PROPDMG.per.event",
                                                          "CROPDMG.per.event"))
        
j <- ggplot(data=economic.per.event, aes(x=EVTYPE, y=value, fill=factor(variable))) +
                geom_bar(colour="black", stat="identity",
                         position="stack",
                         size=.3) +                       
                scale_fill_hue(name="Damage") + xlab("") + ylab("") +
        ggtitle("Total Crop and Property Damage Per Event") +    
                theme_bw() + scale_y_log10()

        
        
        
        grid.arrange(i,j, ncol=2)
#bar

#Across the United States, which types of events have the greatest economic consequences?

#final
#bar


