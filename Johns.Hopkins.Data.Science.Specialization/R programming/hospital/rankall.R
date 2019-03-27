rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## function reads the outcome-of-care-measures.csv file
        ## returns a 2-column data frame
        ## containing the hospital in each state that has the 
        ## ranking specified in num 
        
        ##state variable is "state", must be legitimate state
        ##type of illness is "outcome", must be either
        ##"heart attack", "heart failure", "pneumonia"
        
        
        ## Read outcome data
        ##file has variable "file" and is condensed into
        ##"file_condensed
        ##read file
        
        file <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available",
                         colClasses = "character")
        #View(file,"file")
        
        #create new data frame with only needed columns
        #hospital name, state, heart attack, heart failure,pneumonia
        
        ##outcome variable is "outcomeName"
        ##outcome name can have three values, "heart attack",
        ##column K (11) "Hospital 30-Day Death (Mortality) Rates from Heart Attack"
        ##"heart failure",
        ##column Q (17)Hospital 30-Day Death (Mortality) Rates from Heart Failure
        ##"pneumonia"
        ##column W (23) Hospital 30-Day Death (Mortality) Rates from Pneumonia
        ##hospital name in file has variable called 'Hospital.Name'
        
        file_condensed <- file[c(2,7,11,17,23)]
        names(file_condensed)<-c("hospital","state","heart attack","heart failure",
                                 "pneumonia")
        
        
        column_names <- colnames(file_condensed)
        #print(column_names)
        
        # Find the specific type of outcome to selects
        selected_outcome<-grep(outcome, column_names, value = FALSE)

        #set up vector of valid outcomes
        outcome_valid <- c("heart attack", "heart failure",
                           "pneumonia")
        
        ## Check that state and outcome are valid
        outcome_matches_logical <- outcome_valid == outcome
        
        
        if (sum(outcome_matches_logical)==0){
                stop("invalid outcome")
        }
        
        temp <- file_condensed[c(1,2)]
        temp$outcome=file_condensed[,selected_outcome]
        file_condensed <-temp
        
        file_condensed$outcome <- as.numeric(as.character(file_condensed$outcome))
        
        ##tie breaker is alphabetical
        #order alphabetically
        file_condensed <- file_condensed[order(file_condensed$hospital),]
        #View(file_condensed,"file condensed")
        
        #order by outcome
        ranked_hospitals <- file_condensed[order(file_condensed$outcome),]
        #View(ranked_hospitals,"ranked_hospitals")
        
        #split file_condensed into state_split_df
        state_split_df <- split(ranked_hospitals,ranked_hospitals$state)
        View(state_split_df,"state_split")
        
        #remove NA for outcome from files_condnesed
        #not some states may have no hospital left, so no name will show
        removeNA <- function(x) {
                bad <-is.na(x$outcome)
                #View(bad,"bad")
                x <-x[!bad,]
        }
        
       
        #remove NA
        ranking_hospitals <-lapply(state_split_df, removeNA)
        
        #function to capture hospital name for rank
        hospitalName <- function(x){
                x$hospital[num]
        }
        
        #function to capture hospital name for worst
        hospitalNameWorst <- function(x){
                tail(x$hospital,1)
        }
        
        if (num == "best"){
                        num <-1
                        #set best as indice 1
                        #find maximum nrow in state_split_df
                        #get name of hospitals across state_split_df according to indice
                        ranking_hospitals <-sapply(ranking_hospitals, hospitalName)
                        View(ranking_hospitals,"rankings")

        } else if (num == "worst") {
                ranking_hospitals <-sapply(ranking_hospitals, hospitalNameWorst)
                View(ranking_hospitals,"rankings")
                
        }  else {
                        ranking_hospitals <-sapply(ranking_hospitals, hospitalName)
                        View(ranking_hospitals,"rankings")
        }
        #                 #set worst indice as max nrow
        #                 num <<- nrow(x)
        #                 temp <<- nrow(x)
        # 
        # 
        #         } else {
        #                 #if specific rank chosen, passes as indice
        #         }
        # 
        #         }


        
        
        #create elements and data frame
        list_names <- names(ranking_hospitals)
        list_of_hospitals <- data.frame(hospital=ranking_hospitals, state=list_names,
                        row.names=list_names)

        return(list_of_hospitals)

}     

#I need to remove NA after they have been subset

#res <- lapply(sptx, tail,1)

#!is.na
               
#tail?
        ##for assembling matrix
        ##lapply(x, function (elt) {elt[,1]})
        #try sapply to collapse
        
        ##margin is dimension
        ##apply(x,2,order)

# #set correct indice according to best, worst or a number
# findNrow<-function(x){
#         
#         
#         if (num2 == "best"){
#                 num <<-1
#                 #set best as indice 1
#                 
#         } else if (num2 == "worst" && nrow(x) > temp )  {
#                 #set worst indice as max nrow
#                 num <<- nrow(x)
#                 temp <<- nrow(x)
#                 
#                 
#         } else {
#                 #if specific rank chosen, passes as indice
#         }
#         
# }
# 
# #find maximum nrow in state_split_df
# best_outcome <- lapply(state_split_df, findNrow)