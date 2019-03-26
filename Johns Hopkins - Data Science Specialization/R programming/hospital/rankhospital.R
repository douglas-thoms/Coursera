rankhospital <- function(state, outcome, num = "best") {
        
        ##state variable is "state", must be legitimate state
        ##type of illness is "outcome", must be either
        ##"heart attack", "heart failure", "pneumonia"
        
        
        ## Read outcome data
        ##file has variable "file" and is condensed into
        ##"file_condensed
        ##read file
        
        file <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors = FALSE)
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
        
        #see if any TRUES and state is legitimate
        state_matches_logical <- file_condensed[c(2)] == state
        
        if(sum(state_matches_logical)==0){
                stop("invalid state")
        } 
        
        #set up vector of valid outcomes
        outcome_valid <- c("heart attack", "heart failure",
                           "pneumonia")
        
        ## Check that state and outcome are valid
        outcome_matches_logical <- outcome_valid == outcome
        
        
        if (sum(outcome_matches_logical)==0){
                stop("invalid outcome")
        }
        
        #filter only states entered
        file_condensed <- file_condensed[which(file_condensed$state==state),]
        bad <- is.na(file_condensed[,outcome])
        View(bad,"bad")
        file_condensed <-file_condensed[!bad,]
        
         View(file_condensed,"file condensed")
         
         ##The num argument can take values "best", "worst", or an integer indicating the ranking
         ##for best or worst, use nrow and then reverse as necessary
         
        if (num == "best") {
                
                #find index of minimum value in outcome column
                #which indexes are equal to min value in outcome column
                best_results <- which(file_condensed[,outcome] == min(file_condensed[,outcome]))
                
                #find corresponding hospitals and sort alphabetically
                best_hospitals <- sort(file_condensed[best_results,1])
                return(best_hospitals[1])
                
                
        }       else if (num == "worst") {
                ##order alphabetically
                file_condensed <- file_condensed[order(file_condensed$hospital),]
                View(file_condensed,"file condensed")
                ##order best to worst outcome
                ranked_hospitals <- file_condensed[order(file_condensed[,selected_outcome]),]
                View(ranked_hospitals,"ranked_hospitals")
                num <- nrow(ranked_hospitals)
                return(ranked_hospitals[num,1])                
                                             
        }       else if (num > nrow(file_condensed)) {
                        ##If the number given by num is larger than the number of hospitals in that
                        ##state, then the function should return NA.        
                        return(NA)
       
                        
        }       else {
                        ##order alphabetically
                        ##ties broken alphabetically
                        file_condensed <- file_condensed[order(file_condensed$hospital),]
                        View(file_condensed,"file condensed")
                        
                        ##order best to worst outcome
                        ranked_hospitals <- file_condensed[order(file_condensed[,selected_outcome]),]
                        
                        ## Return hospital name in that state with the given rank
                        ## 30-day death rate
                        View(ranked_hospitals,"ranked_hospitals")
                        return(ranked_hospitals[num,1])
                        
        }
         
        
}
