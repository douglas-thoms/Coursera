# takes a directory of data files and a threshold for complete cases 
# inputs
# directory
# threshold
# 
# 
# calculates the correlation between sulfate and nitrate for monitor locations 
# where the number of completely observed cases (on all variables) 
# is greater than the threshold
# 
# if complete cases > threshold, use corr


corr <- function(directory,threshold = 0){
        ##reads a directory full of files and reports the number of completely 
        ##observed cases in each data file
        
        ##return a data frame where the first column is the name of the file and
        ##the second column is the number of complete cases
        
        #set path of directory using variable directory
        
        
        #set path for directory
        directory <-paste0(getwd(),"/",directory)
        
        
        #get contents of folder, store names in vector
        all_monitors <-list.files(directory)
        all_monitors_length <- length(all_monitors)
        #print(all_monitors_length)
        
        correlation_vector <- c()
        #sulfate_column<- c()
        #nitrate_column<- c()
        
        #loop to call files in monitor_list according to id and combine into total_data
        for(i in 1:all_monitors_length){
                
                input_data_filepath <- paste0(directory,"/",all_monitors[i])
                
                #print(i)
                #print(all_monitors[i])
                #print(input_data_filepath)
                
                # read file into R
                input_table<-read.csv(input_data_filepath,TRUE)
                #print(input_table)
                
                complete_case_logical <- complete.cases(input_table)
                #print(complete_cases)
                
                number_complete_cases <- nrow(input_table[complete_case_logical,])
                #print(number_complete_cases)
                
                if(number_complete_cases > threshold){
                        #print("correlate, greater than threshold")
                        
                        table_wo_NA <- input_table[complete_case_logical,]
                        #print(table_wo_NA)
                
                        correlation_vector <- c(correlation_vector, (cor(table_wo_NA$sulfate, table_wo_NA$nitrate,"complete.obs")))
                        #print(correlation_vector)
                
                        #sulfate <- table_wo_NA$sulfate
                        #sulfate_column <- append(sulfate_column, sulfate)
                        #print(sulfate)
                        
                        #nitrate <- table_wo_NA$nitrate
                        #nitrate_column <- append(nitrate_column, nitrate)
                        #print(nitrate)
                
                }
        
        
        }
        correlation_vector
        #print(correlation_vector)
        
}