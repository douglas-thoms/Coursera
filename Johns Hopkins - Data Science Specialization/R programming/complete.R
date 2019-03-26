complete <- function(directory,id=1:332){
        ##reads a directory full of files and reports the number of completely 
        ##observed cases in each data file
        
        ##return a data frame where the first column is the name of the file and
        ##the second column is the number of complete cases
        
        #set path of directory using variable directory
        
        
        #set path for directory
        directory <-paste0(getwd(),"/",directory)
        
        
        #get contents of folder, store names in vector
        all_monitors <-list.files(directory)
       
        
        #determine length of vector
        id_list_length<-length(id)
        

        #create empty data frame to add ID, nobs values
        ID_column<- c()
        nobs_column<- c()
        
        
        #loop to call files in monitor_list 
         for(i in 1:id_list_length){
        
                 input_data_filepath <- paste0(directory,"/",all_monitors[id[i]])
        
                 #print(all_monitors[id[i]])
                 #print(i)
                 
                 # read file into R
                 input_table<-read.csv(input_data_filepath,TRUE)
                 #print(input_table)
                 
                 
                 complete_case_logical<-complete.cases(input_table)
                 #print(complete_case_logical)
                 
                 nobs <- nrow(input_table[complete_case_logical,])
                 #print(nobs)
                 nobs_column <- append(nobs_column, nobs)
                 #print(nobs_column)
                 ID_column <- append(ID_column, id[i])
                 #print(ID_column)
                 
                
                 

         }
        
        complete_cases <- data.frame(ID=ID_column,nobs=nobs_column)
        #str(complete_cases)
        print(complete_cases)
        
        }