square<-function(x) {
        x*x
}



pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        #set path for directory
        directory <-paste0(getwd(),"/",directory)
        
        #get contents of folder, store names in vector
        all_monitors <-list.files(directory)
        
        #determine length of vector
        id_list_length<-length(id)
        
        #set up selected_monitors variable and total_data variable
        #selected_monitors <- NULL
        total_table <- NULL
        
        #loop to call files in monitor_list according to id and combine into total_data
        for(i in id[1]:id[id_list_length]){
                
                input_data_filepath <- paste0(directory,"/",all_monitors[i])
                
                # read file into R  
                input_table<-read.csv(input_data_filepath,TRUE)
                
                #add to total data
                total_table <- rbind(total_table,input_table)
                
                #make vector to combine all in one rbind
                #selected_monitors <- c(selected_monitors,all_monitors[i])
             
        }
        
        #choose pollutant to analyze, get that column
        data_with_NA <- total_table[[pollutant]]
        
        #find and remove NA
        find_NA <- is.na(data_with_NA)
        data_wo_NA <- data_with_NA[!find_NA]
        
        mean_of_pollutant <- mean(data_wo_NA)
        print(mean_of_pollutant)
        
}