##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  run_analysis.R
##  Date:       28Feb2019
##
##Computes:
## 1) unzips an archive and combines the following files into a dataset,
## renaming variables to more understandable names
## X_train.txt
## subject_train.txt
## y_train.txt
## X_test.txt
## subject_test.txt
## y_test.txt
## features.txt
## activity_labels.txt
##
## 2) creates a tidy dataset of all the variables containing "mean()"
## or "std()" and averages them according to subject and activity
## 
## Args: None
##         
## Returns tidy dataset mentioned above       
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

        
              
        
library(plyr)
library(dplyr)
library(stringr)
  
  #download and unzip file
#check if zip file has been downloaded - if not download and unzip file

if(!file.exists("UCI HAR Dataset/train/X_train.txt")){
        
        file.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(file.url, destfile = "cleaning.data.assignment.zip")
        
        out.path <- "C:\\Users\\Douglas\\Documents\\Coursera\\Coursera\\Getting.Cleaning.Data\\Course.Assignment"
        local.file = "cleaning.data.assignment.zip"
        unzip(local.file,exdir = out.path)
        
}
        
#use readtable to create data frames of following unzipped files below
x.train <- read.table("UCI HAR Dataset/train/X_train.txt")
subject.train <- read.table("UCI HAR Dataset/train/subject_train.txt")


y.train <- read.table("UCI HAR Dataset/train/y_train.txt")
x.test <- read.table("UCI HAR Dataset/test/X_test.txt")
subject.test <- read.table("UCI HAR Dataset/test/subject_test.txt")


y.test <- read.table("UCI HAR Dataset/test/y_test.txt")
features <- read.table("UCI HAR Dataset/features.txt", as.is = TRUE)
activity.labels <- read.table("UCI HAR Dataset/activity_labels.txt")



#ceate column names
features <- features$V2
features <- c("subject","activity",features)

#combine files into merged dataset
dataset.test <- cbind(subject.test, y.test, x.test)
dataset.train <- cbind(subject.train, y.train, x.train)
dataset <- rbind(dataset.test,dataset.train)

colnames(dataset) <- features

#subset only columns with mean() or std()
dataset <- dataset[c("subject","activity", grep("mean\\(\\)|std\\(\\)", names(dataset), value = TRUE))]

#update features variable
features <- colnames(dataset)


        #set up for loop to replace activity number with activity names
        for (i in 1:length(activity.labels[,2])) {


          dataset$activity <- sapply(dataset$activity, str_replace_all, as.character(activity.labels[i,1]),
                                               as.character(activity.labels[i,2]))

        }


#rename columns with more clear variable names
colnames(dataset) <- sapply(colnames(dataset), 
                                   str_replace_all,
                                   c("^t" = "Time Domain ", 
                                       "^f"= "Frequency Domain ", 
                                       "Acc"="Accelerometer ",
                                       "Gyro" = "Gryroscope ", 
                                       "Mag" = "Magnitude ", 
                                       "-" = " ", 
                                       "X" = "Vector X ",
                                       "Y" = "Vector Y ",
                                       "Z" = "Vector Z ", 
                                       "mean\\(\\)" = "Mean",
                                       "std\\(\\)" = "Standard Deviation",
                                       "mad\\(\\)" = "Median Absolute Deviation",
                                       "max\\(\\)" = "Maximum",
                                       "min\\(\\)" = "Minimum",
                                       "sma\\(\\)" = "Signal Magnitude area",
                                       "energy\\(\\)" = "Energy Measure",
                                       "iqr\\(\\)" = "Inerquartile entropy",
                                       "entropy\\(\\)" = "Signal entropy",
                                       "arCoeff\\(\\)" = "Autoregression",
                                       "correlation\\(\\)" = "Correlation",
                                       "maxInds\\(\\)" = "Index of Frequency Component",
                                       "meanFreq\\(\\)" = "Weighted average of frequency components",
                                       "skewness\\(\\)" = "skewness of frequency domain",
                                       "kurtosis\\(\\)" = "kurtosis of frequency interval",
                                       "bandsEnergy\\(\\)" = "Energy of frequency interval",
                                       "angle\\(\\)" = "Angle Between Vectors"))



#create empty dataset to add averaged variables later on
tidy.df <- data.frame(colnames(features))

        #loop through subjects to average activities
        for(h in 1:30){
                
                #filters according to subject
                input.df <- dataset[dataset$subject == 1, ]
                
                #create column.bound.df and sort according to activity
                column.bound.df <- data.frame("activity" = activity.labels[,2])
                column.bound.df <- arrange(column.bound.df,activity.labels[,2])
                
                #get number of columns for for loop later
                cols_to_average = ncol(input.df)
                
                
                #loop through columns to average each
                for(i in 4:cols_to_average){
                
                        #5 - do tapply on all activities
                        output <- tapply(input.df[,i], input.df$activity, mean)
                        
                        #need to add in column name, take chance to rename mean properly, sort
                        temp.df <- data.frame(activity = names(output), mean = output)                
                        temp.df <- arrange(temp.df,activity)
                        
                        #rename variables adding in average
                        variable.name <- names(input.df[i])
                        variable.name <- str_replace_all(variable.name,c("Vector X " = "Vector X Average",
                                                                         "Vector Y " = "Vector Y Average",
                                                                         "Vector Z " = "Vector Z Average"))
                        #replace variable name with new variable ame
                        colnames(temp.df)[2] <- variable.name
                        
                        #add 2nd column of temp.df to column.bound.df which is the averages of the activities of the variable
                        column.bound.df <- cbind(column.bound.df, temp.df[2])
                
                }
        
        #add subject column
        column.bound.df$subject <- h
        
        #put colum first
        col.idx <- grep("subject", names(column.bound.df))
        column.bound.df <- column.bound.df[, c(col.idx, (1:ncol(column.bound.df))[-col.idx])]
        
        #add column.bound.df rows to tidy.df, the final dataset
        tidy.df <- rbind(tidy.df, column.bound.df)
        
        }        


write.table(tidy.df, file = "tidyDataset.txt",row.name = FALSE)

return(tidy.df)