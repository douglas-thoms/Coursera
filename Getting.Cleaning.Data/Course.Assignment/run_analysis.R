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
features <- read.table("UCI HAR Dataset/features.txt")
activity.labels <- read.table("UCI HAR Dataset/activity_labels.txt")

#make features the column names of x.test
colnames(x.test) <- features[,2]

#add test data type as variable and put variable at front
x.test$data.type <- "test"
col.idx <- grep("data.type", names(x.test))
x.test <- x.test[, c(col.idx, (1:ncol(x.test))[-col.idx])]

#add dummy sequence vector to document original order of data sets for future reference
#put column at front
x.test.order.reference <- seq(1:nrow(x.test))
x.test$order.reference <- x.test.order.reference
col.idx <- grep("order.reference", names(x.test))
x.test <- x.test[, c(col.idx, (1:ncol(x.test))[-col.idx])]

#add name to subject.test
names(subject.test) <- "subject"

#add dummy sequence vector to document original order of data sets for future reference
subject.order.reference <- seq(1:nrow(subject.test))
subject.test <- cbind(subject.test, subject.order.reference)

#combine subject.test and x.test
x.test <- cbind(subject.test, x.test)

#add name to y.test
names(y.test) <- "y"

#add dummy sequence vector to document original order of data sets for future reference
y.order.reference <- seq(1:nrow(y.test))
y.test <- cbind(y.test, y.order.reference)

#combine y.test and x.test
x.test <- cbind(y.test, x.test)

#coerce y into character to replace with activity labels
x.test$y <- sapply(x.test$y, as.character)

#make features the column names of x.train
colnames(x.train) <- features[,2]

#add training data type as variable
x.train$data.type <- "train"

#put column at front
col.idx <- grep("data.type", names(x.train))
x.train <- x.train[, c(col.idx, (1:ncol(x.train))[-col.idx])]

#add dummy sequence vector to document original order of data sets for future reference
x.train.order.reference <- seq(1:nrow(x.train))
x.train$order.reference <- x.train.order.reference

#put column at front
col.idx <- grep("order.reference", names(x.train))
x.train <- x.train[, c(col.idx, (1:ncol(x.train))[-col.idx])]

#add name to subject.train
names(subject.train) <- "subject"
#add dummy sequence vector to document original order of data sets for future reference
subject.order.reference <- seq(1:nrow(subject.train))
subject.train <- cbind(subject.train, subject.order.reference)

#combine subject.train and x.train
x.train <- cbind(subject.train, x.train)

#add name to y.train
names(y.train) <- "y"

#add dummy sequence vector to document original order of data sets for future reference
y.order.reference <- seq(1:nrow(y.train))
y.train <- cbind(y.train, y.order.reference)

#combine y.train and x.train
x.train <- cbind(y.train, x.train)

#coerce y into character to replace with activity labels
x.train$y <- sapply(x.train$y, as.character)


#set inputs for sapply str_replace_all
activity.name <- as.character(activity.labels[,2])
activity.number <- as.character(activity.labels[,1])

        
#combine test and train dat
merged.dataset.df <- rbind(x.test, x.train)

        #set up for loop to replace activity number with activity names
        for (i in 1:length(activity.number)) {
                
                
          merged.dataset.df$y <- sapply(merged.dataset.df$y, str_replace_all, activity.number[i], activity.name[i])
                
        }

#select final columns to show
merged.dataset.df <- merged.dataset.df %>%
                #rename y to activity
                rename(activity = y) %>%
                #select columns with means or std
                select(subject, activity, data.type, matches("mean\\(\\)"),
                        matches ("std\\(\\)"))

#rename columns with more clear variable names
colnames(merged.dataset.df) <- sapply(colnames(merged.dataset.df), 
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
tidy.df <- data.frame(colnames(c("subject","activity","data.type", features)))

        #loop through subjects to average activities
        for(h in 1:30){
                
                #filters according to subject
                input.df <- merged.dataset.df[merged.dataset.df$subject == 1, ]
                
                #create column.bound.df and sort according to activity
                column.bound.df <- data.frame(activity.name)
                column.bound.df <- arrange(column.bound.df,activity.name)
                
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