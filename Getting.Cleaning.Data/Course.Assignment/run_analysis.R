cleaning.data.assignment <- function(){

library(plyr)
library(dplyr)
library(stringr)
        
  #see if data directory exists, if not create it
  if(!file.exists("data")){
    
    dir.create("data")
    
  }
  
  #check if zip file has been downloaded - if not download and unzip file
  if(!file.exists("./data/cleaning.data.assignment.zip")){
        
        dir.create("./data/cleaning.data.assignment.zip")
      
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL, destfile = "./data/cleaning.data.assignment.zip")

        outPath <- "C:\\Users\\Douglas\\Documents\\Coursera\\Coursera\\Getting.Cleaning.Data\\Course.Assignment\\data"
        locFile = "./data/cleaning.data.assignment.zip"
        unzip(locFile,exdir = outPath)
        
        print(list.files("./data/"))

  }
        
        #use readtable to create data frames of following unzipped files below
        
        #data set of measurements, defined by column names in features.txt
        X_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
        #subject data that corresponds to measurements in X_train
        subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
        #activity data classified as numbers corresponds to X_train
        y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
        
        #data set of measurements, defined by column names in features.txt
        X_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
        #subject data that corresponds to measurements in X_test
        subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
        #activity data classified as numbers corresponds to X_test
        y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
        
        #vector of column names defining feature
        features <- read.table("./data/UCI HAR Dataset/features.txt")
        #activity data classified as numbers corresponds to X_train
        activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
        
        #make features the column names of X_test
        colnames(X_test) <- features[,2]
       
        #add training as variable
        X_test$volunteer <- "test"
        col_idx <- grep("volunteer", names(X_test))
        X_test <- X_test[, c(col_idx, (1:ncol(X_test))[-col_idx])]
        
        #add dummy sequence vector to document original order of data sets for future reference
        X_test_order_reference <- seq(1:nrow(X_test))
        X_test$order_reference <- X_test_order_reference
        col_idx <- grep("order_reference", names(X_test))
        X_test <- X_test[, c(col_idx, (1:ncol(X_test))[-col_idx])]
        
        #add name to subject_test
        names(subject_test) <- "subject"
        #add dummy sequence vector to document original order of data sets for future reference
        subject_order_reference <- seq(1:nrow(subject_test))
        subject_test <- cbind(subject_test, subject_order_reference)
   
        #combine subject_test and x_test
        X_test <- cbind(subject_test, X_test)
        
        #add name to subject_test
        names(y_test) <- "y"
        
        #add dummy sequence vector to document original order of data sets for future reference
        y_order_reference <- seq(1:nrow(y_test))
        #y_test <- as.character(y_test)
        y_test <- cbind(y_test, y_order_reference)
        
        #combine y_test and x_test
        X_test <- cbind(y_test, X_test)
        
        #coerce y_test into character to replace with activity labels
        X_test$y <- sapply(X_test$y, as.character)
        
        #make features the column names of X_train
        colnames(X_train) <- features[,2]
        
        #add training as variable
        X_train$volunteer <- "train"
        col_idx <- grep("volunteer", names(X_train))
        X_train <- X_train[, c(col_idx, (1:ncol(X_train))[-col_idx])]
        
        #add dummy sequence vector to document original order of data sets for future reference
        X_train_order_reference <- seq(1:nrow(X_train))
        X_train$order_reference <- X_train_order_reference
        col_idx <- grep("order_reference", names(X_train))
        X_train <- X_train[, c(col_idx, (1:ncol(X_train))[-col_idx])]
        
        #add name to subject_train
        names(subject_train) <- "subject"
        #add dummy sequence vector to document original order of data sets for future reference
        subject_order_reference <- seq(1:nrow(subject_train))
        subject_train <- cbind(subject_train, subject_order_reference)
        
        #combine subject_train and x_train
        X_train <- cbind(subject_train, X_train)
        
        #add name to subject_train
        names(y_train) <- "y"
        
        #add dummy sequence vector to document original order of data sets for future reference
        y_order_reference <- seq(1:nrow(y_train))
        #y_train <- as.character(y_train)
        y_train <- cbind(y_train, y_order_reference)
        
        #combine y_train and x_train
        X_train <- cbind(y_train, X_train)
        
        #coerce y_train into character to replace with activity labels
        X_train$y <- sapply(X_train$y, as.character)
        
        
        #set inputs for sapply str_replace_all
        activity_name <- as.character(activity_labels[,2])
        activity_number <- as.character(activity_labels[,1])
        
        
        #set up for loop to do put in activity labels
        for (i in 1:length(activity_number)) {
                
                
                X_train$y <- sapply(X_train$y, str_replace_all, activity_number[i], activity_name[i])
                
        }
        
        #combine test and train data
        merged_dataset_with_order_reference <- rbind(X_test, X_train)
        
        #remove columns used to ensure each of files match up properly
        merged_dataset <- select(merged_dataset_with_order_reference,-y_order_reference,-subject_order_reference,-order_reference)
        
        #select out columns only with std and mean
        #a second, independent tidy data set with the average of each variable for each activity and each subject
        #use mutate and create an extra column - name it tidyset
        
        
               
        return(merged_dataset)

  
}