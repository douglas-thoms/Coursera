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
X_test$dataType <- "test"
col_idx <- grep("dataType", names(X_test))
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
X_train$dataType <- "train"
col_idx <- grep("dataType", names(X_train))
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

        
        #combine test and train data
        #merged_dataset_with_order_reference <- rbind(X_test, X_train)
        
merged_dataset <- rbind(X_test, X_train)

        #set up for loop to do put in activity labels
        for (i in 1:length(activity_number)) {
                
                
                merged_dataset$y <- sapply(merged_dataset$y, str_replace_all, activity_number[i], activity_name[i])
                
        }
        
        #select final columns to show
        merged_dataset <- merged_dataset %>%
                                #rename y to activity
                                rename(activity = y) %>%
                                #select columns with means or std
                                select(subject, activity, dataType, matches("(mean\\(\\)| std\\(\\))"))
                                
        
        #select out columns only with std and mean
        #a second, independent tidy data set with the average of each variable for each activity and each subject
        #use mutate and create an extra column - name it tidyset

        outputDf <- data.frame()
        
        #create a loop 
        #1 - first filters according to subject
        input_matrix <- merged_dataset[merged_dataset$subject == 1, ]
        
        #3 select column to choose
        #4 - create a loop through all the different columns
        #5 - do tapply on all activities
        output_vector <- tapply(input_matrix[,4], input_matrix$activity, mean)
        
        
        
        print(variable_name)
        print(output_vector)
        
        #need to add in column name, take chance to rename mean properly
        tempDf <- data.frame(activity = names(output_vector), mean = output_vector)
        #rename column with proper value
        # t=time domain    f=frequency domain   Acc=accelerometer  Gyro= gyroscope Mag= magnitude Jerk = jerk
        variable_name <- paste(names(input_matrix[4]),"Average", sep = "")
        colnames(tempDf)[2] <- variable_name
        
        #after loop add subject
        #ownames(outputDf) <- c()
        #outputDf$subject <- 1
        #col_idx <- grep("subject", names(outputDf))
        #outputDf <- outputDf[, c(col_idx, (1:ncol(outputDf))[-col_idx])]
        
        
        
        #then use tapply to break it down to activities        
               
        return(merged_dataset)

  
}