# READ ME

## Scripts
* run_analysis.R

## Computations
1) Unzips UCI HAR dataset archive and combines the following files into a dataset:
* X_train.txt
* subject_train.txt
* y_train.txt
* X_test.txt
* subject_test.txt
* y_test.txt
* features.txt
* activity_labels.txt

2) Renames variables to more verbose understandable names

3) Creates a tidy dataset of the averages of variables containing "mean()"
or "std()" broken down by subject and activity

## Arguments
* None
       
## Returns
* a wide tidy dataset consisting of variable names as column names, subject, activity and variable averages as observation

## Pseudocode
1) Downloads and unzips UCI HAR dataset
2) inputs the files above into vectors and dataframes
3) combines the files above into a single dataframes with variables (features) along columns and subject and activity along rows
4) renames variable in more verbose easy to understand format
4) it creates a second dataset - this is a tidy data set consisting of the averages of mean and standard variables broken down by subject and activity

## Calculations
* average of variables (see codebook for more details)
 
         
