Scripts: run_analysis.R

Computes:
1) unzips an archive and combines the following files into a dataset,
renaming variables to more understandable names
X_train.txt
subject_train.txt
y_train.txt
X_test.txt
subject_test.txt
y_test.txt
features.txt
activity_labels.txt

2) creates a tidy dataset of all the variables containing "mean()"
or "std()" and averages them according to subject and activity

Args: None
       
Returns: tidy dataset mentioned above   

Pseudocode:
1) Downloads and unzips UCI HAR dataset
2) inputs the files above into vectors and dataframes
3) combines the files above into a single dataframes with variables (features) along columns and subject activity along rows
4) renames variable in more verbose easy to understand format
4) it creates a second dataset - this is a tidy data set consisting of the averages of mean and standard variables broken down by subject and activity

The only calculations made were the average calculations of the standard and mean variables according to subject and activity.
 
         