library(dplyr);

##############################################################################################
## run_analysis() : 
##
##    Return the required tidy dataset after having prepared an internal tidy dataset based 
##    on the prodived data files generated by the Samsung Galaxy S
##
##  Pre-requisite
##    Download the raw ZIP file at the following URL : "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
##    Unzip it in your working directory.
##
##  Usage 
##    source('~/run_analysis.R')
##    result <- run_analysis()
##
##  Cookbook used to generate the required dataset is the following :
##
##     1- Read "activity_labels.txt" file which will be used to label the subject activity for each measure
##     2- Read "features.txt" file which will be used to label the measure columns
##     3- Prepare a tidy dataset for test measures by using the internal method prepare_dataset. See prepare_dataset cookbook.
##     4- Prepare a tidy dataset for train measures by using the internal method prepare_dataset. See prepare_dataset cookbook.
##     5- Combine the 2 partial datasets (test and train) in a 'big' one internal tidy dataset
##     6- Aggregate the values of this dataset in order to return the average of each variable for each activity and each subject
##     
run_analysis <- function() {
        
        # Retrieve Activities labels from the 'activity_labels.txt' file
        activity <- read.table('./UCI HAR Dataset/activity_labels.txt', col.names = c('activityId', 'activity'))
        
        # Read the features column names from the 'features.txt' file
        features <- read.table('./UCI HAR Dataset/features.txt', col.names = c('colunm', 'columnName'))
        
        # Build test dataset as a Tidy dataset
        test_ds <- prepare_dataset(activity, features, 'test')
        
        # Build train dataset as a Tidy dataset
        train_ds <- prepare_dataset(activity, features, 'train')
        
        # Clean up operation
        rm(activity, features)
        
        # Combine test and train datasets
        internal <- bind_rows(test_ds, train_ds)
        
        # Aggregates the values in order to return mean for each values for each activity and for each subject
        aggregate(. ~ activity:subjectId, internal, mean)
}

##############################################################################################
## prepare_dataset(activity, features, type) : 
##
##  Parameters
##    activity : data frame which is built from the "activity_labels.txt" file 
##    features : data frame which is built from the "features.txt" file 
##    type : expected string are "test" and "plan".
##
##    Return a prepared tidy dataset based on the prodived data files generated by the Samsung Galaxy S.
##    The type parameter will be used to select which files will be used here : test files or train files.
##
##  Cookbook used to generate the required dataset is the following :
##
##     1- Prepare file names to use with the type parameter thank to the naming convention used by Samsung 
##     2- Read the recorded subject for the dataset from the 'subject_****.txt' file 
##     3- Read the corresponding recorded activity for dataset from the 'y_****.txt' file 
##     4- Append the activity name as a new column of this last created dataset (Usage of dplyr:join method in order to not be puzzled by merge method which does not preserve the row order...)
##     5- Read the feature values for the dataset from the 'X_****.txt' file and assign feature column names thanks to features column labelled "columnName"
##     6- Extract only mean() and std() for all parameters of the feature values data frame created in step 5
##     7- Build the expected internal dataset by combining the previously created dataset :
##     8- Remove activityId column and return the dataset
##     
prepare_dataset <- function(activity, features, type) {
        
        # Prepare file names to use with the type parameter thank to the naming convention used by Samsung 
        subfilename <- sprintf("./UCI HAR Dataset/%s/subject_%s.txt", type, type)
        xfilename <- sprintf("./UCI HAR Dataset/%s/X_%s.txt", type, type)
        yfilename <- sprintf("./UCI HAR Dataset/%s/y_%s.txt", type, type)
        
        # Read the recorded subject for the dataset from the 'subject_****.txt' file 
        ds_sub <- read.table(subfilename, col.names = c('subjectId'))
        
        # Read the recorded activity for dataset from the 'y_****.txt' file 
        ds_lbl <- read.table(yfilename, col.names = c('activityId'))
        
        # Join the datataset by 'activityId' is performed. Note: inner_join method does not mix the row order like merge... 
        ds_lbl <- inner_join(ds_lbl, activity)
        
        # Read the feature values for the dataset from the 'X_****.txt' file and assign feature column names 
        ds_values <- read.table(xfilename, col.names = features$columnName)
        
        # Extract only mean() and std() for all parameters
        ds_values <- select(ds_values, 
                            contains("mean", ignore.case = FALSE), 
                            contains("std", ignore.case = FALSE), 
                            -contains("meanFreq", ignore.case = FALSE))
        
        # Build dataset as a Tidy dataset
        ds <- bind_cols(ds_sub, ds_lbl, ds_values)
        
        # Remove activityId column and return the dataset
        ds <- select(ds, -activityId)
}
