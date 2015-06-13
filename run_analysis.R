library(plyr); library(dplyr);

############################################
##
############################################
prepare_dataset <- function(activities, features, type) {
        
        subfilename <- sprintf("./UCI HAR Dataset/%s/subject_%s.txt", type, type)
        xfilename <- sprintf("./UCI HAR Dataset/%s/X_%s.txt", type, type)
        yfilename <- sprintf("./UCI HAR Dataset/%s/y_%s.txt", type, type)
        
        # Read the recorded activity for dataset from the 'y_****.txt' file 
        ds_sub <- read.table(subfilename, col.names = c('subjectId'))
        
        # Read the recorded activity for dataset from the 'y_****.txt' file 
        ds_lbl <- read.table(yfilename, col.names = c('activityId'))
        
        # Join the 2 data frames by 'activityId' 
        ds_lbl <- join(ds_lbl, activities)
        activity <- ds_lbl$activity
                
        # Read the feature values for the dataset from the 'X_****.txt' file and assign feature column names 
        ds <- read.table(xfilename, col.names = features$columnName)
        
        # Extract only mean() and std() for all parameters
        ds <- select(ds, 1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215, 227:228, 240:241, 253:254, 266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543)
        
        # Build dataset as a Tidy dataset
        cbind(ds_sub, activity, ds)
}

############################################
##
############################################
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
        internal <- rbind(test_ds, train_ds)
        
        # Aggregates the values in order to return mean for each values for each activity and for each subject
        aggregate(. ~ activity:subjectId, internal, mean)
}