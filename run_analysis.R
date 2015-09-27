require('plyr')

run_analysis <- function(dataset_dir = "UCI HAR Dataset") {
    ###########################################################################
    # Utility Functions
    ###########################################################################
    
    # Utility function that returns a vector of files paths
    generate_filenames <- function(group_name) {
        group_dir = paste(dataset_dir, group_name, sep = "/")
        subject_file = paste(group_dir, paste("subject_", group_name, ".txt", sep = ""), sep = "/")
        x_file = paste(group_dir, paste("X_", group_name, ".txt", sep = ""), sep = "/")
        y_file = paste(group_dir, paste("y_", group_name, ".txt", sep = ""), sep = "/")
        list(subject_file = subject_file, x_file = x_file, y_file = y_file)
    }
    
    # Validates that the files expected are present
    validate_files <- function(files) {
        if(any(sapply(files, file.exists) == FALSE)) {
            stop("Some of the expected data files are missing")
        }        
    }
    
    ###########################################################################
    # Start
    ###########################################################################
    
    # Determine data requires extracting
    if(dir.exists(dataset_dir) == FALSE) {
        zipfile_name = paste("getdata-projectfiles-", dataset_dir, ".zip", sep = "")
        if(file.exists(zipfile_name)) {
            unzip(zipfile_name, exdir = '.', overwrite = TRUE)
        }
        else stop("Unable to find data set")
    }
    
    # Get all the files names to load and confirm they all exists
    files_test <- generate_filenames("test")
    validate_files(files = files_test)
    
    files_train <- generate_filenames("train")
    validate_files(files = files_test)
    
    ###########################################################################
    # Merge test and train datasets to create a single dataset
    ###########################################################################
    full_x <- rbind(read.table(file = files_train$x_file), 
                    read.table(file = files_test$x_file))
    
    full_y <- rbind(read.table(file = files_train$y_file), 
                    read.table(file = files_test$y_file))
    
    full_subjects <- rbind(read.table(file = files_train$subject_file), 
                           read.table(file = files_test$subject_file))
    
    full_dataset <- cbind(full_x, full_subjects)
    
    ###########################################################################
    # Extracts only the measurements on the mean and standard deviation for 
    # each measurement. 
    ###########################################################################
    features_data <- read.table(paste(dataset_dir, "features.txt", sep = "/"))
    mean_and_std_colnums <- grep(".*-(mean|std)\\(\\)", features_data[,2])
    full_dataset <- full_dataset[, mean_and_std_colnums]
    
    
    ###########################################################################
    # Appropriately labels the data set with descriptive variable names and
    # Uses descriptive activity names to name the activities in the data set.
    ###########################################################################
    colnames(full_dataset) <- features_data[mean_and_std_colnums, 2]
    
    activities_data <- read.table(paste(dataset_dir, "activity_labels.txt", sep = "/"))
    full_y[, 1] <- activities_data[full_y[, 1], 2]
    colnames(full_y) <- "Activities"
    
    processed_data <- cbind(full_y, full_dataset)
   
    ###########################################################################
    # From the data set in step 4, creates a second, independent tidy data set 
    # with the average of each variable for each activity and each subject.
    ###########################################################################
    tidy_dataset = ddply(processed_data, c("Activities"), function (x) { 
        colMeans(x[, 2:66])
    })
    
    write.table(tidy_dataset, file = "tidy-dataset.txt", row.names = FALSE)
    
    tidy_dataset
}

