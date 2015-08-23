runAnalysis <- function () {
   # Code to set working directory and unzipping file if neede
   # setwd("data/getdata_project")
   # datafile <- unzip("getdata_projectfiles_UCI HAR Dataset.zip")
   
   # read all data
   testSet <- read.table("UCI HAR Dataset/test/X_test.txt")
   trainSet <- read.table("UCI HAR Dataset/train/X_train.txt")
   activitytest <- read.table("UCI HAR Dataset/test/Y_test.txt")
   activitytrain <- read.table("UCI HAR Dataset/train/Y_train.txt")
   labels <- read.table("UCI HAR Dataset/activity_labels.txt")
   subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
   subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt")
   features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
   
   # add information about activity and person
   trainSet <- cbind(activitytrain, subjecttrain, trainSet)
   testSet <- cbind(activitytest, subjecttest, testSet)
   
   # Merges the training and the test sets to create one data set.
   data <- rbind(trainSet, testSet)
   features_vector <- c("Activity_nr", "Subject", features[,2])
   
   # Appropriately labels the data set with descriptive variable names. 
   names(data) <- features_vector
   
   # Uses descriptive activity names to name the activities in the data set
   names(labels) <- c("Activity_nr", "Activity")
   library(plyr)
   data <- join(data, labels)
   
   # Extracts only the measurements on the mean and standard deviation for each measurement. 
   mean_cols <- grep("mean()", names(data))
   std_cols <- grep("std()", names(data))
   data <- data[,c(2, 564, mean_cols, std_cols)]
   
   # From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
   newdata <- melt(data, id=c("Subject", "Activity"))
   newdcast <- dcast(newdata, Subject + Activity ~ variable, mean)
   write.table(newdcast, file="solution.txt", row.name=FALSE)
}
