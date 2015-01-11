#
# Getting and Cleaning Data - Course Project
#
#
# Data to be used:
#     https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
# Script goals:
# 1.   Merges the training and the test sets to create one data set.
# 2.   Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.   Uses descriptive activity names to name the activities in the data set
# 4.   Appropriately labels the data set with descriptive variable names. 
# 5.   From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# Step 0 - download the data
# 
# Added commented code in the script to be able to simply run & replicate the results

#download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "data.zip")
#unzip("data.zip")

# Step 1 - merge the training and the test set
# (assumes the data in the working folder)

# read the test data
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
testActivities <- read.table("UCI HAR Dataset/test/y_test.txt")
#features can have leading spaces, need to remove them, 
#   then using read.table which splits by white space (any number of chars)
featData <- textConnection(sub("^\\s+", "", readLines("UCI HAR Dataset/test/X_test.txt")))
testFeatures <- read.table(featData)
close(featData)

# read the train data
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
trainActivities <- read.table("UCI HAR Dataset/train/y_train.txt")
featData <- textConnection(sub("^\\s+", "", readLines("UCI HAR Dataset/train/X_train.txt")))
trainFeatures <- read.table(featData)
close(featData)

# merge the test and train data frames
library(dplyr)
subjects <- rbind_all(list(testSubjects, trainSubjects))
activities <- rbind_all(list(testActivities, trainActivities))
features <- rbind_all(list(testFeatures, trainFeatures))

# merge all data frames to a single one
dataset <- cbind(subjects, activities, features)


# Step 1b - assign proper column labels 
#   (done before extracting only the required measurements as it's easier)

# read the feature names
featureList <- read.table("UCI HAR Dataset/features.txt")

# built the feature column names
featureNames <- make.names(featureList[,2])

# assign column names
#  done in an earlier stage as it's easier than to be performed at the end
names(dataset) <- c("Subject", "Activity", featureNames)


# Step 2 - extract only the mean and stdev info for each measurement

# checking for "mean()" and "std()" in feature name (we also have meanFreq in the dataset, which is not wanted)
requiredFeatures <- featureList[,2][grep("(mean|std)\\(\\)", featureList[,2], perl=TRUE)]

# build required column names list
reqFeatNames <- make.names(requiredFeatures)

# subset the data for the required columns only
dataSubset <- dataset[c("Subject", "Activity", reqFeatNames)]


# Step 3 - use descriptive activity names

# read the activities list
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")

# lookup instead of using indexing: we DO NOT assume that labels will always incremental values
#    (although it would had been easier, given the data, which makes it always to be the same as the index)
activitiesIndex <- match(dataSubset$Activity, activityLabels[,1])

# replace the Activity column with labels
dataSubset$Activity <- activityLabels[,2][activitiesIndex]


# Step 4 - descriptive column names

# we already have them from Step 1b, just clean the repetitive dots and the ones at the end of the variable name
names(dataSubset) <- gsub("\\.$", "", gsub("\\.+", "\\.", names(dataSubset), perl=TRUE), perl=TRUE)


# Step 5 - create a tidy data set with the average of each variable for each activity and each subject

# build the dataset
averageValues <- dataSubset %>% 
    group_by(Activity, Subject) %>%
    summarise_each(funs(mean))

# save it to a text file
write.table(averageValues, "averageValues.txt", row.names=FALSE)
