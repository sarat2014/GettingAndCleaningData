# Script Name  : run_analysis.R
# Author Name  : Sarat Daggubati
# Date Created : 2014-07-23
# Description  : This script works with "UCI Human Activity Recognition
#                Using Smartphones" Data Set. This code
#                1. Merges the training and the test sets to create one
#                   data set
#                2. Extracts only the measurements on the mean and standard
#                   deviation for each measurement
#                3. Uses descriptive activity names to name the activities
#                   in the data set
#                4. Appropriately labels the data set with descriptive
#                   variable names
#                5. Creates a second, independent tidy data set with the
#                   average of each variable for each activity and each
#                   subject

library("bitops")
library("RCurl")
library("plyr")
library("reshape2")
library("data.table")

run_analysis <- function() {
  
  # Set Path and download files
  current.path <- getwd()
  tidy.data.path <- file.path(current.path, "HARUsingSmartphones.txt")
  UCI.path <- file.path(current.path, "UCI HAR Dataset")
  file <- "Dataset.zip"
  url <- paste("https://d396qusza40orc.cloudfront.net/",
               "getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", sep = "")
  
  if(file.exists("HARUsingSmartphones.txt"))
    stop("HARUsingSmartphones.txt exists")
  
  message("Downloading file")
  if (!file.exists(file))
    download.file(url, file.path(current.path, file), quiet = TRUE)
  
  message("Unzip Started")
  if (!file.exists("UCI HAR Dataset"))
    unzip(file)
  
  # Read Subject files
  message("Reading Subject files")
  subject.train <- fread(file.path(UCI.path, "train", "subject_train.txt"))
  subject.test  <- fread(file.path(UCI.path, "test" , "subject_test.txt" ))
  
  # Read Activity files
  message("Reading Activity files")
  activity.train <- fread(file.path(UCI.path, "train", "Y_train.txt"))
  activity.test  <- fread(file.path(UCI.path, "test" , "Y_test.txt" ))
  
  # Read Data files
  message("Reading Data files")
  train <- data.table(read.table(file.path(UCI.path, "train", "X_train.txt")))
  test  <- data.table(read.table(file.path(UCI.path, "test" , "X_test.txt" )))
  
  # Merge the training and the test sets to create one data set
  message("Merge the training and the test sets")
  
  # Bind the data tables
  message("Creating Data Table")
  subject <- rbind(subject.train, subject.test)
  setnames(subject, "V1", "subject")
  activity <- rbind(activity.train, activity.test)
  setnames(activity, "V1", "activityNum")
  data <- rbind(train, test)
  
  # Merge columns
  subject <- cbind(subject, activity)
  data <- cbind(subject, data)
  
  # Set key
  setkey(data, subject, activityNum)
  message("Data Table Created")
  message("Success!")
  
  # Extract only the measurements on the mean and standard deviation
  message("Extract the mean and standard deviation")
  
  # Extract only the mean and standard deviation
  message("Extracting mean and standard deviation")
  features <- fread(file.path(UCI.path, "features.txt"))
  setnames(features, names(features), c("featureNum", "featureName"))
  
  # Subset only measurements for the mean and standard deviation
  message("Subseting mean and standard deviation")
  features <- features[grepl("mean\\(\\)|std\\(\\)", featureName)]
  
  # Convert the column numbers to a vector of variable names
  message("Converting column numbers")
  features$featureCode <- features[, paste0("V", featureNum)]
  
  # Subset variables using variable names
  message("Subset variables using variable names")
  select <- c(key(data), features$featureCode)
  data <- data[, select, with=FALSE]
  message("Success!")
  
  # Use descriptive activity names
  message("Use descriptive activity names")
  
  # Read activity_labels.txt
  activity.names <- fread(file.path(UCI.path, "activity_labels.txt"))
  setnames(activity.names, names(activity.names),
           c("activityNum", "activityName"))
  message("Success!")
  
  # Label with descriptive activity names
  message("Label with descriptive activity names")
  
  # Merge activity labels and set key
  message("Merge activity labels and set key")
  data <- merge(data, activity.names, by="activityNum", all.x=TRUE)
  setkey(data, subject, activityNum, activityName)
  
  # Melt the data table to reshape it
  message("Melt the data table")
  data <- data.table(melt(data, key(data), variable.name="featureCode"))
  
  # Merge activity name
  message("Merge activity name")
  data <- merge(data, features[, list(featureNum, featureCode, featureName)],
                by="featureCode", all.x=TRUE)
  
  # Create `activity`  and `feature`
  message("Create activity and feature variables")
  data$activity <- factor(data$activityName)
  data$feature <- factor(data$featureName)
  
  # Features with 1 category
  message("Features with 1 category")
  data$featJerk <- factor(grepl("Jerk", data$feature),
                          labels=c(NA, "Jerk"))
  data$featMagnitude <- factor(grepl("Mag", data$feature),
                               labels=c(NA, "Magnitude"))
  
  # Features with 2 categories
  message("Features with 2 categories")
  n <- 2
  y <- matrix(seq(1, n), nrow=n)
  x <- matrix(c(grepl("^t", data$feature),
                grepl("^f", data$feature)), ncol=nrow(y))
  data$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
  x <- matrix(c(grepl("Acc", data$feature),
                grepl("Gyro", data$feature)), ncol=nrow(y))
  data$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
  x <- matrix(c(grepl("BodyAcc", data$feature),
                grepl("GravityAcc", data$feature)), ncol=nrow(y))
  data$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
  x <- matrix(c(grepl("mean()", data$feature),
                grepl("std()", data$feature)), ncol=nrow(y))
  data$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
  
  # Features with 3 categories
  message("Features with 3 categories")
  n <- 3
  y <- matrix(seq(1, n), nrow=n)
  x <- matrix(c(grepl("-X", data$feature), grepl("-Y", data$feature),
                grepl("-Z", data$feature)), ncol=nrow(y))
  data$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))
  
  # Check to make sure all possible combinations of `feature` are accounted
  # for by all possible combinations of the factor class variables.
  message("Confirm Features")
  r1 <- nrow(data[, .N, by=c("feature")])
  r2 <- nrow(data[, .N, by=c("featDomain", "featAcceleration",
                             "featInstrument", "featJerk", "featMagnitude",
                             "featVariable", "featAxis")])
  r1 == r2
  message("Success!")
  
  # Create a tidy data set
  message("Create a tidy data set")
  setkey(data, subject, activity, featDomain, featAcceleration,
         featInstrument, featJerk, featMagnitude, featVariable, featAxis)
  tidy.data <- data[, list(count = .N, average = mean(value)), by=key(data)]
  write.table(tidy.data, tidy.data.path, quote=FALSE, sep="\t", row.names=FALSE)
  message("Tidy data set created")
  message("Success!")
}
