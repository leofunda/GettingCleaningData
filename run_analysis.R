
## Download the file and put the file in the data folder
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

## Unzip the file
unzip(zipfile="./data/Dataset.zip",exdir="./data")

## Packages to be used
library(data.table)
library(dplyr)

## set working directory
setwd("C:/Users/Leonardo/Desktop/datasciencecoursera/GettingCleaningData/data")

## Read Metadata
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

## Format training and test data sets
## training and test data sets are split into three files (subject, activity and features)
## Reading training data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
## Reading test data
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)




## Part 1 - Merging training and the test data sets into a single data set

## putting subject, activity and features objects, containing both the Training and the Test data
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

## Naming the columns
colnames(features) <- t(featureNames[2])

## Merge the data
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)




## Part 2 - Extracting the mean and the standard deviation

## Extract the column indices that have either mean or std in them.
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

## Add activity and subject columns and show the dimension of completeData
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

## create extractedData with the columns selected from requiredColumnsand show the dimension of requiredColumns.
extractedData <- completeData[,requiredColumns]
dim(extractedData)




## Part 3 - Uses descriptive activity names to name the activities in the data set

## The original activity field in extractedData is numeric. We need to change the type into character.
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

## We need to change the activity column into a factor.
extractedData$Activity <- as.factor(extractedData$Activity)




## Part 4 - Appropriately labels the data set with descriptive variable names

## To make the variables easily readable, the following actions will be performed
## Acc will be replaced with Accelerometer
## Gyro will be replaced with Gyroscope
## BodyBody will be replaced with Body
## Mag will be replaced with Magnitude
## Character f will be replaced with Frequency
## Character t will be replaced with Time
## etc...
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
names(extractedData)




## Part 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

## change Subject into a factor variable.
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

## create a data set with average for each activity and subject; Order it as requested and write it into processed.txt file
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Processed.txt", row.names = FALSE)











