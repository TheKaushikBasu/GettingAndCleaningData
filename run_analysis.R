# ------------------------------------------------------------------------------------
# Assignment Name: Peer-graded Assignment: Getting and Cleaning Data Course Project
# Student Name   : Kaushik Basu
# Created By     : 14-OCT-2017
# ------------------------------------------------------------------------------------

#if(!file.exists("./data")){dir.create("./data")}
#fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(fileUrl,destfile="./data/Dataset.zip")

# Unzip dataSet to /data directory
#unzip(zipfile="./data/Dataset.zip",exdir="./data")

# Reading trainings tables:
tabXTrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
tabYTrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
tabSubjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# Reading testing tables:
tabXTest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
tabYTest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
tabSubjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# Reading feature vector:
tabFeatures <- read.table('./data/UCI HAR Dataset/features.txt')

# Reading activity labels:
tabActivityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')


# Assign column names
colnames(tabXTrain) <- tabFeatures[,2] 
colnames(tabYTrain) <-"activityId"
colnames(tabSubjectTrain) <- "subjectId"

colnames(tabXTest) <- tabFeatures[,2] 
colnames(tabYTest) <- "activityId"
colnames(tabSubjectTest) <- "subjectId"

colnames(tabActivityLabels) <- c('activityId','activityType')


## 1. Merges the training and the test sets to create one data set.
##-------------------------------------------------------------------
mrgTrain <- cbind(tabYTrain, tabSubjectTrain, tabXTrain)
mrgTest <- cbind(tabYTest, tabSubjectTest, tabXTest)
mrgTestTrain <- rbind(mrgTrain, mrgTest)


## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
##-----------------------------------------------------------------------------------------------

# Read Column Names
colNames <- colnames(mrgTestTrain) 

# Assign ID, mean and standard deviation to a vector
mean_and_std <- (grepl("activityId" , colNames) | grepl("subjectId" , colNames) | 
                   grepl("mean.." , colNames) | grepl("std.." , colNames))

# Create subset from mrgTestTrain
setForMeanAndStd <- mrgTestTrain[ , mean_and_std == TRUE]



## 3. Uses descriptive activity names to name the activities in the data set
##----------------------------------------------------------------------------

setWithActivityNames <- merge(setForMeanAndStd, tabActivityLabels,
                              by='activityId',
                              all.x=TRUE)

## 4. Appropriately labels the data set with descriptive variable names.
##----------------------------------------------------------------------------
names(setWithActivityNames)<-gsub("^t", "time", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("^f", "frequency", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("Acc", "Accelerometer", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("Gyro", "Gyroscope", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("Mag", "Magnitude", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("BodyBody", "Body", names(setWithActivityNames))


## 5. From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject
##----------------------------------------------------------------------------
secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]

write.table(secTidySet, "tidyDataSet.txt", row.name=FALSE)

