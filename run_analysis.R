##########################################################################################################

##  Coursera Getting and Cleaning Data Course Project
##  Matthias Hoffmann
##  20161106

##  run_analysis.R File Description:

##  This script will perform the following steps on the UCI HAR Dataset: 
##  1.Merges the training and the test sets to create one data set.
##  2.Extracts only the measurements on the mean and standard deviation for each measurement.
##  3.Uses descriptive activity names to name the activities in the data set
##  4.Appropriately labels the data set with descriptive variable names.
##  5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##########################################################################################################

library(dplyr)

rm(list = ls())

##  1.Merges the training and the test sets to create one data set.

##  Reading the data from files
features <- read.table("UCI HAR Dataset/features.txt",header=FALSE)
activityType <- read.table("UCI HAR Dataset/activity_labels.txt",header=FALSE)
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt",header=FALSE)
xTrain <- read.table("UCI HAR Dataset/train/x_train.txt",header=FALSE)
yTrain <- read.table("UCI HAR Dataset/train/y_train.txt",header=FALSE)

##  Assigin column names to the data 
colnames(activityType) <- c("activityId","activityType")
colnames(subjectTrain) <- "subjectId"
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activityId"

##  Create the final training set by merging yTrain, subjectTrain, and xTrain
trainingData <- bind_cols(yTrain,subjectTrain,xTrain)

##  Reading in the test data
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt",header=FALSE)
xTest <- read.table("UCI HAR Dataset/test/x_test.txt",header=FALSE) 
yTest <- read.table("UCI HAR Dataset/test/y_test.txt",header=FALSE) 

##  Assign column names to the test data 
colnames(subjectTest) <- "subjectId"
colnames(xTest) <- features[,2] 
colnames(yTest) <- "activityId"

##  Create the final test set by merging the xTest, yTest and subjectTest data
testData <- bind_cols(yTest,subjectTest,xTest)

##  Combine training and test data to create a final data set
finalData <- bind_rows(trainingData,testData)

##  Create a vector for the column names from the finalData with the desired mean() & stddev() columns
colNames <- colnames(finalData)

##  2.Extracts only the measurements on the mean and standard deviation for each measurement. 

##  Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

##  Subset finalData table based on the logicalVector
finalData <- finalData[logicalVector==TRUE]

##  3.Uses descriptive activity names to name the activities in the data set

##  Merge the finalData set with the acitivityType table 
finalData <- full_join(finalData,activityType, by="activityId")

##  Updating the colNames vector to include the new column names
colNames <- colnames(finalData)

##  4.Appropriately labels the data set with descriptive variable names. 

##  Renaming the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("-std$","StdDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("^(t)","time",colNames[i])
  colNames[i] <- gsub("^(f)","freq",colNames[i])
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
}

##  Reassigning the new descriptive column names to finalData
colnames(finalData) <- colNames

##  5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##  Create a new table finalDataNoActivityType without the activityType column
finalDataNoActivityType <- finalData[,names(finalData) != 'activityType']

##  Summarizing the finalDataNoActivityType table to include the mean of each variable for each activity and each subject
tidyData <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c("activityId","subjectId")],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)

##  Joining the tidyData with activityType to include descriptive acitvity names
tidyData <- full_join(tidyData,activityType,by="activityId")
tidyData <- arrange(tidyData, activityType)

##  Export the tidyData set table
write.table(tidyData, "tidyData.txt",row.names=TRUE,sep='\t')


