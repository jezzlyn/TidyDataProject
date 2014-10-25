################################################################################################
## This script performs the following on the data obtained from                               ##
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones        ##
##                                                                                            ##  
## 1. Merges the training and the test sets to create one data set.                           ##
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. ## 
## 3. Uses descriptive activity names to name the activities in the data set                  ##
## 4. Appropriately labels the data set with descriptive variable names.                      ##
## 5. From the data set in step 4, creates a second, independent tidy data set with the       ##
##    average of each variable for each activity and each subject.                            ##
################################################################################################

print("Analysis started")
ptm <- proc.time()

require(data.table)

# Remember to setwd()

dataDir <- "UCI HAR Dataset/"
testDataDir <- "UCI HAR Dataset/test/"
trainDataDir <- "UCI HAR Dataset/train/"


# Reads features and finds rows that contain the string mean or std
features <- read.table(paste(dataDir,"features.txt",sep = ""))
meanStdColsIndex <- grepl("[Mm]ean()|[Ss]td()",features$V2)
meanStdColsName <- as.character(features[meanStdColsIndex,"V2"])

# A1 - Reads the test data set and extracts only means and std columns
testDataSet <- read.table(paste(testDataDir,"X_test.txt",sep = ""))
testDataSet <- testDataSet[,meanStdColsIndex]
setnames(testDataSet,meanStdColsName)
  
# A2 - Reads test data subjects
testDataSubj <- read.table(paste(testDataDir,"subject_test.txt",sep = ""))
setnames(testDataSubj,1,"Subject")
  
# A3 - Reads test data labels
testDataLabels <- read.table(paste(testDataDir,"y_test.txt",sep = ""))
setnames(testDataLabels,1,"Activity")
  
# Combines A1, A2, and A3 into a data table for test data
testData <- cbind(testDataSubj,testDataLabels,testDataSet)

# B1 - Reads the train data set and extracts only means and std columns
trainDataSet <- read.table(paste(trainDataDir,"X_train.txt",sep = ""))
trainDataSet <- trainDataSet[,meanStdColsIndex]
setnames(trainDataSet,meanStdColsName)
  
# B2 - Reads train data subjects
trainDataSubj <- read.table(paste(trainDataDir,"subject_train.txt",sep = ""))
setnames(trainDataSubj,1,"Subject")
  
# B3 - Reads train data labels
trainDataLabels <- read.table(paste(trainDataDir,"y_train.txt",sep = ""))
setnames(trainDataLabels,1,"Activity")
  
# Combines B1, B2, and B3 into a data table for train data
trainData <- cbind(trainDataSubj,trainDataLabels,trainDataSet)
  
# Merges all rows in test data and train data
allData <- rbind(testData,trainData)
  
# Reads activity labels 
activities <- read.table(paste(dataDir,"activity_labels.txt",sep = ""))
setnames(activities,c("Activity","ActivityDescription"))
  
# Introduce ActivityDescription by merging
allData <- merge(x = allData, y = activities, by = "Activity", all.x=TRUE)
# Move ActivityDescription to 1st column
allData <- allData[,c(ncol(allData),1:(ncol(allData)-1))]
# remove activity column
allData <- allData[,-which(names(allData)=="Activity")] 
# replace column name 
setnames(allData,"ActivityDescription", "Activity")
# Move Activity to 2nd column
allData <- allData[,c(2,1,3:ncol(allData))] 

# Aggregates data
allData$Activity <- as.factor(allData$Activity)
allData$Subject <- as.factor(allData$Subject)
tidyData = aggregate(allData, by=list(Activity = allData$Activity, Subject=allData$Subject), mean)
tidyData <- tidyData[,c(2,1,5:ncol(tidyData))]

# Writes tidy data to text file
write.table(tidyData, "tidyData.txt", sep="\t", row.name=FALSE)

# Writes tidy data to csv file for easy reading, uncomment where required
# write.table(tidyData, "tidyData.csv", sep=",", row.name=FALSE)

print("Analysis completed")
print(proc.time() - ptm)
