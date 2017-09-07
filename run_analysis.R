#load the dplyr library
library("dplyr")
library("plyr")

### Read the data
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
File <- "UCI HAR Dataset.zip"

if (!file.exists(File)) {
  download.file(Url, File, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(File)
}

#read the training data
trainX <- read.table(file.path(dataPath,"train", "X_train.txt"))
trainY <- read.table(file.path(dataPath,"train", "Y_train.txt"))
trainSub <- read.table(file.path(dataPath,"train", "subject_train.txt"))

#read the testing data
testX <- read.table(file.path(dataPath,"test", "X_test.txt"))
testY <- read.table(file.path(dataPath,"test", "Y_test.txt"))
testSub <- read.table(file.path(dataPath,"test", "subject_test.txt"))

#read the features

features <- read.table(file.path(dataPath,"features.txt"), as.is = TRUE)

#read the activities

act <- read.table(file.path(dataPath,"activity_labels.txt"))
colnames(act) <- c("id", "label")


###Step 1 -  Merge the training and testing dataset to create on dataset

training <- cbind(trainSub, trainX, trainY )

testing <- cbind( testSub,testX, testY)

total <- rbind(training , testing)

colnames(total) <- c("subject", features[,2], "activity")

### step 2 - Extract only the measurements on the mean and standard deviation for each measurement. 

total <- total[,grepl("subject|activity|mean|std", colnames(total))]

### step 3 - Use descriptive activity names to name the activities in the data set

total$activity <- factor(total$activity, levels = act[, 1], labels = act[, 2])

### step 4 - Appropriately label the data set with descriptive variable names

totalNames <- colnames(total)

#removing special characters
totalNames <- gsub("[\\(\\)-]", "", totalNames)

# creating descriptive names
totalNames <- gsub("^f", "frequencyDomain", totalNames)
totalNames <- gsub("^t", "timeDomain", totalNames)
totalNames <- gsub("Acc", "Accelerometer", totalNames)
totalNames <- gsub("Gyro", "Gyroscope", totalNames)
totalNames <- gsub("Mag", "Magnitude", totalNames)
totalNames <- gsub("Freq", "Frequency", totalNames)
totalNames <- gsub("mean", "Mean", totalNames)
totalNames <- gsub("std", "StandardDeviation", totalNames)
totalNames <- gsub("BodyBody", "Body", totalNames)

colnames(total) <- totalNames

### step - 5 From the data set in step 4, create
###a second, independent tidy data set with the average of each variable for each activity and each subject.
 
totAveraged <- ddply(total, .(subject, activity), function(x) colMeans(x[, 1:66]))

write.table(totAveraged, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)