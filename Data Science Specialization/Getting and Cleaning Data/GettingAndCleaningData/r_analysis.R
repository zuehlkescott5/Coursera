
# Ensure required packages installed on machine
# install.packages("data.table")
# install.packages("dplyr")

#Load libraries

library(data.table)
library(dplyr)

datafile <- "Alldata.zip"
dataURL <-
  "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

## Download and unzip the dataset:

if (!file.exists(datafile)) {
  tryCatch({
    download.file(dataURL, datafile)
    print("File did not exist, but was successfully downloaded from the URL.")
  },
  error = function(cond) {
    print(
      "File did not exist, but there was an error in downloading the file from the provided URL."
    )
  })
} else {
  print("File Alldata.zip already exists")
}
if (!file.exists("UCI HAR Dataset")) {
  tryCatch({
    unzip(datafile)
    print("File UCI HAR Dataset did not exist, but was successfully unzipped.")
  },
  error = function(cond) {
    print("File UCI HAR Dataset did not exist, and there was an error in unzipping the file.")
  })
} else {
  print("File UCI HAR Dataset already exists.")
}


# Read in features list, convert to row and use to make feature names column.

featuresList <- read.table('UCI HAR Dataset\\features.txt',header=FALSE)
featuresList.asrow <- t(featuresList[2])

# Read in activity Labels, add column titles.

activityLabels <- read.table('UCI HAR Dataset\\activity_labels.txt',header=FALSE)
colnames(activityLabels) <- c("ActivityID", "Activity")

# Read in train data, add column names and column-bind to create Trainer dataset.

subject.Train <- read.table('UCI HAR Dataset\\train\\subject_train.txt',header=FALSE)
colnames(subject.Train) <- "Subject"
feat.Train <- read.table('UCI HAR Dataset\\train\\x_train.txt',header=FALSE)
colnames(feat.Train) <- featuresList.asrow
act.Train <- read.table('UCI HAR Dataset\\train\\y_train.txt',header=FALSE)
colnames(act.Train) <- "ActivityID"
activity.Train <- merge(act.Train, activityLabels, by = "ActivityID")

trainer <- cbind(subject.Train, activity.Train, feat.Train)


# Read in test data, add column names and column-bind to create Tester dataset.

subject.Test <- read.table('UCI HAR Dataset\\test\\subject_test.txt',header=FALSE)
colnames(subject.Test) <- "Subject"
feat.Test <- read.table('UCI HAR Dataset\\test\\x_test.txt',header=FALSE)
colnames(feat.Test) <- featuresList.asrow
act.Test <- read.table('UCI HAR Dataset\\test\\y_test.txt',header=FALSE)
colnames(act.Test) <- "ActivityID"
activity.Test <- merge(act.Test, activityLabels, by = "ActivityID")
tester <- cbind(subject.Test, activity.Test, feat.Test)

# Combine trainer and tester files to create a single DataSet

ModelDataSet <- rbind(trainer, tester)

# Get only columns dealing with mean and standard deviation

ModelDataSetMeanSTD <- grep(".*Mean.*|.*Std.*", names(ModelDataSet), ignore.case=TRUE)

#Extract desired columns from ModelDataSetMeanSTD.  Add "1,2,3" to beginning as placeholder for subject, activityid, activity.  Filter ModelDataSet to include only
#columns with Mean or Standard Deviation values for all Trainer + Test observations

TargetColumns <- c(1,2,3,ModelDataSetMeanSTD)
TargetData <- ModelDataSet[,TargetColumns]
TargetData$ActivityID <- NULL

# Rename columns to have more descriptive titles, then write data to file tidy.txt

names(TargetData)<-gsub("Gyro", "Gyroscope", names(TargetData))
names(TargetData)<-gsub("BodyBody", "Body", names(TargetData))
names(TargetData)<-gsub("Mag", "Magnitude", names(TargetData))
names(TargetData)<-gsub("-mean()", "Mean", names(TargetData), ignore.case = TRUE)
names(TargetData)<-gsub("^f", "Frequency", names(TargetData))
names(TargetData)<-gsub("tBody", "TimeBody", names(TargetData))
names(TargetData)<-gsub("-std()", "STD", names(TargetData), ignore.case = TRUE)
names(TargetData)<-gsub("-freq()", "Frequency", names(TargetData), ignore.case = TRUE)
names(TargetData)<-gsub("angle", "Angle", names(TargetData))
names(TargetData)<-gsub("gravity", "Gravity", names(TargetData))
names(TargetData)<-gsub("Acc", "Accelerometer", names(TargetData))
names(TargetData)<-gsub("^t", "Time", names(TargetData))

write.table(TargetData, file = "tidy.txt", row.names = FALSE)

# Aggregate data to create tidy2Data file, and write to Directory as txt file.

tidy2Data <- aggregate(. ~Subject + Activity, TargetData, mean)
tidy2Data <- tidy2Data[order(tidy2Data$Subject,tidy2Data$Activity),]
write.table(tidy2Data, file = "tidy2.txt", row.names = FALSE)

str(tidy2Data)
