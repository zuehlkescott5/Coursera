# Code Book for Course Project

## Overview

###  The script run_analysis.R performs the following process to clean up the data and create tiny data sets:

* Pull data from site https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

* Merges the training and test sets to create one data set.

* Reads features.txt and uses only the measurements on the mean and standard deviation for each measurement.

* Reads activity_labels.txt and applies human readable activity names to name the activities in the data set.

* Labels the data set with descriptive names. (Names are converted to lower case; underscores and brackets are removed.)

* Merges the features with activity labels and subject IDs. 

* The result is saved as tidyData.txt.

* The average of each measurement for each activity and each subject is merged to a second data set. 

* The result is saved as tidyData2.txt.

### Output from run_analysis.R

#### tidyData.txt is a 10299x88 data frame.

* The first column contains subject IDs.
* The second column contains activity names.
* The last 86 columns are measurements.

#### tidyData2.txt is a 40x88 data frame.

* The first column contains subject IDs.
* The second column contains activity names.
* The averages for each of the 86 attributes are in columns 3-88.