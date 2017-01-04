
setwd("~/Desktop/Practical Machine Learning")

set.seed(5335)

#install.packages('caret', dependencies = TRUE)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ipred)
library(gbm)

# Check to see if pml-training.csv and pml-testing.csv already exists in working directory.  If they don't exist already, 
# pull data from Coursera webpage and load into working directory as files pml-training.csv and pml-testing.csv.
# If they do exist, skip.

# Data for this project was provided to the Coursera Practical Machine Learning course by http://groupware.les.inf.puc-rio.br/har.

if (!file.exists("pml-training.csv")) {
  tryCatch({
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "pml-training.csv")
    print("File did not exist, but was successfully downloaded from the URL.")
  },
  error = function(cond) {
    print(
      "File did not exist, but there was an error in downloading the file from the provided URL."
    )
  })
} else {
  print("pml-training.csv already exists")
}

if (!file.exists("pml-testing.csv")) {
  tryCatch({
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "pml-testing.csv")
    print("File did not exist, but was successfully downloaded from the URL.")
  },
  error = function(cond) {
    print(
      "File did not exist, but there was an error in downloading the file from the provided URL."
    )
  })
} else {
  print("pml-testing.csv already exists")
}


training.data <- read.csv('pml-training.csv', na.strings=c("NA","#DIV/0!",""))
testing.data <- read.csv('pml-testing.csv',na.strings=c("NA","#DIV/0!",""))


# Drop columns that contain mostly NA

training.data <- training.data[,colSums(is.na(training.data)) == 0] 
testing.data <- testing.data[,colSums(is.na(testing.data)) == 0] 

# Data Cleanup

## Remove small variance metrics
NZVcols.trian <- nearZeroVar(training.data, saveMetrics=TRUE)
training.data.NZV <- training.data[,NZVcols.trian$nzv==FALSE]

NZVcols.test <- nearZeroVar(training.data, saveMetrics=TRUE)
testing.data.NZV <- testing.data[,NZVcols.test$nzv==FALSE]



## Split training.data.NZV into a usable train and test set.


inTrain <- createDataPartition(training.data.NZV$classe, p=0.75, list=FALSE)
train.data <- training.data.NZV[inTrain, ]
test.data <- training.data.NZV[-inTrain, ]



## Convert factor variables into indicators, drop user_name field

train.data$user_name <- NULL
test.data$user_name <- NULL
testing.data.NZV$user_name <- NULL


train.data$cvtd_timestamp <- NULL
test.data$cvtd_timestamp <- NULL
testing.data.NZV$cvtd_timestamp <- NULL

train.data$X <- NULL
test.data$X <- NULL
testing.data.NZV$X <- NULL


# Set cross validation details

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random",allowParallel = TRUE)

## Basic Random Forest
rf.start <- proc.time()
rf <- train(classe~., data=train.data, method="rf", trControl = control, metric = "Accuracy")
rf.run <- proc.time() - rf.start
conf_rf.train <- confusionMatrix(train.data$classe, predict(rf, train.data))
conf_rf.test <- confusionMatrix(test.data$classe, predict(rf, test.data))

## Decision Tree
dt.start <- proc.time()
dt.base <- train(classe ~ ., data = train.data, method = "rpart", trControl = control,metric="Accuracy")
dt.run <- proc.time() - dt.start
conf_dt.train <- confusionMatrix(train.data$classe, predict(dt.base, train.data))
conf_dt.test <- confusionMatrix(test.data$classe, predict(dt.base, test.data))

## Bagged Decision Tree
bdt.start <- proc.time()
dt.bag <- bagging(classe~., data=train.data)
bdt.run <- proc.time() - bdt.start
conf_bdt.train <- confusionMatrix(train.data$classe, predict(dt.bag, train.data))
conf_bdtree.test <- confusionMatrix(test.data$classe, predict(dt.bag, test.data))








varImp(dt.bag)

valPreds <- data.frame(problem_id = testing.data$problem_id, predicted=predict(dt.bag, testing.data.NZV))
write.csv(valPreds, "PML Quiz Predictions.csv",row.names = FALSE)

print(valPreds)



