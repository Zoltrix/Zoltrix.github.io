library(caret)
library(ggplot2)
library(randomForest)

set.seed(123)

#get the data files
if (!file.exists("./data/pml-training.csv")){
	download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
		      "./data/pml-training.csv")
}

#load the training data into R
data <- read.csv("data/pml-training.csv", na.strings = c("NA", ""))

#partition the data (70% training; 30% testing)
inTrain <- createDataPartition(y = data$classe, p = 0.7, list = FALSE)
training <- data[inTrain, ]
testing <- data[-inTrain, ]

dim(training)

#see how NA's are distributed among the data
nas <- sapply(training, function(x) sum(is.na(x)))
table(nas)

#remove FULL NA cols
bad.cols <- names(nas[nas == 13460])
training <- training[, !names(training) %in% bad.cols]
training <- training[, -c(1:7)]

#train using random forests model
modelFit <- train(classe ~ ., method = "rf", data = training)
saveRDS(modelFit, "rf.RDS")

#see confusion matrix to calculate accuracy
conf.matrix <- confusionMatrix(testing$classe, predict(modelFit, testing))
print(conf.matrix)
