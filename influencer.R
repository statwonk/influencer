# Christopher Peters
# Influencer

# setwd("C:/Users/Chris/Desktop/RCode/influencer/influencer/")
setwd("C:/R_stuff/influencer/influencer")
data <- read.csv("train.csv")
test <- read.csv("test.csv")

pairs(data[ , 1:5])

set.seed(1)
train <- sample(1:length(data$Choice), 0.8 * length(data$Choice), replace = FALSE)

# install.packages("gbm")
library(gbm)

ada_1 <- gbm(Choice ~ ., distribution = "adaboost", 
             data = data[train, ], n.trees = 2000, 
             shrinkage = 0.005,
             interaction.depth = 6, train.fraction = 1, cv.folds = 4)

ada_preds_cv <- ifelse(predict(ada_1, data[-train, ], type = "response") < 0, 0, 1)
ada_preds_test <- ifelse(predict(ada_1, test, type = "link") < 0, 0, 1)

write.csv(ada_preds_test, "preds_1.csv", row.names = F)

confusionMatrix(ada_preds_cv, data[-train, 1])

# install.packages("caret")
# install.packages("e1071")
# install.packages("pROC")
library(caret)
library(e1071)
library(pROC)

inTrain <- createDataPartition(y = data$Choice,
                               p = 0.8)

ctrl <- trainControl(method = "cv", 
                     number = 3,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)


data$Choice <- as.factor(data$Choice)
levels(data$Choice) <- c("X0", "X1")

library(randomForest)

tuneRF(data[inTrain$Resample1, -1], data[inTrain$Resample1, 1], 5, ntreeTry = 50, stepFactor = 2, improve = 0.01,
       trace=TRUE, plot=TRUE, doBest = T)

rfGrid <- expand.grid(.mtry = c(2, 3, 4))

rf_1 <- train(Choice ~ .,
                data = data[inTrain$Resample1, ],
                method = "rf",
                metric = "ROC",
                tuneLength = 3,
                trControl = ctrl,
                tuneGrid = rfGrid)

rf_classes <- predict(rf_1, newdata = data[-inTrain$Resample1, ])
confusionMatrix(rf_classes, data[-inTrain$Resample1, 1])

rf_preds_test <- predict(rf_1, test, type = "prob")

write.csv(rf_preds_test[2], "preds_2.csv", row.names = F)

tree_1 <- train(Choice ~ .,
          data = data[inTrain$Resample1, ],
          method = "rpart",
          metric = "ROC",
          tuneLength = 15,
          trControl = ctrl)

tree_classes <- predict(tree_1, newdata = data[-inTrain$Resample1, ])
confusionMatrix(tree_classes, data[-inTrain$Resample1, 1])

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(tree_1$finalModel)

preds <- predict(tree_1$finalModel, newdata = data[-inTrain$Resample1, ], type = "class")
  table(preds, data[-inTrain$Resample1, 1]) #In test data, the label is -1 & 1

# install.packages("kernlab")
library(kernlab)

cvControl <- trainControl(method = "cv",
                          number = 2,
                          classProbs = TRUE,
                          verboseIter = TRUE)
svmr_1 <- train(Choice ~ .,
                data = data[inTrain$Resample1, ],
                method = "svmRadial",
                metric = "ROC",
                tuneLength = 4,
                preProcess = c("center", "scale"),
                trControl = cvControl)

svmr_classes <- predict(svmr_1, newdata = data[-inTrain$Resample1, ])
confusionMatrix(svmr_classes, data[-inTrain$Resample1, 1])




