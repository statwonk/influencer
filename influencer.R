# Christopher Peters
# Influencer

# setwd("C:/Users/Chris/Desktop/RCode/influencer/influencer/")
setwd("C:/R_stuff/influencer/influencer")
data <- read.csv("train.csv")
# test <- read.csv("test.csv")

pairs(data[ , 1:5])

set.seed(1)
train <- sample(1:length(data$Choice), 0.8 * length(data$Choice), replace = FALSE)

# install.packages("gbm")
library(gbm)

ada_1 <- gbm(Choice ~ ., distribution = "adaboost", 
             data = data[train, ], n.trees = 2000, 
             shrinkage = 0.005,
             interaction.depth = 6, train.fraction = 1, cv.folds = 4)


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




