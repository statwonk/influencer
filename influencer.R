# Christopher Peters
# Influencer

setwd("C:/Users/Chris/Desktop/RCode/influencer/influencer/")
data <- read.csv("train.csv")
# test <- read.csv("test.csv")

set.seed(1)
train <- sample(1:length(data$Choice), 0.8 * length(data$Choice), replace = FALSE)

# install.packages("gbm")
library(gbm)

ada_1 <- gbm(Choice ~ ., distribution = "adaboost", 
             data = data[train, ], n.trees = 2000, 
             shrinkage = 0.005,
             interaction.depth = 6, train.fraction = 1, cv.folds = 4)


# install.packages("caret")
library(caret)

ada_2 <- train(data[train, -1], as.factor(data[train, 1]),
          method = "gbm", shrinkage = 0.005,
          interaction.depth = 6, train.fraction = 1, cv.folds = 4, 
          distribution = "adaboost")


set.seed(1)

# install.packages("doMC")

gbmGrid <- expand.grid(.interaction.depth = 4:6, 
                       .n.trees = c(500, 1000), 
                       .shrinkage = c(0.001, 0.01, 0.05))

seq(0.001, 0.05, by = 0.01)

# install.packages("lda")
library(lda)

bootControl <- trainControl(number = 3)
gbmFit <- train(data[train, -1], 
                as.factor(data[train, 1]), 
                method = "gbm", 
                trControl = trainControl(method = "cv", number = 3), 
                distribution = "adaboost",
                verbose = TRUE,
                bag.fraction = 0.5,
                train.fraction = 1,
                tuneGrid = gbmGrid)

cforest_1 <- train(as.factor(Choice) ~ ., data = data[train, ],
                  method = "cforest",
                  trControl = trainControl(method = "cv", number = 1),
                  verbose = TRUE,
                  bag.fraction = 0.5,
                  train.fraction = 1)
      
      


# install.packages("ada")
library(ada)



# install.packages("lars")
library(lars)

# lasso_1 <- lars(as.matrix(data[train, -1]), as.numeric(data[train, 1]), type = "lasso")
# 
# # install.packages("glmnet")
# library(glmnet)
# 
# glm_1 <- cv.glmnet(as.matrix(data[train, -1]), as.factor(data[train, 1]),
#        family = "binomial", type = "class")
# 
# mboost_1 <- mboost(as.factor(Choice) ~ ., baselearner = "btree", 
#                    data = data[train, ], family = Binomial(),
#                    control = boost_control(mstop = 1000))

# ada_2 <- ada(as.matrix(data[train, -1]), as.numeric(data[train, 1]), loss = "data",
#              type = "real", iter = 400, nu = 0.1, bag.frac = 0.5)

# # 0.8512
# ada_1 <- gbm(Choice ~ ., distribution = "adaboost", 
#              data = data[train, ], n.trees = 1000, interaction.depth = 8, train.fraction = 0.8, cv.folds = 5)

# # 0.8504
# ada_1 <- gbm(Choice ~ ., distribution = "adaboost", 
#              data = data[train, ], n.trees = 1000, interaction.depth = 8, train.fraction = 0.8, cv.folds = 9)


# pred.ada_1 <- predict(ada_1, newdata = data[-train, ], type="response")
# pred.ada_1 <- predict(glm_1, newx = as.matrix(data[-train, -1]), s = 0.1, type="response")
# pred.ada_1 <- as.numeric(predict(glm_1, as.matrix(data[-train, -1]), type="class", s=glm_1$lambda.1se))
# pred.ada_1 <- predict(lasso_1, newx = as.matrix(data[-train, -1]), s = 10, type="fit")
# pred.ada_1 <- as.numeric(predict(mboost_1, newdata = data[-train, ], type="class"))
# pred.ada_1 <- predict(ada_2, newdata = data[-train, ], type="vector")
preds <- predict(gbmFit$finalModel, newdata = data[-train, ], type = "response", 
                 n.trees = 200)

pred.label <- 1*(preds < mean(preds)) #1: > 0; 0: otherwise

table(pred.label, data[-train , 1]) #In test data, the label is -1 & 1


# AUC 
ROC <-function(res){
  # 1st column is class has 0 and 1 only
  # 2nd colum is their scores
  ord<-order(res[,2],decreasing=T)
  score<-res[ord,2]
  class<-res[ord,1]
  temp1<-unique(score)
  n2<-length(temp1)
  n<-length(class)
  class0<-which(class==0)
  class1<-which(class==1)
  n1<-length(class1)
  n0<-length(class0)
  Sen<-rep(0,(n2+1)) #Sensitivity
  Spe<-rep(1,(n2+1)) #Specificity
  for (i in 1:n2){
    tmp1<-which(score>=temp1[i])
    tmp2<-setdiff(1:n,tmp1)
    Sen[(i+1)]<-length(intersect(tmp1,class1))/n1
    Spe[(i+1)]<-length(intersect(tmp2,class0))/n0
  }
  out<-data.frame(Sen=Sen,Spe=Spe)
  out
}

ROC.score <- function(Sen,Spe){
  n<-length(Sen)-1
  tmp1<-1-Spe
  tmp2<-diff(tmp1,lag=1)
  tmp3<-rep(0,n)
  for (i in 1:n){ tmp3[i]<-(Sen[i]+Sen[(i+1)])/2 }
  out<-tmp3%*%tmp2
  out
}

temp <- ROC(cbind(data[-train, 1], pred.label))

ROC.score(temp$Sen,temp$Spe) #AUC score

plot(1-temp$Spe,temp$Sen,ylab="Sensitivity",xlab="1-Specificity",
     type="l",lwd=2,lty=1)
lines(c(0,1),c(0,1))



