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
             data = data[train, ], n.trees = 2000, interaction.depth = 6, train.fraction = 1, cv.folds = 5)

# # 0.8512
# ada_1 <- gbm(Choice ~ ., distribution = "adaboost", 
#              data = data[train, ], n.trees = 1000, interaction.depth = 8, train.fraction = 0.8, cv.folds = 5)

# # 0.8504
# ada_1 <- gbm(Choice ~ ., distribution = "adaboost", 
#              data = data[train, ], n.trees = 1000, interaction.depth = 8, train.fraction = 0.8, cv.folds = 9)


pred.ada_1 <- predict(ada_1, newdata = data[-train, ], type="response")
pred.label <- 1*(pred.ada_1 > 0) #1: > 0; 0: otherwise

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

temp <- ROC(cbind(data[-train, 1], pred.ada_1))

ROC.score(temp$Sen,temp$Spe) #AUC score

plot(1-temp$Spe,temp$Sen,ylab="Sensitivity",xlab="1-Specificity",
     type="l",lwd=2,lty=1)
lines(c(0,1),c(0,1))



