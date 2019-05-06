library(tidyverse)
library(caret)
#install.packages("DAAG")
library(DAAG)


mydat <- read.csv(here('data', 'Preprocessing_data_outputs', 'Paper', 'data_out.csv'))%>%
  filter(threshold == 250 & winds == 2)%>%
  select(-winds, - threshold, -index)
 dim(mydat) 

#mydat2 <- mydat[ complete.cases(mydat$SDNN) , ]

mydat1 <- mydat[ complete.cases(mydat) , ]
rmind <- which((mydat1$Stress>=5) & (mydat1$Y == "Control"))
mydat2 <- mydat1[-rmind, ]
dim(mydat2)
#142
str(mydat2)
table(mydat2$Y)
#    Control Episode 
#     0     110      19 

#creat within subject z scores
mydat3 <- mydat2
temp =  unique(mydat2$ID)
m = length(temp)
for (i in 1:m){
tempdat <- mydat2[which(mydat2$ID == temp[i]), ]	
mydat3[which(mydat2$ID == temp[i]), 6:15] <- scale(tempdat[ , 6:15])	
}
mydat3 <- mydat3[complete.cases(mydat3), ]
table(mydat3$Y)
#        Control Episode 
#      0      107      19

#dataset = mydat2
#folder = subdata$Fold3

library(e1071)
# make predictions
library(ROSE)
library(caret)
library(caretEnsemble)
#mydat2, subdata$Fold1
modelfit <- function(dataset, folder){
data <- dataset[-folder, ]
mytest <- dataset[folder, ] 
##Generate balanced dataset
train.m.bal<-ovun.sample(Y ~ ., data=data, method="both",p=0.6, seed=1342)$data
names(train.m.bal)
table(train.m.bal$Y)
xtrain <- train.m.bal[, 6:15]
xtrain <- as.matrix(xtrain)
ytrain <- train.m.bal$Y
#levels(ytrain) <- list(no="0", yes="1") 
#y.train <- as.numeric(train.m.bal$Y)
#y.train[which(train.m.bal$Y == "Control")] <- 0
#y.train[which(train.m.bal$Y == "Episode")] <- 1
table(ytrain) # 0 is control, 1 is episode
ytrain <- as.factor(ytrain)
xtrain <- data.frame(xtrain)
datatrain <- cbind(ytrain, xtrain)
#datatrain$ytrain <- as.factor(datatrain$ytrain)
#control <- trainControl(method="repeatedcv", number=5, repeats=3)
fit<- train(ytrain ~ ., data= datatrain,method="svmPoly")
svmFit <- train(ytrain ~ ., data=datatrain, method="svmPoly", trControl = trainControl( classProbs=T))

#svmFit <- train(ytrain ~ ., data=datatrain, method="svmPoly", trControl = trainControl(method = "repeatedcv", repeats=1, classProbs=T))


#fit2<- train(as.factor(y.train) ~ ., data= data.train,method="gbm")
#models <- list(svm =fit)
#predict(fit, newdata= x.test, type = "prob")
#varImp(fit)
xtest <- mytest[, 6:15]
xtest <- data.frame(xtest)
ytest <- as.numeric(mytest$Y)-1
table(ytest)
yhat6 = predict(fit, xtest)
table(yhat6)
yhat6 <- as.numeric(yhat6)
yhat6 <- as.factor(yhat6)
ytest <-as.factor(ytest)
temp <- caret::confusionMatrix(yhat6,ytest)


yhat62 = predict(svmFit, xtest)
table(yhat62)
yhat62 <- as.numeric(yhat62)
yhat62 <- as.factor(yhat62)
temp2 = caret::confusionMatrix(yhat62, ytest)

#predict(fit, testX = xtest, type="prob")

#predict(svmFit, testX = xtest, type="prob")
#extractProb(models, x.test)


#result <- c(temp$overall[1], temp$byClass[1:2]) #<-can change threshold if you want

result <- c(temp$overall[1], temp$byClass[1:2],temp2$overall[1], temp2$byClass[1:2]) #<-can change threshold if you want
return(result)
}

#make predictions
#involve random sampling, need to set the seed

set.seed(9)
nfolds=4
subdata<-createFolds(mydat3$Y, nfolds)
t1 <- modelfit(mydat3, subdata$Fold1)
t1
t2 <- modelfit(mydat3, subdata$Fold2)
t2
t3 <- modelfit(mydat3, subdata$Fold3)
t3
t4 <- modelfit(mydat3, subdata$Fold4)
t4
colMeans(rbind(t1,t2,t3,t4))



