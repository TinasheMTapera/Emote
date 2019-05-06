library(tidyverse)
library(caret)
#install.packages("DAAG")
library(DAAG)


mydat = read.csv("data_out_20181113.csv")%>%
  filter(threshold == 100 & winds == 2)%>%
  select(-winds, - threshold, -index)
 dim(mydat) 
ind <- c(1, 4, 5, 16:37)
mydat_freq <- mydat[, ind]
#mydat2 <- mydat[ complete.cases(mydat$SDNN) , ]

mydat1 <- mydat_freq[ complete.cases(mydat_freq) , ]
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
mydat3[which(mydat2$ID == temp[i]), 4:25] <- scale(tempdat[ , 4:25])	
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
#mydat2, subdata$Fold1
modelfit <- function(dataset, folder){
mytrain <- dataset[-folder, ]
mytest <- dataset[folder, ] 
##Generate balanced dataset
train.m.bal<-ovun.sample(Y ~ ., data=get("mytrain", sys.frame(1)), method="both",p=0.6, seed=1342)$data
names(train.m.bal)
table(train.m.bal$Y)
x.train <- train.m.bal[, 6:15]
x.train <- as.matrix(x.train)
y.train <- train.m.bal$Y
#y.train <- as.numeric(train.m.bal$Y)
#y.train[which(train.m.bal$Y == "Control")] <- 0
#y.train[which(train.m.bal$Y == "Episode")] <- 1
table(y.train) # 0 is control, 1 is episode
data.train <- cbind(y.train, x.train)
fit<- train(as.factor(y.train) ~ ., data= data.train,method="svmPoly")
x.test <- mytest[, 4:25]
x.test <- as.matrix(x.test)
ytest <- as.numeric(mytest$Y)-1
table(ytest)
yhat6 = predict(fit, x.test)
yhat6 <-as.numeric(yhat6)-1
yhat6 <- as.factor(yhat6)
ytest <- as.factor(ytest)
temp <- confusionMatrix(yhat6,ytest)

result <- c(temp$overall[1], temp$byClass[1:2]) #<-can change threshold if you want
return(result)
}

#make predictions
#involve random sampling, need to set the seed
set.seed(9)
nfolds=4
subdata<-createFolds(mydat2$Y, nfolds)
t1 <- modelfit(mydat2, subdata$Fold1)
t1
t2 <- modelfit(mydat2, subdata$Fold2)
t2
t3 <- modelfit(mydat2, subdata$Fold3)
t3
t4 <- modelfit(mydat2, subdata$Fold4)
t4
colMeans(rbind(t1,t2,t3,t4))

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






