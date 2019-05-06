library(tidyverse)
library(caret)
#install.packages("DAAG")
library(DAAG)


mydat = read.csv("data_out.csv")%>%
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
x.test <- mytest[, 6:15]
x.test <- as.matrix(x.test)
ytest <- as.numeric(mytest$Y)-1
table(ytest)
yhat6 = predict(fit, x.test)
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










#run permutation t test to examine these features between controls and episodes 
mydatC<-mydat2[ which(mydat2$Y == "Control"), ]
mydatE<-mydat2[ which(mydat2$Y == "Episode"), ]
dim(mydatC)
dim(mydatE)

twot.permutation(x1=mydatC$Stress, x2=mydatE$Stress, nsim=5000, plotit=T)
summary(mydatC$Stress)
summary(mydatE$Stress)


twot.permutation(x1=mydatC$SDNN[complete.cases(mydatC$SDNN)], x2=mydatE$SDNN[complete.cases(mydatE$SDNN)], nsim=5000, plotit=T )
summary(mydatC$SDNN[complete.cases(mydatC$SDNN)])
summary(mydatE$SDNN[complete.cases(mydatE$SDNN)])


twot.permutation(x1=mydatC$SDANN[complete.cases(mydatC$SDANN)], x2=mydatE$SDANN[complete.cases(mydatE$SDANN)], nsim=5000, plotit=T)
summary(mydatC$SDANN[complete.cases(mydatC$SDANN)])
summary(mydatE$SDANN[complete.cases(mydatE$SDANN)])

twot.permutation(x1=mydatC$SDNNIDX[complete.cases(mydatC$SDNNIDX)], x2=mydatE$SDNNIDX[complete.cases(mydatE$SDNNIDX)], nsim=5000, plotit=T)
summary(mydatC$SDNNIDX[complete.cases(mydatC$SDNNIDX)])
summary(mydatE$SDNNIDX[complete.cases(mydatE$SDNNIDX)])

twot.permutation(x1=mydatC$pNN50[complete.cases(mydatC$pNN50)], x2=mydatE$pNN50[complete.cases(mydatE$pNN50)], nsim=5000, plotit=T)
summary(mydatC$pNN50[complete.cases(mydatC$pNN50)])
summary(mydatE$pNN50[complete.cases(mydatE$pNN50)])

twot.permutation(x1=mydatC$SDSD[complete.cases(mydatC$SDSD)], x2=mydatE$SDSD[complete.cases(mydatE$SDSD)], nsim=5000, plotit=T)
summary(mydatC$SDSD[complete.cases(mydatC$SDSD)])
summary(mydatE$SDSD[complete.cases(mydatE$SDSD)])

twot.permutation(x1=mydatC$rMSSD[complete.cases(mydatC$rMSSD)], x2=mydatE$rMSSD[complete.cases(mydatE$rMSSD)], nsim=5000, plotit=T)
summary(mydatC$rMSSD[complete.cases(mydatC$rMSSD)])
summary(mydatE$rMSSD[complete.cases(mydatE$rMSSD)])
 
twot.permutation(x1=mydatC$IRRR[complete.cases(mydatC$IRRR)], x2=mydatE$IRRR[complete.cases(mydatE$IRRR)], nsim=5000, plotit=T)
summary(mydatC$IRRR[complete.cases(mydatC$IRRR)])
summary(mydatE$IRRR[complete.cases(mydatE$IRRR)])
 
twot.permutation(x1=mydatC$MADRR[complete.cases(mydatC$MADRR)], x2=mydatE$MADRR[complete.cases(mydatE$MADRR)], nsim=5000, plotit=T)
summary(mydatC$MADRR[complete.cases(mydatC$MADRR)])
summary(mydatE$MADRR[complete.cases(mydatE$MADRR)])
 
twot.permutation(x1=mydatC$TINN[complete.cases(mydatC$TINN)], x2=mydatE$TINN[complete.cases(mydatE$TINN)], nsim=5000, plotit=T)
summary(mydatC$TINN[complete.cases(mydatC$TINN)])
summary(mydatE$TINN[complete.cases(mydatE$TINN)])
  
twot.permutation(x1=mydatC$HRVi[complete.cases(mydatC$HRVi)], x2=mydatE$HRVi[complete.cases(mydatE$HRVi)], nsim=5000, plotit=T)
summary(mydatC$HRVi[complete.cases(mydatC$HRVi)])
summary(mydatE$HRVi[complete.cases(mydatE$HRVi)])
