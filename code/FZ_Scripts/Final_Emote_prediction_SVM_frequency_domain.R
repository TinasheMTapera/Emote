library(tidyverse)
library(caret)
#install.packages("DAAG")
library(DAAG)
library(here)

mydat = read.csv(here("DataOutputs", "data_out_20181113 copy.csv")) %>%
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
modelfit <- function(data, folder){
  
  mytrain <- data[-folder, ]
  mytest <- data[folder, ]
  ##Generate balanced dataset
  train.m.bal<-ovun.sample(Y ~ ., data=data, method="both",p=0.6, seed=1342)$data
  #names(train.m.bal)
  #table(train.m.bal$Y)
  # 6:15
  #[1] "ID"         "Stress"     "Y"          "Avg_niHR"   "Start_niHR"
  #[6] "End_niHR"   "Avg_HR"     "HF_1"       "HF_2"       "HF_3"      
  #[11] "HF_4"       "HF_5"       "HF_6"       "LF_1"       "LF_2"      
  #[16] "LF_3"       "LF_4"       "LF_5"       "LF_6"       "LFHF_1"    
  #[21] "LFHF_2"     "LFHF_3"     "LFHF_4"     "LFHF_5"     "LFHF_6"    
  # just take column 4 to column 19 
  x.train <- train.m.bal[, 4:19]
  x.train <- as.matrix(x.train)
  y.train <- train.m.bal$Y
  #y.train <- as.numeric(train.m.bal$Y)
  #y.train[which(train.m.bal$Y == "Control")] <- 0
  #y.train[which(train.m.bal$Y == "Episode")] <- 1
  print(table(y.train)) # 0 is control, 1 is episode
  data.train <- cbind(y.train, x.train)
  fit<- train(as.factor(y.train) ~ ., data= data.train,method="svmPoly")
  x.test <- mytest[, 4:19]
  x.test <- as.matrix(x.test)
  ytest <- as.factor(as.numeric(mytest$Y))
  print(table(ytest))
  yhat6 = predict(fit, x.test)
  #yhat6 <-as.numeric(yhat6)
  #yhat6 <- as.factor(yhat6)
  ytest <- as.factor(ytest)
  temp <- confusionMatrix(yhat6,ytest)
  
  result <- c(temp$overall[1], temp$byClass[1:2]) #<-can change threshold if you want
  
  var_imp <- filterVarImp(
    # get variable important; in this case, AUC analysis of a single predictor at a time
    x=as.data.frame(fit$finalModel@xmatrix), 
    y=fit$finalModel@ymatrix[fit$finalModel@alphaindex[[1]]]
  )
  
  result["ntrain"] = nrow(data.train)
  result["ntest"] = nrow(x.test)
  
  return(list(result=result, var_imp = var_imp))
}
#make predictions
#involve random sampling, need to set the seed
set.seed(9)
nfolds=4
subdata<-createFolds(mydat2$Y, nfolds)
t1_raw <- modelfit(mydat2, subdata$Fold1)
t1_raw
t2_raw <- modelfit(mydat2, subdata$Fold2)
t2_raw
t3_raw <- modelfit(mydat2, subdata$Fold3)
t3_raw
t4_raw <- modelfit(mydat2, subdata$Fold4)
t4_raw
colMeans(rbind(t1_raw$result,t2_raw$result,t3_raw$result,t4_raw$result))

set.seed(9)
nfolds=4
subdata<-createFolds(mydat3$Y, nfolds)
t1_z <- modelfit(mydat3, subdata$Fold1)
t1_z
t2_z <- modelfit(mydat3, subdata$Fold2)
t2_z
t3_z <- modelfit(mydat3, subdata$Fold3)
t3_z
t4_z <- modelfit(mydat3, subdata$Fold4)
t4_z
colMeans(rbind(t1_z$result,t2_z$result,t3_z$result,t4_z$result))

results_freq <- 
  rbind(
    t1_raw$result, 
    t2_raw$result,
    t3_raw$result,
    t4_raw$result,
    t1_z$result, 
    t2_z$result,
    t3_z$result,
    t4_z$result
  ) %>%
  data.frame() %>%
  as_tibble() %>%
  mutate(type = rep(c("raw", "z"), each=4), importance = list(
    rownames_to_column(t1_raw$var_imp),
    rownames_to_column(t2_raw$var_imp),
    rownames_to_column(t3_raw$var_imp),
    rownames_to_column(t4_raw$var_imp),
    rownames_to_column(t1_z$var_imp),
    rownames_to_column(t2_z$var_imp),
    rownames_to_column(t3_z$var_imp),
    rownames_to_column(t4_z$var_imp))
  )




