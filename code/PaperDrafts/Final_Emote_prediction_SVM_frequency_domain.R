library(tidyverse)
library(caret)
#install.packages("DAAG")
library(DAAG)
library(here)

mydat = read.csv(here('data', 'Preprocessing_data_outputs', 'Paper', 'data_out_20181113.csv')) %>%
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

leave_one_out_auc <- function(train, test){
  
  ret <- NULL
  
  for(col in 2:ncol(train)){
    
    fit <- train(Y ~ ., data = select(train, -col), method="svmPoly")
    yhat <- predict(fit, select(test, -col, -Y))
    auc <- auc(test$Y, yhat)
    var <- names(train)[col]
    ret <- rbind(ret, data.frame(auc,var))
  }
  
  return(ret)
}

modelfit <- function(dataset, folder){
  
  mytrain <- dataset[-folder, ]
  mytest <- dataset[folder, ]
  print(as_tibble(data))
  # generate a balanced training set
  train <- ovun.sample(
    Y ~ .,
    data=get("mytrain", sys.frame(1)),
    method = "both",
    p = 0.6,
    seed = 1342
  ) %>%
    .$data
  
  train <- train %>%
    as_tibble() %>%
    select(Y:LF_6) %>%
    droplevels()
  test <- mytest %>%
    as_tibble() %>%
    select(Y:LF_6) %>%
    droplevels()
  
  print("Training set:")
  print(train)
  print(table(train$Y))
  print("Testing set:")
  print(test)
  print(table(test$Y))
  
  # fit the model on the training set
  fit<- train(Y ~ ., data = train, method="svmPoly")
  
  # predict on the test set
  yhat = predict(fit, select(test, -Y))
  
  # evaluate test accuracy
  conf <- caret::confusionMatrix(yhat, test$Y)
  result <- c(conf$overall[1], conf$byClass[1:2]) #<-can change threshold if you want
  
  # get variable importance by looping over the columns, leave-one-out for each model
  importance <- leave_one_out_auc(train, test)
  result["ntrain"] <- nrow(train)
  result["ntest"] <- nrow(test)
  result["auc"] <- auc(test$Y, yhat)
  
  return(list(result=result, var_imp = importance))
}

#make predictions
#involve random sampling, need to set the seed
# set.seed(9)
# nfolds=4
# subdata<-createFolds(mydat2$Y, nfolds)
# t1_raw <- modelfit(mydat2, subdata$Fold1)
# t1_raw
# t2_raw <- modelfit(mydat2, subdata$Fold2)
# t2_raw
# t3_raw <- modelfit(mydat2, subdata$Fold3)
# t3_raw
# t4_raw <- modelfit(mydat2, subdata$Fold4)
# t4_raw
# colMeans(rbind(t1_raw$result,t2_raw$result,t3_raw$result,t4_raw$result))

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




