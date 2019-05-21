## ----packages and setup, include=FALSE, warning=FALSE--------------------
knitr::opts_chunk$set(fig.width=9, fig.height=6) 
suppressPackageStartupMessages(
  {
    library(tidyverse, quietly = TRUE)
    library(knitr, quietly = TRUE)
    library(DAAG, quietly = TRUE)
    library(here, quietly = TRUE)
    library(naniar, quietly = TRUE)
    library(kableExtra, quietly = TRUE)
    library(ggsignif, quietly = TRUE)
    library(viridis, quietly = TRUE)
    library(forcats, quietly = TRUE)
    library(caret, quietly = TRUE)
    library(ROSE, quietly = TRUE)
    library(e1071, quietly = TRUE)
    library(ModelMetrics, quietly = TRUE)
    library(scales, quietly = TRUE)
  }
)

p_format <- function(vec, thresh = 0.05, star1 = 0.05, star2 = 0.01, star3 = 0.001){
  
  vec2 = NULL
  vec2 <- sapply(vec, formatC)
  
  for(x in 1:length(vec)){
    if(vec[x] <= star3){
      vec2[x] = "< 0.001 ***"
    }
    else if(vec[x] <= star2){
      vec2[x] = "< 0.01 **"
    }
    else if(vec[x] <= star1){
      vec2[x] = "< 0.05 *"
    }
  }
  vec2
}

permutation_plot <- function(df, height = 1.2, tip = 0.1){
  
  n <- df %>%
    rowid_to_column() %>%
    group_by(variable) %>%
    arrange(-Mean) %>%
    slice(1) %>%
    pull(rowid)
  y_position <- df$Mean[n] * height
  
  plt  <- df %>% 
    mutate(Feature = variable) %>%
    {
      ggplot(., aes(x = Feature, y = Mean, fill = Group)) +
        geom_col(position = "dodge") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=17, family = "Times")) + 
        geom_signif(family = "Times", annotations = sapply(.$p.val[n], p_format),
                    xmin = seq(0.8, length.out = length(n)),
                    xmax = seq(1.2, length.out = length(n)),
                    y_position = y_position,
                    tip_length=tip
                    ) +
        scale_fill_viridis_d(labels = c("Control", "Episode"))
  }
  plt
}

set.seed(1342)


## ----load data, include=FALSE, warning=FALSE, message=FALSE--------------
# all initial surveys
all_surveys <- read_csv(here('data','Preprocessing_data_outputs', 'UpdatedSurveys_180713.csv')) %>%
  filter(complete.cases(.)) %>%
  mutate(Y = ifelse(EmotionalEatingEpisodes == 2, 'Control', 'Episode'))

# data used for frequency domain analysis
df_freq <- read_csv(here('data', 'Preprocessing_data_outputs', 'Paper', 'data_out_20181113.csv'))

# data used for time domain analysis
df_time <- read_csv(here('data', 'Preprocessing_data_outputs', 'Paper', 'data_out.csv'))


## ----source-missingness, echo = FALSE------------------------------------
df_time_nrow <- df_time %>% 
  # use params threshold = 250, windows = 2
  filter(threshold == 250 & winds == 2) %>%
  # remove high stress controls
  filter(!(Stress >= 5 & Y == "Control")) %>%
  select(-c(winds, threshold, index, when, Event, Stress, ID), Y, SDNN:HRVi) %>%
  group_by(Y) %>%
  count()

df_freq_nrow <- df_freq %>%
  # use params threshold = 100, windows = 2
  filter(threshold == 100 & winds == 2) %>%
  # remove high stress controls
  filter(!(Stress >= 5 & Y == "Control")) %>%
  select(-c(winds, threshold, index, when, Event, Stress, ID), Y, SDNN:HRVi) %>%
  group_by(Y) %>%
  count()


## ----missingness-time, echo = FALSE, fig.cap='(ref:missingness-time)'----
df_time %>% 
  # use params threshold = 250, windows = 2
  filter(threshold == 250 & winds == 2) %>%
  # remove high stress controls
  filter(!(Stress >= 5 & Y == "Control")) %>%
  select(-c(winds, threshold, index, when, Event, Stress, ID, Y), SDNN:HRVi) %>%
  vis_miss(.) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    text = element_text(size=15, family = "Times")
    )


## ----missingness-freq, fig.cap='(ref:missingness-freq)', echo = FALSE----
df_freq %>% 
  # use params threshold = 100, windows = 2
  filter(threshold == 100 & winds == 2) %>%
  # remove high stress controls
  filter(!(Stress >= 5 & Y == "Control")) %>%
  select(-c(winds, threshold, index, when, Event, Stress, ID, Y)) %>%
  select(Avg_niHR:LFHF_6) %>%
  vis_miss(.) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    text = element_text(size=15, family = "Times")
    )


## ----missingness-tab, echo = FALSE, warning = FALSE, message = FALSE, results='asis'----
missingnesstable_time <- df_time %>% 
  # use params threshold = 250, windows = 2
  filter(threshold == 250 & winds == 2) %>%
  # remove high stress controls
  filter(!(Stress >= 5 & Y == "Control")) %>%
  select(-c(winds, threshold, index, when, Event, Stress, ID), Y, SDNN:HRVi) %>%
  group_by(Y) %>%
  summarise_all(funs(sum(!is.na(.)))) %>%
  t()

colnames(missingnesstable_time) <- missingnesstable_time[1,]
missingnesstable_time <- missingnesstable_time[-1,] %>%
  data.frame() %>%
  rownames_to_column(var = "Variable")

missingnesstable_freq <- df_freq %>% 
  # use params threshold = 250, windows = 2
  filter(threshold == 100 & winds == 2) %>%
  # remove high stress controls
  filter(!(Stress >= 5 & Y == "Control")) %>%
  # select frequency domain
  select(-c(winds, threshold, index, when, Event, Stress, ID)) %>%
  select(Y, Avg_niHR:LFHF_6) %>%
  group_by(Y) %>%
  summarise_all(funs(sum(!is.na(.)))) %>%
  t()

colnames(missingnesstable_freq) <- missingnesstable_freq[1,]
missingnesstable_freq <- missingnesstable_freq[-1,] %>%
  data.frame() %>%
  rownames_to_column(var = "Variable")

bind_rows(missingnesstable_time, missingnesstable_freq)%>%
  kable(caption = '(ref:missingness-tab)') %>%
  kable_styling() %>%
  kableExtra::group_rows("Heart Rate Variability", 1, 10) %>%
  kableExtra::group_rows("Heart Rate", 11, 14) %>%
  kableExtra::group_rows("High Frequency", 15, 20) %>%
  kableExtra::group_rows("Low Frequency", 21, 26) %>%
  kableExtra::group_rows("LF-HF Ratio", 27, 32)


## ----source-tests, echo = FALSE, warning = FALSE, message = FALSE, cache = TRUE----
df_time_summaries <- df_time %>% 
  # use params threshold = 250, windows = 2
  filter(threshold == 250 & winds == 2) %>%
  # remove high stress controls
  filter(!(Stress >= 5 & Y == "Control")) %>%
  # select time domain
  select(-c(winds, threshold, index, when, Event, Stress, ID), Y, SDNN:HRVi) %>%
  # use summarise_at to create within variable summaries
  summarise_at(
    .funs = funs(
      
      # for each variable, count non-NA
      n = length(.[complete.cases(.)]),
      n.control = length(.[complete.cases(.) & Y == "Control"]),
      n.episode = length(.[complete.cases(.) & Y != "Control"]),
      
      # for each variable, calculate the mean, removing NA in each
      mean = mean(., na.rm = TRUE),
      mean.control = mean(.[Y == "Control"], na.rm = TRUE),
      mean.episode = mean(.[Y != "Control"], na.rm = TRUE),
      
      # for each variable, calculate the sd, removing NA in each
      sd = sd(., na.rm = TRUE),
      sd.control = sd(.[Y == "Control"], na.rm = TRUE),
      sd.episode = sd(.[Y != "Control"], na.rm = TRUE),
      
      # for each variable, run TwoT perm between control and episode, removing NA in each
      p.val = twotPermutation(.[Y == "Control" & !is.na(.)],
                              .[Y != "Control" & !is.na(.)],
                              nsim = 10000, plotit = F)
      ),
    .vars = vars(SDNN:HRVi)
    ) %>% 
  gather(key, value) %>%
  separate(key, c("variable", "statistic"), sep = "_")

df_freq_summaries <- df_freq %>% 
  # use params threshold = 250, windows = 2
  filter(threshold == 100 & winds == 2) %>%
  # remove high stress controls
  filter(!(Stress >= 5 & Y == "Control")) %>%
  # select frequency domain
  select(-c(winds, threshold, index, when, Event, Stress, ID)) %>%
  select(Y, Avg_niHR:LFHF_6) %>%
  
  # truncate frequency power bands
  mutate_at(., .funs = funs(ifelse(. > 3630, 3630, 
                                   ifelse(. < 80, 80, .))),
            .vars = vars(matches("^HF_.*")))%>%
  mutate_at(., .funs = funs(ifelse(. > 1010, 101,
                                   ifelse(. < 190, 190, .))), 
            .vars = vars(matches("^LF_.*")))%>%
  mutate_at(., .funs = funs(ifelse(. > 12, 12,
                                   ifelse(. < 1, 1, .))), 
            .vars = vars(matches("LFHF_.*"))) %>%
  
  # use summarise_at to create within variable summaries
  summarise_at(
    .funs = funs(
      
      # for each variable, count non-NA
      n = length(.[complete.cases(.)]),
      n.control = length(.[complete.cases(.) & Y == "Control"]),
      n.episode = length(.[complete.cases(.) & Y != "Control"]),
      
      # for each variable, calculate the mean, removing NA in each
      mean = mean(., na.rm = TRUE),
      mean.control = mean(.[Y == "Control"], na.rm = TRUE),
      mean.episode = mean(.[Y != "Control"], na.rm = TRUE),
      
      # for each variable, calculate the sd, removing NA in each
      sd = sd(., na.rm = TRUE),
      sd.control = sd(.[Y == "Control"], na.rm = TRUE),
      sd.episode = sd(.[Y != "Control"], na.rm = TRUE),
      
      # for each variable, run TwoT perm between control and episode, removing NA in each
      p.val = twotPermutation(.[Y == "Control" & !is.na(.)],
                              .[Y != "Control" & !is.na(.)],
                              nsim = 10000, plotit = F)
      ),
    .vars = vars(Avg_niHR:LFHF_6)
    ) %>% 
  gather(key, value) %>%
  separate(key, c("variable", "statistic"), sep = "_(?!.*_)")



## ----timeSummary, echo = FALSE, warning = FALSE, message = FALSE, results='asis'----

df_time_summaries %>%
  filter(statistic != "p.val") %>%
  filter(!str_detect(statistic, "control|episode")) %>%
  spread(key = statistic, value = value) %>%
  select(variable, n, mean, sd) %>% 
  kable(caption = '(ref:timeSummary)') %>%
  kable_styling()
  


## ----timeComparison, echo = FALSE, warning = FALSE, message = FALSE------
df_time_summaries %>%
  filter(str_detect(statistic, "control|episode|p.val")) %>%
  spread(key = statistic, value = value) %>%
  select(-p.val, everything()) %>%
  kable(digits=2, caption = '(ref:timeComparison)') %>%
  kable_styling()


## ----timeComparisonPlot, echo = FALSE, warning = FALSE, message = FALSE, fig.cap = '(ref:timeComparisonPlot)'----
df_time_summaries %>%
  filter(str_detect(statistic, "control|episode|p.val")) %>%
  spread(key = statistic, value = value) %>%
  select(-p.val, everything()) %>%
  gather("Group", "Mean", mean.control:mean.episode) %>%
  permutation_plot(., tip = 0.01)+
    labs(caption = expression(~italic(p)~"-values for permutation "~italic(t)~"-test with 10,000 permutations shown"))


## ----freqSummary, echo = FALSE, warning = FALSE, message = FALSE---------
order = c("Avg_HR", "Avg_niHR", "Start_niHR", "End_niHR",
          paste0("HF_", 1:6),
          paste0("LF_", 1:6),
          paste0("LFHF_", 1:6))
df_freq_summaries %>%
  filter(statistic != "p.val") %>%
  filter(!str_detect(statistic, "control|episode")) %>%
  spread(key = statistic, value = value) %>%
  select(variable, n, mean, sd) %>%
  slice(match(order, variable)) %>%
  kable(digits=2, caption = '(ref:freqSummary)') %>%
  kable_styling() %>%
  kableExtra::group_rows("Heart Rate", 1, 4) %>%
  kableExtra::group_rows("High Frequency", 5, 10) %>%
  kableExtra::group_rows("Low Frequency", 11, 16) %>%
  kableExtra::group_rows("LF-HF Ratio", 17, 22)


## ----freqComparison, echo = FALSE, warning = FALSE, message = FALSE------
df_freq_summaries %>%
  filter(str_detect(statistic, "control|episode|p.val")) %>%
  spread(key = statistic, value = value) %>%
  select(-p.val, everything()) %>%
  slice(match(order, variable)) %>%
  kable(digits=2, caption = '(ref:freqComparison)') %>%
  kable_styling() %>%
  kableExtra::group_rows("Heart Rate", 1, 4) %>%
  kableExtra::group_rows("High Frequency", 5, 10) %>%
  kableExtra::group_rows("Low Frequency", 11, 16) %>%
  kableExtra::group_rows("LF-HF Ratio", 17, 22)


## ----freqComparisonPlot, echo = FALSE, warning = FALSE, message = FALSE, fig.cap = '(ref:freqComparisonPlot)'----

faceted <- df_freq_summaries %>%
  filter(str_detect(statistic, "control|episode|p.val")) %>%
  spread(key = statistic, value = value) %>%
  select(-p.val, everything()) %>%
  gather("Group", "Mean", mean.control:mean.episode) %>%
  mutate(facet = case_when(
    str_detect(variable, "^HF_.*") ~ "High_Frequency",
    str_detect(variable, "^LF_.*") ~ "Low_Frequency",
    str_detect(variable, "^LFHF_.*") ~ "LF-HF Ratio",
    str_detect(variable, "HR") ~ "Heart Rate"
    )
  ) %>%
  nest(-facet)
plots <- pmap(list(df = faceted$data, 
             height = c(1.1, 1.1, 1.1, 1.1),
             tip = c(0.3, 0.05, 0.1, 0.08)),
        permutation_plot)

#ggpubr::ggarrange(plotlist = plots, common.legend = T, font.label = list(face = 'bold', family = 'Times'), labels = paste0(letters[1:4], ")"))


## ----freqComparisonPlotA, echo = FALSE, warning = FALSE, message = FALSE, fig.cap = '(ref:freqComparisonPlotA)'----
df_freq_summaries %>%
  filter(str_detect(statistic, "control|episode|p.val")) %>%
  spread(key = statistic, value = value) %>%
  select(-p.val, everything()) %>%
  gather("Group", "Mean", mean.control:mean.episode) %>%
  mutate(facet = case_when(
    str_detect(variable, "^HF_.*") ~ "High_Frequency",
    str_detect(variable, "^LF_.*") ~ "Low_Frequency",
    str_detect(variable, "^LFHF_.*") ~ "LF-HF Ratio",
    str_detect(variable, "HR") ~ "Heart Rate"
    )
  ) %>%
  filter(facet == "Heart Rate") %>%
  permutation_plot(., tip = 0.3, height = 1.1) +
      labs(caption = expression(~italic(p)~"-values for permutation "~italic(t)~"-test with 10,000 permutations shown")) +
  ylab(expression(paste("Mean Beats/min")))



## ----freqComparisonPlotD, echo = FALSE, warning = FALSE, message = FALSE, fig.cap = '(ref:freqComparisonPlotD)'----
df_freq_summaries %>%
  filter(str_detect(statistic, "control|episode|p.val")) %>%
  spread(key = statistic, value = value) %>%
  select(-p.val, everything()) %>%
  gather("Group", "Mean", mean.control:mean.episode) %>%
  mutate(facet = case_when(
    str_detect(variable, "^HF_.*") ~ "High_Frequency",
    str_detect(variable, "^LF_.*") ~ "Low_Frequency",
    str_detect(variable, "^LFHF_.*") ~ "LF-HF Ratio",
    str_detect(variable, "HR") ~ "Heart Rate"
    )
  ) %>%
  filter(facet == "LF-HF Ratio") %>%
  permutation_plot(., tip = 0.05, height = 1.1)+
      labs(caption = expression(~italic(p)~"-values for permutation "~italic(t)~"-test with 10,000 permutations shown")) +
  ylab(expression(paste("Mean ", ms^{2})))



## ----ml-functions, echo = FALSE, warning = FALSE, message = FALSE, cache = TRUE----
modelfit <- function(data){
  
  train <- data$train
  test <- data$test
  # fit the model on the training set
  fit <- train(Y ~ ., data = train, method="svmPoly")
  
  # predict on the test set
  yhat = predict(fit, newdata = select(test, -Y))
  
  # evaluate test accuracy
  conf <- caret::confusionMatrix(yhat, test$Y)
  result <- c(conf$overall[1], conf$byClass[1:2]) #<-can change threshold if you want
  result["auc"] <- auc(test$Y, yhat)
  
  return(result)
  
}

perturb_model_fit <- function(data){
  
  train <- data$train
  test <- data$test
  
  # fit the model on the training set
  invisible(capture.output(fit <- train(Y ~ ., data = train, method="svmPoly", trControl = trainControl(classProbs = TRUE))))
  # get these fitted probabilities
  y = predict(fit, newdata = select(train, -Y), type = "prob") %>%
    rownames_to_column()
  
  # perturb a column from the training set and get the new probabilities
  result <- list(Control = y)
  
  for(c in 2:ncol(train)){
    
    col <- names(train)[c]
    temp <- train
    temp[, col] <- temp[, col] + 1
    result[[col]] <- predict(fit, newdata = select(temp, -Y), type = "prob") %>%
      rownames_to_column()
    
  }
  
  # tidy
  bound <- bind_rows(result, .id='Perturbed_Feature') %>%
    mutate(rowname = as.numeric(rowname)) %>%
    select(-Control) %>% 
    spread(key = "Perturbed_Feature", value = Episode) %>%
    as_tibble() %>%
    bind_cols(Y = train$Y)
  
  return(bound)
  
}

train_test <- function(dataset, folder, print = FALSE){
  
  # issue here with namespace and "data" variable reference
  data <- dataset[-folder, ]
  mytest <- dataset[folder, ]
  
  # generate a balanced training set
  train <- ovun.sample(
    Y ~ .,
    data=data,
    method = "both",
    p = 0.6,
    seed = 1342
  )$data
  
  train <- as_tibble(train) %>%
    droplevels()
  test <- as_tibble(mytest) %>%
    droplevels()
  
  if(print){
    print("Training set:")
    print(train)
    print(table(train$Y))
    print("Testing set:")
    print(test)
    print(table(test$Y))
  }
  
  return(list(train = train, test = test))
  
}

# get variable importance by looping over the columns, leave-one-out for each model
leave_one_out_auc <- function(df, col){
  
  df_reduced <- df %>%
    select(-!!col)
  set.seed(9)
  nfolds <- 4
  subdata <- createFolds(df_reduced$Y, nfolds)
  
  temp1 <- train_test(df_reduced, subdata$Fold1) %>%
    modelfit()
  temp2 <- train_test(df_reduced, subdata$Fold2) %>%
    modelfit()
  temp3 <- train_test(df_reduced, subdata$Fold3) %>%
    modelfit()
  temp4 <- train_test(df_reduced, subdata$Fold4)%>%
    modelfit()

    
  auc <- colMeans(rbind(temp1['auc'],temp2['auc'], temp3['auc'], temp4['auc'])) %>%
    t() %>%
    data.frame(auc = ., variable = names(df)[col])
  return(auc)
}


## ----ml-datasets, echo = FALSE, warning = FALSE, message = FALSE, cache = TRUE----
df_time_ml <- read.csv(here('data', 'Preprocessing_data_outputs', 'Paper', 'data_out.csv'))%>%
  # use params threshold = 250, windows = 2
  filter(threshold == 250 & winds == 2)%>%
  select(-winds, - threshold, -index) %>%
  filter(complete.cases(.)) %>%
  filter(!((Stress>=5) & (Y == "Control")))

df_time_ml <- df_time_ml %>%
  mutate(ID = as.factor(ID)) %>%
  group_by(ID) %>%
  mutate_at(vars(SDNN:HRVi), scale) %>%
  ungroup() %>%
  select(Y:HRVi) %>%
  filter(complete.cases(.))

df_freq_ml <- read.csv(here('data', 'Preprocessing_data_outputs', 'Paper', 'data_out_20181113.csv'))%>%
  # use params threshold = 250, windows = 2
  select(-c(winds, threshold, index, when, Event)) %>%
  select(Y, ID, Stress, Avg_niHR:LF_6) %>%
  filter(complete.cases(.)) %>%
  filter(!(Stress >= 5 & Y == "Control"))
  

df_freq_ml <- df_freq_ml %>%
  mutate(ID = as.factor(ID)) %>%
  group_by(ID) %>%
  mutate_at(vars(Avg_niHR:LF_6), scale) %>%
  ungroup() %>%
  select(Y, Avg_niHR:LF_6) %>%
  filter(complete.cases(.))


## ----ml-time, echo = FALSE, warning = FALSE, message = FALSE, cache = TRUE----
set.seed(9)
nfolds <- 4
subdata<-createFolds(df_time_ml$Y, nfolds)
t1_z <- train_test(df_time_ml, subdata$Fold1) %>%
  modelfit()
t2_z <- train_test(df_time_ml, subdata$Fold2) %>%
  modelfit()
t3_z <- train_test(df_time_ml, subdata$Fold3) %>%
  modelfit()
t4_z <- train_test(df_time_ml, subdata$Fold4) %>%
  modelfit()
results_time <- colMeans(rbind(t1_z, t2_z, t3_z, t4_z))


## ----auc-importance-time, echo = FALSE, warning = FALSE, message = FALSE, cache = TRUE----
auc_comparison_time <- list()

for(c in 2:ncol(df_time_ml)){
  auc_comparison_time <- leave_one_out_auc(df_time_ml, c) %>%
    rbind(auc_comparison_time)
}

auc_time <- auc_comparison_time %>%
  mutate(Reduction_AUC = results_time['auc'] - auc) %>%
  mutate(Reduction_AUC = rescale(Reduction_AUC, c(0,1))) %>%
  arrange(-Reduction_AUC)


## ----perturbations-time, echo = FALSE, warning = FALSE, message = FALSE, cache = TRUE----
set.seed(9)
nfolds <- 4
subdata<-createFolds(df_time_ml$Y, nfolds)

perturbed_time_1 <- train_test(df_time_ml, subdata$Fold1) %>%     # split train and test
  perturb_model_fit() %>%                                   # fit model and perturb each column
  mutate_at(                                                # tidy to get differences
    .vars = vars(-rowname, -Y, -Control),
    .funs = list(Delta = ~. - Control))# %>%
  #select(Y, contains("Delta"))

perturbed_time_2 <- train_test(df_time_ml, subdata$Fold2) %>%     # split train and test
  perturb_model_fit() %>%                                   # fit model and perturb each column
  mutate_at(                                                # tidy to get differences
    .vars = vars(-rowname, -Y, -Control),
    .funs = list(Delta = ~. - Control)) #%>%
  #select(Y, contains("Delta"))

perturbed_time_3 <- train_test(df_time_ml, subdata$Fold3) %>%     # split train and test
  perturb_model_fit() %>%                                   # fit model and perturb each column
  mutate_at(                                                # tidy to get differences
    .vars = vars(-rowname, -Y, -Control),
    .funs = list(Delta = ~. - Control)) #%>%
  #select(Y, contains("Delta"))

perturbed_time_4 <- train_test(df_time_ml, subdata$Fold4) %>%     # split train and test
  perturb_model_fit() %>%                                   # fit model and perturb each column
  mutate_at(                                                # tidy to get differences
    .vars = vars(-rowname, -Y, -Control),
    .funs = list(Delta = ~. - Control)) #%>%
  #select(Y, contains("Delta"))



## ----ml-freq, echo = FALSE, warning = FALSE, message = FALSE, cache = TRUE----
set.seed(9)
nfolds <- 4
subdata<-createFolds(df_freq_ml$Y, nfolds)
f1_z <- train_test(df_freq_ml, subdata$Fold1) %>%
  modelfit()
f2_z <- train_test(df_freq_ml, subdata$Fold2) %>%
  modelfit()
f3_z <- train_test(df_freq_ml, subdata$Fold3) %>%
  modelfit()
f4_z <- train_test(df_freq_ml, subdata$Fold4) %>%
  modelfit()
results_freq <- colMeans(rbind(f1_z, f2_z, f3_z, f4_z))


## ----auc-importance-freq, echo = FALSE, warning = FALSE, message = FALSE, cache = TRUE----
auc_comparison_freq <- list()

for(c in 2:ncol(df_freq_ml)){
  auc_comparison_freq <- leave_one_out_auc(df_freq_ml, c) %>%
    rbind(auc_comparison_freq)
}

auc_freq <- auc_comparison_freq %>%
  mutate(Reduction_AUC = results_freq['auc'] - auc) %>%
  mutate(Reduction_AUC = rescale(Reduction_AUC, c(0,1))) %>%
  arrange(-Reduction_AUC)


## ----perturbations-freq, echo = FALSE, warning = FALSE, message = FALSE, cache = TRUE----
set.seed(9)
nfolds <- 4
subdata<-createFolds(df_freq_ml$Y, nfolds)

perturbed_freq_1 <- train_test(df_freq_ml, subdata$Fold1) %>%     # split train and test
  perturb_model_fit() %>%                                   # fit model and perturb each column
  mutate_at(                                                # tidy to get differences
    .vars = vars(-rowname, -Y, -Control),
    .funs = list(Delta = ~. - Control)) #%>%
  #select(Y, contains("Delta"))

perturbed_freq_2 <- train_test(df_freq_ml, subdata$Fold2) %>%     # split train and test
  perturb_model_fit() %>%                                   # fit model and perturb each column
  mutate_at(                                                # tidy to get differences
    .vars = vars(-rowname, -Y, -Control),
    .funs = list(Delta = ~. - Control)) #%>%
  #select(Y, contains("Delta"))

perturbed_freq_3 <- train_test(df_freq_ml, subdata$Fold3) %>%     # split train and test
  perturb_model_fit() %>%                                   # fit model and perturb each column
  mutate_at(                                                # tidy to get differences
    .vars = vars(-rowname, -Y, -Control),
    .funs = list(Delta = ~. - Control)) #%>%
  #select(Y, contains("Delta"))

perturbed_freq_4 <- train_test(df_freq_ml, subdata$Fold4) %>%     # split train and test
  perturb_model_fit() %>%                                   # fit model and perturb each column
  mutate_at(                                                # tidy to get differences
    .vars = vars(-rowname, -Y, -Control),
    .funs = list(Delta = ~. - Control)) #%>%
  #select(Y, contains("Delta"))



## ----gather-importance, echo = FALSE, warning = FALSE, message = FALSE----
time_domain_deltas <- bind_rows(perturbed_time_1, perturbed_time_2, perturbed_time_3, perturbed_time_4) %>%
  group_by(Y) %>%
  summarise_if(is.numeric, mean) %>%
  select(-rowname) %>%
  select(Y, contains("_Delta")) %>%
  gather("variable", "Delta_Probability", -Y) %>%
  mutate(variable = str_replace(variable, pattern = "_Delta", replacement = ""))

freq_domain_deltas <- bind_rows(perturbed_freq_1, perturbed_freq_2, perturbed_freq_3, perturbed_freq_4) %>%
  group_by(Y) %>%
  summarise_if(is.numeric, mean) %>%
  select(-rowname) %>%
  select(Y, contains("_Delta")) %>%
  gather("variable", "Delta_Probability", -Y) %>%
  mutate(variable = str_replace(variable, pattern = "_Delta", replacement = ""))


## ----ml-metrics, echo = FALSE, warning = FALSE, message = FALSE, fig.cap = '(ref:ml-metrics)'----
ml_time <- results_time %>%
  enframe(name = "Metric", value = "Value") %>%
  mutate(Metric = ifelse(Metric == "auc", "AUC", Metric),
         Domain = "Time")

ml_freq <- results_freq %>%
  enframe(name = "Metric", value = "Value") %>%
  mutate(Metric = ifelse(Metric == "auc", "AUC", Metric),
         Domain = "Frequency")

bind_rows(ml_time, ml_freq) %>%
  ggplot(aes(x=Metric, y=Value, fill=Domain))+
  geom_col(position = "dodge") +
  geom_text(aes(label=formatC(Value, 2), y = Value + 0.025), position = position_dodge(width=0.9), vjust=-0.25, family = "Times") +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(text = element_text(size=15, family = "Times"))


## ----varImportance-time, echo = FALSE, warning = FALSE, message = FALSE, fig.cap = '(ref:varImportance-time)'----
time_domain_deltas %>% 
  group_by(variable) %>%
  summarise(Delta_Probability = mean(Delta_Probability)) %>%
  right_join(auc_time, by = "variable") %>%
  as_tibble() %>%
  mutate(
    Feature = fct_reorder(variable, Reduction_AUC, .desc = TRUE),
    Delta_Probability = as.factor(ifelse(Delta_Probability >= 0, "Increase", "Decrease"))
  ) %>%
  select(-variable) %>%
  ggplot(aes(x = Feature, y = Reduction_AUC))+
    geom_segment(
      aes(
        y = 0,
        x = Feature, 
        yend = Reduction_AUC, 
        xend = Feature
      ), 
      color = "black"
    ) +
    geom_point(aes(colour = Delta_Probability), size = 8) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      text = element_text(size=15, family = "Times"),
      legend.position = "bottom"
    ) +
    labs(
      x = "Feature",
      y = "Decrease in Model AUC When Feature Removed"
    ) +
  scale_color_viridis_d(name = "Mean Change in Probability of \"Episode\" Prediction when Perturbed")


## ----varImportance-freq, echo = FALSE, warning = FALSE, message = FALSE, fig.cap = '(ref:varImportance-freq)'----
freq_domain_deltas %>% 
  group_by(variable) %>%
  summarise(Delta_Probability = mean(Delta_Probability)) %>%
  right_join(auc_freq, by = "variable") %>%
  as_tibble() %>%
  mutate(
    Feature = fct_reorder(variable, Reduction_AUC, .desc = TRUE),
    Delta_Probability = as.factor(ifelse(Delta_Probability >= 0, "Increase", "Decrease"))
  ) %>%
  select(-variable) %>%
  ggplot(aes(x = Feature, y = Reduction_AUC))+
    geom_segment(
      aes(
        y = 0,
        x = Feature, 
        yend = Reduction_AUC, 
        xend = Feature
      ), 
      color = "black"
    ) +
    geom_point(aes(colour = Delta_Probability), size = 8) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      text = element_text(size=15, family = "Times"),
      legend.position = "bottom"
    ) +
    #facet_grid(Y~.) +
    labs(
      x = "Feature",
      y = "Decrease in Model AUC When Feature Removed"
    ) +
  scale_color_viridis_d(name = "Mean Change in Probability of \"Episode\" Prediction when Perturbed")


## ----highfreq-timeseries, echo = FALSE, warning = FALSE, message = FALSE, fig.cap = '(ref:highfreq-timeseries)'----
df_freq_ml %>%
  gather('variable', 'value', HF_1:HF_6) %>%
  ggplot(aes(x=variable, y=value, colour=Y)) +
    geom_jitter(alpha = 0.6, width = 0.15) +
    stat_smooth(aes(group = Y), method = "loess") +
    labs(x = "Frequency Band Window", y = "Z-Value") +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size=15, family = "Times")
    ) +
    scale_color_viridis_d()

