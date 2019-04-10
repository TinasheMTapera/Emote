######## Truncation ###########

# data used for frequency domain analysis
df_freq <- read_csv(here('DataOutputs','data_out_20181113 copy.csv'))

# data used for time domain analysis
df_time <- read_csv(here('DataOutputs','data_out copy.csv'))

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
  
  ################# TRUNCATION HAPPENS HERE#####################
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
  #############################################################
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