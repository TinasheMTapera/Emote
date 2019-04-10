library(tidyverse)
library(purrr)
library(stringr)
library(furrr)
library(lubridate)
library(broom)
library(knitr)
library(readxl, quietly = TRUE)

#install.packages("here")
library(here)
here() # should give you the root folder of the project on your machine

eda_dirs <- here("EDA_Analysis", "outputs")
eda_df <- list.files(eda_dirs,
                     pattern = ".rds",
                     recursive = TRUE,
                     full.names = TRUE) %>%
  future_map_dfr(.f = function(x) {
    
    df <- readRDS(x)
    df$ID <- str_extract(x, "[[:digit:]]+")
    
    df
  })

events_dir <- here("DataOutputs/", "UpdatedSurveys_180713.xlsx")
events = read_excel(events_dir) %>%
  group_by(ID) %>%
  arrange(ID, Event) %>%
  ungroup() %>%
  filter(!is.na(ID)) %>%
  mutate(Event = ymd_hms(Event)) %>%
  mutate(start = Event - 60 * 30) %>%
  mutate(intervals = interval(Event, start))


remove_overlaps <- function(intervals){
  
  overlaps = rep(FALSE, length(intervals))
  
  for(l in 1:(length(intervals)-1)){
    if(!(is.na(intervals[l]) & is.na(intervals[l+1]))){
      if(int_overlaps(intervals[l], intervals[l+1])){
        overlaps[l] = TRUE
      }
    }
  }
  
  return(overlaps)
}

events <- events %>%
  mutate(ID = as.factor(ID)) %>%
  group_by(ID) %>%
  mutate(overlapped = remove_overlaps(intervals)) %>%
  select(-intervals) %>%
  filter(overlapped ==  FALSE) %>%
  select(-overlapped) %>%
  ungroup()

eda_df <- events %>%
  select(ID, EmotionalEatingEpisodes, Event, Stress) %>%
  mutate(ID = as.character(ID)) %>%
  left_join(eda_df, by = c("ID", "Event"))

eda_df %>% select(ID) %>% unique() %>% nrow() # 21 participants