library(knitr)
library(remotes)
library(tidyverse)
library(lme4)
library(lmerTest)
library(tictoc)
library(peekbankr)
library(here)
library(tidymodels)
library(wordbankr)

t_range <- c(-1000,3000)
knitr::opts_chunk$set(cache = TRUE, warn = FALSE, message = FALSE)
load(file = here("cogsci2021/data/aoi_data_joined.Rds"))
d_datasets <- get_datasets()


compute_trial_score <- function(aoi_data_joined, window_start_time, window_end_time, proportion_missing_threshold ){
  rt_data <- aoi_data_joined %>%
    filter(any(t_norm == 0), # must have data at 0
           t_norm >= 0) %>% # only pass data after 0
    group_by(administration_id, trial_id) %>%
    summarise(lengths = rle(aoi)$lengths, 
              values = rle(aoi)$values) 
  rts <- rt_data %>%
    group_by(administration_id, trial_id) %>%
    nest() %>%
    mutate(data = lapply(data, get_rt)) %>%
    unnest(cols = c(data)) 
  rts <- left_join(rts, 
                   aoi_data_joined %>%
                     select(administration_id, trial_id, 
                            age, dataset_name, 
                            english_stimulus_label, 
                            stimulus_novelty, trial_order) %>%
                     distinct())
  
  acc <- window_accuracy(aoi_data_joined, window_start_time, window_end_time, proportion_missing_threshold)
  
}

window_accuracy <- function(aoi_data_joined, window_start_time, window_end_time, proportion_missing_threshold){
  window_start_time = t_range[1]
  window_end_time = t_range[2]
  proportion_missing_threshold = 1
  accuracy_df <- aoi_data_joined %>%
    filter(t_norm < window_end_time & t_norm > window_start_time) %>%
    select(aoi, t_norm, trial_id) %>%
    mutate(aoi_binary = case_when(aoi == 'distractor' ~ 0, aoi == 'other' ~ -1, aoi == 'missing' ~ -1, aoi == 'target' ~ 1)) 
    
  if(((temp$aoi_binary == -1)/(length(temp$aoi_binary))) <= proportion_missing_threshold){
    accuracy_df <- accuracy_df %>% 
      filter(aoi > -1) %>%
      group_by(trial_id) %>%
      summarise(trial_accuracy = mean(aoi_binary))
  } else{
    warning("proportion of missing values greater than proportion missing threshold")
    accuracy_df <- accuracy_df %>% 
      filter(aoi > -1) %>%
      group_by(trial_id) %>%
      summarise(trial_accuracy = mean(aoi_binary))
  }
  return(accuracy_df)
}

get_rt <- function (rle_data, SAMPLING_RATE = 40) {
  # end if no data
  if (is.null(rle_data$values) | is.null(rle_data$lengths)) {
    return(tibble(rt = NA, 
                  shift_type = NA))
  }
  
  onset_aoi <- rle_data$values[1] # zero point AOI
  
  # end if missing for start
  if (!(onset_aoi %in% c("target","distractor"))) {
    return(tibble(rt = NA, 
                  shift_type = "other"))
  }
  first_landing <- rle_data$values[rle_data$values != onset_aoi &
                                     rle_data$values %in% c("target","distractor")][1]
  # end if no shift
  if (is.na(first_landing)) {
    return(tibble(rt = NA, 
                  shift_type = "no shift"))
  }
  
  shift_type <- case_when(onset_aoi == "distractor" &
                            first_landing == "target" ~ "D-T",
                          onset_aoi == "target" &
                            first_landing == "distractor" ~ "T-D",
                          TRUE ~ "other")
  first_landing_idx <- which(rle_data$values == first_landing)[1]
  
  values_before_first_landing <- rle_data$lengths[1:(first_landing_idx-1)]
  # rt is the number of samples happening before arrival + 1 
  # (first sample of arrival)
  # times the length of a sample
  rt <- (sum(values_before_first_landing) + 1) * (1000/SAMPLING_RATE)
  return(tibble(rt = rt, 
                shift_type = shift_type))
}


baseline_increase <- function(){
  
}

