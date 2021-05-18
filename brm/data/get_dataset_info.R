library(tidyverse)
library(png)
library(grid)
library(ggplot2)
library(xtable)
library(here)

load(file = here("data","dataset_info.Rds"))

dataset_name_mapping <- read_csv(here("data","dataset_name_mapping.csv"))

dataset_unique_subj <- dataset_info %>%
  distinct(subject_id,sex)

summarize_datasets <- dataset_info %>%
  left_join(dataset_name_mapping) %>%
  group_by(dataset_rename) %>%
  summarize(
    #num_admin=length(unique(administration_id)),
    num_subj=length(unique(subject_id)),
    avg_age=mean(age,na.rm=T),
    method=unique(coding_method)[1],
    highest_age=max(age,na.rm=T),
    lowest_age=min(age, na.rm=T),
    language=unique(native_language)[1],
    tracker=unique(tracker)[1],
    num_female=length(sex=="female"),
    num_male=length(sex=="male")) %>%
  mutate(
    method=case_when(
      method=="manual gaze coding" ~ "manual coding",
      method=="eyetracking" ~ "eye-tracking",
      TRUE ~ method)
  ) %>%
  arrange(dataset_rename)

tab1 <- xtable::xtable(summarize_datasets, digits=c(1), 
                       caption = "Overview over the datasets in the current database.")

names(tab1) <- c("Dataset Name", "N","Mean Age (mos.)", "Method", "Maximum Age (mos.)", "Minimum Age (mos.)", "Native Language", "Eyetracker", "Number of females", "Number of males")

print(tab1, type="latex", comment = F, table.placement = "H",include.rownames=FALSE, size="\\fontsize{9pt}{10pt}\\selectfont")
