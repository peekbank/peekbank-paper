library(tidyverse)
library(png)
library(grid)
library(ggplot2)
library(xtable)
library(here)

load(file = here("bu2021","table","dataset_info.Rds"))

dataset_name_mapping <- read_csv(here("bu2021","table","dataset_name_mapping.csv"))
iso_codes <- read_csv(here("bu2021","table","iso_language_codes.csv"))

dataset_unique_subj <- dataset_info %>%
  distinct(subject_id,sex)

summarize_datasets <- dataset_info %>%
  left_join(dataset_name_mapping) %>%
  group_by(dataset_rename, apa_cite) %>%
  summarize(
    #num_admin=length(unique(administration_id)),
    num_subj=length(unique(subject_id)),
    #percent_female=round(sum(sex=="female")/sum(sex %in% c("female","male")),2),
    avg_age=mean(age,na.rm=T),
    min_age_months = round(min(age, na.rm = TRUE),0), 
    max_age_months = round(max(age, na.rm = TRUE),0),
    method=unique(coding_method)[1],
    native_language = names(which.max(table(native_language)))) %>%
  # mutate(
  #   percent_female = case_when(
  #     is.nan(percent_female) ~ "N/A",
  #     TRUE ~ paste0(percent_female*100,"%"))
  # ) %>%
  mutate(
    method=case_when(
      method=="manual gaze coding" ~ "manual coding",
      method=="eyetracking" ~ "eye-tracking",
      method=="preprocessed eyetracking" ~ "eye-tracking",
      TRUE ~ method)
  ) %>%
  #split language into multiple columns (only two expected; expand if dataset acquires more)
  separate(native_language,into=c("native_language_1","native_language_2"),sep=", ") %>%
  #join based on ISO standard
  left_join(iso_codes,by=c("native_language_1"="iso_code")) %>%
  left_join(iso_codes,by=c("native_language_2"="iso_code")) %>%
  rename(
    language_name_1 = language_name.x,
    language_name_2 = language_name.y
  ) %>%
  #clean up some naming issues
  mutate(
    language_name_1 = case_when(
      language_name_1 == "Spanish; Castilian" ~ "Spanish",
      TRUE ~ language_name_1
    )) %>%
  #unite names
  mutate(
    language = case_when(
      !is.na(language_name_2) ~ paste(language_name_1,language_name_2,sep=", "),
      TRUE ~ language_name_1)) %>%
  # native language special cases
  mutate(
    language = case_when(
      dataset_rename == "tseltal" ~ "Tseltal",
      TRUE ~ language)) %>%
  #convert age range into one column
  mutate(age_range_months= paste(min_age_months,max_age_months,sep=" - ")) %>%
  mutate(
    apa_cite=case_when(
      is.na(apa_cite) ~ "unpublished",
      TRUE ~ apa_cite
    )) %>%
  select(dataset_rename,apa_cite, num_subj,avg_age,age_range_months,method,language) %>%
  arrange(dataset_rename)

tab1 <- xtable::xtable(summarize_datasets, digits=c(1), 
                       caption = "Overview over the datasets in the current database.")

names(tab1) <- c("Dataset Name", "Citation","N","Mean Age (mos.)","Age Range (mos.)", "Method", "Language")

align(tab1) <- c("llrrrrrr")

print(tab1, type="latex", comment = F, table.placement = "H",include.rownames=FALSE, size="\\fontsize{9pt}{10pt}\\selectfont")

latex <- print.xtable(tab1, print.results = FALSE,include.rownames=FALSE)
writeLines(
  c(
    "\\documentclass[12pt]{article}",
    "\\usepackage{geometry}",
    "\\geometry{legalpaper, landscape, margin=1in}",
    "\\begin{document}",
    "\\thispagestyle{empty}",
    latex,
    "\\end{document}"
  ),
  "dataset_table.tex"
)
tools::texi2pdf("dataset_table.tex",clean=TRUE)
