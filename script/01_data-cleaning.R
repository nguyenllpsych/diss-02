#########################################
## Dissertation Study 2                ##
## Data Cleaning for ESM Dyads Project ##
## Linh Nguyen                         ##
## 2024-01-15                          ##
#########################################

# This script loads the processed esm and baseline data files and clean the data
#   including: (1) recoding all factor likert variables as numeric
#              (2) creating all scale scores

# This script exports full cleaned data files with all variables and items
# This script can also export selected variables for specific projects
#   by specifying the needed variables at the last section

# I. 2024-01-15 ================================================================
## 1. META ---------------------------------------------------------------------

# load libraries
library(dplyr) # for general wrangling
library(here)  # for directory management

# data paths
esm_path <- paste0(here(),
                   "/data/2024-01-15/esm_processed_2024-01-15.RDS")
baseline_path <- paste0(here(),
                        "/data/2024-01-15/baseline_processed_2024-01-15.RDS")
dict_path <- paste0(here(),
                    "/data/dict.xlsx")

# load data and dictionary
esm <- rio::import(esm_path)
baseline <- rio::import(baseline_path)
dict_esm <- rio::import(dict_path, na = c("NA")) %>%
  filter(data == "esm")
dict_bl <- rio::import(dict_path, na = c("NA")) %>%
  filter(data == "baseline")

# source helpers
source(paste0(here(), "/script/00_helpers.R"))

## 2. CLEANING -----------------------------------------------------------------

# recode likert variables from choice text to numeric/factors
esm <- recode_func(.dict = dict_esm, .data = esm)
baseline <- recode_func(.dict = dict_bl, .data = baseline)

# reverse score
esm <- reverse_func(.dict = dict_esm, .data = esm)
baseline <- reverse_func(.dict = dict_bl, .data = baseline)

# race and ethnicity coding
baseline <- baseline %>%
  mutate(race_cat = case_when(
    grepl("biracial|mixed|multi|asian/white|white/hispanic|white, latina|white/native|caucasian/latina", 
          race, ignore.case = T) ~ 5,
    grepl("white|caucasian|norwegian", race, ignore.case = T) ~ 1,
    grepl("black|afric", race, ignore.case = T ) ~ 2,
    grepl("asian|viet|hmong|filip|indian|kore|pakis", race, ignore.case = T) ~ 3,
    grepl("latin|hisp", race, ignore.case = T) ~ 4,
    TRUE ~ NA
  ))

# partner number coding
esm$P_num <- as.numeric(
  substr(esm$Participant_ID, 5, 8)
)
baseline$P_num <- as.numeric(
  substr(baseline$Participant_ID, 5, 8)
)

## 3. SCALE SCORES -------------------------------------------------------------
esm <- score_func(.dict = dict_esm, .data = esm)
baseline <- score_func(.dict = dict_bl, .data = baseline)

## 4. EXPORT -------------------------------------------------------------------

### 4a. export full cleaned data -----------------------------------------------
rio::export(esm, paste0(here(), "/data/2024-01-15/esm_cleaned_2024-01-15.RDS"))
rio::export(baseline, paste0(here(), "/data/2024-01-15/baseline_cleaned_2024-01-15.RDS"))

### 4b. 2024-01-17: Linh RA Project Group 1 (MA, NH, MG) -----------------------
# osf link: https://osf.io/vectx/
# paths to files
dict_path <- paste0(here(),
                    "/data/dict.xlsx")
baseline_path <- paste0(here(),
                        "/data/2024-01-15/baseline_cleaned_2024-01-15.RDS")
dict <- rio::import(dict_path)

# selected items
baseline_data <- rio::import(baseline_path) %>% 
  select(
    # identifiers and demographics
    Couple_ID, Participant_ID, P_num,
    age:race_cat,
    # scale items
    bfas_self_1:bfas_partner_100,
    csi_happiness:csi_feeling_7,
    # scale scores
    self_neuro:partner_open,
    self_withd:partner_opena,
    csi_overall
  )
dictionary <- dict %>%
  filter(data == "baseline" &
           var %in% c(names(baseline_data)))

# export files
save(dictionary, baseline_data,
     file = paste0(here(),
                   "/data/2024-01-15/RA-Group-1_2024-01-17.RData"))

### 4c. 2024-01-17: Linh RA Project Group 2 (FA, KM, EN) -----------------------
# osf link: https://osf.io/m673a/

# paths to files
dict_path <- paste0(here(),
                    "/data/dict.xlsx")
baseline_path <- paste0(here(),
                        "/data/2024-01-15/baseline_cleaned_2024-01-15.RDS")
esm_path <- paste0(here(),
                   "/data/2024-01-15/esm_cleaned_2024-01-15.RDS")
dict <- rio::import(dict_path)

# selected items
baseline_agreeableness <- dict %>% 
  filter(
    data == "baseline" &
    grepl("Agree", scale)) %>% pull(var)
esm_agreeableness <- dict %>%
  filter(
    data == "esm" &
    grepl("Agree", scale)) %>% pull(var)
baseline_data <- rio::import(baseline_path) %>% 
  select(
    # identifiers and demographics
    Couple_ID, Participant_ID, P_num,
    age:race_cat,
    # scale items
    all_of(baseline_agreeableness),
    csi_happiness:csi_feeling_7,
    # scale scores
    self_agree, partner_agree,
    csi_overall
    )
esm_data <- rio::import(esm_path) %>%
  select(
    # identifiers
    Couple_ID, Participant_ID, P_num, time, day, time_idx, day_idx,
    # scale items
    all_of(esm_agreeableness),
    csi_happiness:csi_satisfaction_4,
    # scale scores
    tipi_agree, csi_short,
    # partner presence
    partner_presence
  )
dictionary <- dict %>%
  filter(var %in% c(names(baseline_data), names(esm_data)))

# export files
save(dictionary, baseline_data, esm_data,
     file = paste0(here(),
                   "/data/2024-01-15/RA-Group-2_2024-01-17.RData"))

# I. 2024-03-11 ================================================================
## 1. META ---------------------------------------------------------------------

# load libraries
library(dplyr) # for general wrangling
library(here)  # for directory management

# data paths
esm_path <- paste0(here(),
                   "/data/2024-03-11/esm_processed_2024-03-11.RDS")
baseline_path <- paste0(here(),
                        "/data/2024-03-11/baseline_processed_2024-03-11.RDS")
dict_path <- paste0(here(),
                    "/data/dict.xlsx")

# load data and dictionary
esm <- rio::import(esm_path)
baseline <- rio::import(baseline_path)
dict_esm <- rio::import(dict_path, na = c("NA")) %>%
  filter(data == "esm")
dict_bl <- rio::import(dict_path, na = c("NA")) %>%
  filter(data == "baseline")

# source helpers
source(paste0(here(), "/script/00_helpers.R"))

## 2. CLEANING -----------------------------------------------------------------

# recode likert variables from choice text to numeric/factors
esm <- recode_func(.dict = dict_esm, .data = esm)
baseline <- recode_func(.dict = dict_bl, .data = baseline)

# reverse score
esm <- reverse_func(.dict = dict_esm, .data = esm)
baseline <- reverse_func(.dict = dict_bl, .data = baseline)

# race and ethnicity coding
baseline <- baseline %>%
  mutate(race_cat = case_when(
    grepl("biracial|hispanic & white|bi racial|black/asian|white/ native|white/asian|afro latino", 
          race, ignore.case = T) ~ 5,
    grepl("white|caucasian", race, ignore.case = T) ~ 1,
    grepl("black", race, ignore.case = T ) ~ 2,
    grepl("american indian", race, ignore.case = T) ~ NA,
    grepl("asian|korean|indian", race, ignore.case = T) ~ 3,
    grepl("latin|hisp", race, ignore.case = T) ~ 4,
    TRUE ~ NA
  ))

# partner number coding
esm$P_num <- as.numeric(
  substr(esm$Participant_ID, 5, 8)
)
baseline$P_num <- as.numeric(
  substr(baseline$Participant_ID, 5, 8)
)

## 3. SCALE SCORES -------------------------------------------------------------
esm <- score_func(.dict = dict_esm, .data = esm)
baseline <- score_func(.dict = dict_bl, .data = baseline)

## 4. EXPORT -------------------------------------------------------------------

### 4a. export full cleaned data -----------------------------------------------
rio::export(esm, paste0(
  here(), "/data/2024-03-11/esm_cleaned_2024-03-11.RDS"))
rio::export(baseline, paste0(
  here(), "/data/2024-03-11/baseline_cleaned_2024-03-11.RDS"))

### 4b. combine all baseline and all esm data
old_esm <- rio::import(paste0(
  here(), "/data/2024-01-15/esm_cleaned_2024-01-15.RDS"))
old_baseline <- rio::import(paste0(
  here(), "/data/2024-01-15/baseline_cleaned_2024-01-15.RDS"))

rio::export(rbind(old_esm, esm), paste0(
  here(), "/data/esm_cleaned.RDS"
))
rio::export(rbind(old_baseline, baseline), paste0(
  here(), "/data/baseline_cleaned.RDS"
))

### 4b. 2024-03-11: Linh RA Project Group 1 (MA, NH, MG) -----------------------
# osf link: https://osf.io/vectx/
# paths to files
dict_path <- paste0(here(),
                    "/data/dict.xlsx")
baseline_path <- paste0(here(),
                        "/data/baseline_cleaned.RDS")
dict <- rio::import(dict_path)

# selected items
baseline_data <- rio::import(baseline_path) %>% 
  select(
    # identifiers and demographics
    Couple_ID, Participant_ID, P_num,
    age:race_cat,
    # scale items
    bfas_self_1:bfas_partner_100,
    csi_happiness:csi_feeling_7,
    # scale scores
    self_neuro:partner_open,
    self_withd:partner_opena,
    csi_overall
  )
dictionary <- dict %>%
  filter(data == "baseline" &
           var %in% c(names(baseline_data)))

# export files
save(dictionary, baseline_data,
     file = paste0(here(),
                   "/data/RA-Group-1.RData"))

### 4c. 2024-03-11: Linh RA Project Group 2 (FA, KM, EN) -----------------------
# osf link: https://osf.io/m673a/

# paths to files
dict_path <- paste0(here(),
                    "/data/dict.xlsx")
baseline_path <- paste0(here(),
                        "/data/baseline_cleaned.RDS")
esm_path <- paste0(here(),
                   "/data/esm_cleaned.RDS")
dict <- rio::import(dict_path)

# selected items
baseline_agreeableness <- dict %>% 
  filter(
    data == "baseline" &
    grepl("Agree", scale)) %>% pull(var)
esm_agreeableness <- dict %>%
  filter(
    data == "esm" &
    grepl("Agree", scale)) %>% pull(var)
baseline_data <- rio::import(baseline_path) %>% 
  select(
    # identifiers and demographics
    Couple_ID, Participant_ID, P_num,
    age:race_cat,
    # scale items
    all_of(baseline_agreeableness),
    csi_happiness:csi_feeling_7,
    # scale scores
    self_agree, partner_agree,
    csi_overall
    )
esm_data <- rio::import(esm_path) %>%
  select(
    # identifiers
    Couple_ID, Participant_ID, P_num, time, day, time_idx, day_idx,
    # scale items
    all_of(esm_agreeableness),
    csi_happiness:csi_satisfaction_4,
    # scale scores
    tipi_agree, csi_short,
    # partner presence
    partner_presence
  )
dictionary <- dict %>%
  filter(var %in% c(names(baseline_data), names(esm_data)))

# export files
save(dictionary, baseline_data, esm_data,
     file = paste0(here(),
                   "/data/RA-Group-2.RData"))
