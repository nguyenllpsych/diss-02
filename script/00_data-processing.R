###########################################
## Dissertation Study 2                  ##
## Data Processing for ESM Dyads Project ##
## Linh Nguyen                           ##
## 2024-01-15                            ##
###########################################

# This script loads raw data files from ExpiWell and Qualtrics
#   and participant tracking log from Box.
#   ESM and Baseline data are combined using matching participant IDs from log.
#   The full processed data file is exported.

# This script was created and ran on data collected in the Fall 2023 semester.
#   This was rerun for data collected in the Spring 2024 semester.
#   Generating two time-stamped versions: 2024-01-15 and 2024-03-11

# I. 2024-01-15 ================================================================

## 1. META ---------------------------------------------------------------------

# load libraries
library(dplyr)     # for general wrangling
library(rio)       # for import export
library(here)      # for directory management
library(tidyr)     # for reshaping to long
library(lubridate) # for working with date time

# data paths
log_path <- paste0(
  here(),
  "/data/2024-01-15/Participant_Tracking_2024-01-15.xlsx")
esm_path <- paste0(
  here(), 
  "/data/2024-01-15/ExpiWell_Responses_2023-12-23.csv")
baseline_int_path  <- paste0(
  here(),
  "/data/2024-01-15/Qualtrics_Responses_Interview_2024-01-15.csv")
baseline_noint_path <- paste0(
  here(),
  "/data/2024-01-15/Qualtrics_Responses_NoInterview_2014-01-15.csv")
dict_path <- paste0(
  here(),
  "/data/dict.xlsx"
)

## 2 PROCESSING ----------------------------------------------------------------

### 2a. Log --------------------------------------------------------------------

# load in data
log <- rio::import(log_path)

# clean participant log to use as matching indices
log <- log %>%
  
  # select only completed participants
  filter(!is.na(ESM_Number_P1) & !is.na(ESM_Number_P2)) %>%
  
  # select only relevant variables
  select(Couple_ID, ESM_Number_P1, ESM_Number_P2, ESM_ID_P1, ESM_ID_P2,
         ESM_End) %>%
  
  # transform to long so each participant is a row
  gather(key = "variable", "value", ESM_Number_P1:ESM_ID_P2) %>%
  separate_wider_delim(cols = variable, delim = "_P", 
                       names = c("variable", "Participant")) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  arrange(Couple_ID)%>%
  
  # create a participant_specific ID
  mutate(Participant_ID = paste0(Couple_ID, "00", Participant),
         Start_Date = as.Date(ESM_End) - 6) %>%
  
  # rearrange variables
  select(Couple_ID, Participant_ID, Start_Date, ESM_Number, ESM_ID) %>%
  
  # ensure esm_id conforms to names standards
  mutate(ESM_ID = gsub(pattern = " ", replacement = "",
                       x = tolower(ESM_ID)))

### 2b. Baseline ---------------------------------------------------------------

# load in data files
baseline_int_names <- rio::import(baseline_int_path) %>%
  names()
baseline_int <- rio::import(baseline_int_path,
  skip = 2)
names(baseline_int) <- baseline_int_names
baseline_noint_names <- rio::import(baseline_noint_path) %>%
  names()
baseline_noint <- rio::import(baseline_noint_path,
  skip = 2)
names(baseline_noint) <- baseline_noint_names

# remove unnecessary qualtrics variables and save only finished entries
baseline_int <- baseline_int %>%
  filter(Finished == "TRUE") %>%
  select(-c(StartDate:consent_name),
         -c(consent_record, recruit, future))
baseline_noint <- baseline_noint %>%
  filter(Finished == "TRUE") %>%
  select(-c(StartDate:consent_name),
         -c(recruit, future))

# double check that baseline_int and baseline_noint have the same columns
setdiff(names(baseline_int), names(baseline_noint))
setdiff(names(baseline_noint),names(baseline_int))

# combine interview and no interview data
baseline <- rbind(baseline_int, baseline_noint)

# fix IDs
baseline <- baseline %>%
  
  # filter out test ids
  filter(!consent_id %in% c("dul8558", "moi2331", "nic5747", "rud3772",
                            "Fat7571", "kel7003", "maa6391", "ngu9331",
                            "mad1391", "hay5417", "cor6232", "jen4528",
                            "Sara6994")) %>%
  
  # ensure ESM_ID conforms to names standards
  mutate(ESM_ID = gsub(pattern = " ", replacement = "",
                       x = tolower(consent_id)))

# double check that all baseline esm_id can be found in log's esm_id
all(baseline$ESM_ID %in% log$ESM_ID)

# add couple and participant IDs
baseline <- merge(
  log %>% select(Couple_ID, ESM_ID, Participant_ID),
  baseline
) %>%
  # remove esm_id and consent_id
  select(-c(ESM_ID, consent_id)) %>%
  # sort by couple_id
  arrange(Couple_ID)

# export
rio::export(baseline,
            paste0(here(), "/data/2024-01-15/baseline_processed_2024-01-15.RDS"))

### 2c. ESM --------------------------------------------------------------------

# load in dict
dict_esm <- rio::import(dict_path) %>%
  filter(data == "esm")

# load in data
esm <- rio::import(esm_path, skip = 4) %>%
  select(
    # remove unnecessary meta-data
    -c(V2:V9), 
    # remove ESM_ID variable
    -V11,
    # remove built-in "Score" variable from ExpiWell
    -V14)

# merge in start date from log
esm <- merge(esm, log %>% select(V10 = ESM_Number, Start_Date))

# time variables
#   enforce time format to survey start time
esm$survey_time <- mdy_hm(esm$V1)

#   time: 0 -> 4
#     0 = 9am, 1 = 12pm, 2 = 3pm, 3 = 6pm, 4 = 9pm
#     ensure that if surveys were completed at the end of time frame
#     would still have the same time stamp
esm$time   <- format(esm$survey_time, "%H")
wrong_time <- c("10", "13", "16", "19", "22")
good_time  <- c("09", "12", "15", "18", "21")
for(i in seq(wrong_time)){
  esm$time[esm$time == wrong_time[i]] <- good_time[i]
}
esm$time   <- dense_rank(as.numeric(esm$time)) - 1

#   day: month day year
esm$day <- as.Date(esm$survey_time)

#   day indices: first ever to last ever survey day starting from 0 -> 6
esm$day_idx <- as.numeric(esm$day - esm$Start_Date)

#   time indices: first ever to last ever survey instance starting from 0 -> 34
esm$time_idx <- esm$time + 5 * esm$day_idx

# rearrange variables
esm <- esm %>%
  select(V10, time, day, time_idx, day_idx,
         V12:V29)

# import names from dictionary
names(esm) <- c("ESM_Number", dict_esm$var)

# filter out test participants
esm <- esm %>%
  filter(!ESM_Number %in% c("Participant 1", "Preview Response"))

# double check that all ESM Numbers exist in log
all(esm$ESM_Number %in% log$ESM_Number)

# merge in participant ID
esm <- merge(
  log %>% select(Couple_ID, ESM_Number, Participant_ID),
  esm
) %>%
  # remove ESM_Number
  select(-ESM_Number) %>%
  # sort by couple_id
  arrange(Couple_ID)

# remove responses that were accidentally completed before start dates
esm <- esm %>% filter(time_idx >= 0)

# export
rio::export(esm, 
            paste0(here(), "/data/2024-01-15/esm_processed_2024-01-15.RDS"))
            
# II. 2024-03-11 ===============================================================

## 1. META ---------------------------------------------------------------------

# load libraries
library(dplyr)     # for general wrangling
library(rio)       # for import export
library(here)      # for directory management
library(tidyr)     # for reshaping to long
library(lubridate) # for working with date time

# data paths
log_path <- paste0(
  here(),
  "/data/2024-03-11/Participant_Tracking_2024-03-11.xlsx")
esm_path <- paste0(
  here(), 
  "/data/2024-03-11/ExpiWell_Responses_2024-03-11.csv")
baseline_int_path  <- paste0(
  here(),
  "/data/2024-03-11/Qualtrics_Responses_Interview_2024-03-11.csv")
baseline_noint_path <- paste0(
  here(),
  "/data/2024-03-11/Qualtrics_Responses_NoInterview_2024-03-11.csv")
dict_path <- paste0(
  here(),
  "/data/dict.xlsx"
)

## 2 PROCESSING ----------------------------------------------------------------

### 2a. Log --------------------------------------------------------------------

# load in data
log <- rio::import(log_path)

# clean participant log to use as matching indices
log <- log %>%
  
  # select only completed participants
  filter(!is.na(ESM_Number_P1) & !is.na(ESM_Number_P2)) %>%
  
  # select only relevant variables
  select(Couple_ID, ESM_Number_P1, ESM_Number_P2, ESM_ID_P1, ESM_ID_P2,
         ESM_End) %>%
  
  # transform to long so each participant is a row
  gather(key = "variable", "value", ESM_Number_P1:ESM_ID_P2) %>%
  separate_wider_delim(cols = variable, delim = "_P", 
                       names = c("variable", "Participant")) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  arrange(Couple_ID)%>%
  
  # create a participant_specific ID
  mutate(Participant_ID = paste0(Couple_ID, "00", Participant),
         Start_Date = as.Date(ESM_End) - 6) %>%
  
  # rearrange variables
  select(Couple_ID, Participant_ID, Start_Date, ESM_Number, ESM_ID) %>%
  
  # ensure esm_id conforms to names standards
  mutate(ESM_ID = gsub(pattern = " ", replacement = "",
                       x = tolower(ESM_ID)))

### 2b. Baseline ---------------------------------------------------------------

# load in data files
baseline_int_names <- rio::import(baseline_int_path) %>%
  names()
baseline_int <- rio::import(baseline_int_path,
  skip = 2)
names(baseline_int) <- baseline_int_names
baseline_noint_names <- rio::import(baseline_noint_path) %>%
  names()
baseline_noint <- rio::import(baseline_noint_path,
  skip = 2)
names(baseline_noint) <- baseline_noint_names

# remove unnecessary qualtrics variables and save only finished entries
baseline_int <- baseline_int %>%
  filter(Finished == "TRUE") %>%
  select(-c(StartDate:consent_name),
         -c(consent_record, recruit, future))
baseline_noint <- baseline_noint %>%
  filter(Finished == "TRUE") %>%
  select(-c(StartDate:consent_name),
         -c(recruit, future))

# double check that baseline_int and baseline_noint have the same columns
setdiff(names(baseline_int), names(baseline_noint))
setdiff(names(baseline_noint),names(baseline_int))

# combine interview and no interview data
baseline <- rbind(baseline_int, baseline_noint)

# fix IDs
baseline <- baseline %>%
  
  # ensure ESM_ID conforms to names standards
  mutate(ESM_ID = gsub(pattern = " ", replacement = "",
                       x = tolower(consent_id)))

# double check that all baseline esm_id can be found in log's esm_id
all(baseline$ESM_ID %in% log$ESM_ID)

# add couple and participant IDs
baseline <- merge(
  log %>% select(Couple_ID, ESM_ID, Participant_ID),
  baseline
) %>%
  # remove esm_id and consent_id
  select(-c(ESM_ID, consent_id)) %>%
  # sort by couple_id
  arrange(Couple_ID)

# export
rio::export(baseline,
            paste0(here(), "/data/2024-03-11/baseline_processed_2024-03-11.RDS"))

### 2c. ESM --------------------------------------------------------------------

# load in dict
dict_esm <- rio::import(dict_path) %>%
  filter(data == "esm")

# load in data
esm <- rio::import(esm_path, skip = 4) %>%
  select(
    # remove unnecessary meta-data
    -c(V2:V9), 
    # remove ESM_ID variable
    -V11,
    # remove built-in "Score" variable from ExpiWell
    -V14)

# merge in start date from log
esm <- merge(esm, log %>% select(V10 = ESM_Number, Start_Date))

# time variables
#   enforce time format to survey start time
esm$survey_time <- mdy_hm(esm$V1)

#   time: 0 -> 4
#     0 = 9am, 1 = 12pm, 2 = 3pm, 3 = 6pm, 4 = 9pm
#     ensure that if surveys were completed at the end of time frame
#     would still have the same time stamp
esm$time   <- format(esm$survey_time, "%H")
wrong_time <- c("10", "13", "16", "19", "22")
good_time  <- c("09", "12", "15", "18", "21")
for(i in seq(wrong_time)){
  esm$time[esm$time == wrong_time[i]] <- good_time[i]
}
esm$time   <- dense_rank(as.numeric(esm$time)) - 1

#   day: month day year
esm$day <- as.Date(esm$survey_time)

#   day indices: first ever to last ever survey day starting from 0 -> 6
esm$day_idx <- as.numeric(esm$day - esm$Start_Date)

#   time indices: first ever to last ever survey instance starting from 0 -> 34
esm$time_idx <- esm$time + 5 * esm$day_idx

# rearrange variables
esm <- esm %>%
  select(V10, time, day, time_idx, day_idx,
         V12:V29)

# import names from dictionary
names(esm) <- c("ESM_Number", dict_esm$var)

# double check that all ESM Numbers exist in log
all(esm$ESM_Number %in% log$ESM_Number)

# merge in participant ID
esm <- merge(
  log %>% select(Couple_ID, ESM_Number, Participant_ID),
  esm
) %>%
  # remove ESM_Number
  select(-ESM_Number) %>%
  # sort by couple_id
  arrange(Couple_ID)

# remove responses that were accidentally completed before start dates
esm <- esm %>% filter(time_idx >= 0)

# export
rio::export(esm, 
            paste0(here(), "/data/2024-03-11/esm_processed_2024-03-11.RDS"))