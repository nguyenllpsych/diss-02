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
#   This will need to be rerun for data collected in 2024.

# I. META ----------------------------------------------------------------------

# load libraries
library(dplyr) # for general wrangling
library(rio)   # for import export
library(here)  # for directory management
library(tidyr) # for reshaping to long

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

# II. PROCESSING ---------------------------------------------------------------

# 1. Log -----------------------------------------------------------------------

# load in data
log <- rio::import(log_path)

# clean participant log to use as matching indices
log <- log %>%
  
  # select only completed participants
  filter(!is.na(ESM_Number_P1) & !is.na(ESM_Number_P2)) %>%
  
  # select only relevant variables
  select(Couple_ID, ESM_Number_P1, ESM_Number_P2, ESM_ID_P1, ESM_ID_P2) %>%
  
  # transform to long so each participant is a row
  gather(key = "variable", "value", ESM_Number_P1:ESM_ID_P2) %>%
  separate_wider_delim(cols = variable, delim = "_P", 
                       names = c("variable", "Participant")) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  arrange(Couple_ID)%>%
  
  # create a participant_specific ID
  mutate(Participant_ID = paste0(Couple_ID, "00", Participant)) %>%
  
  # rearrange variables
  select(Couple_ID, Participant_ID, ESM_Number, ESM_ID) %>%
  
  # ensure esm_id conforms to names standards
  mutate(ESM_ID = gsub(pattern = " ", replacement = "",
                       x = tolower(ESM_ID)))

# 2. Baseline ------------------------------------------------------------------

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

# 3. ESM -----------------------------------------------------------------------

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

# time variables
#   enforce time format to survey start time
esm$survey_time <- as.POSIXct(unlist(strsplit(esm$V1, " - "))
                              [seq(from = 1, to = nrow(esm)*2, by = 2)], 
                              format = "%m/%d/%Y %I:%M %p")

#   time: hour minute second
esm$time <- unlist(strsplit(as.character(
  esm$survey_time), " "))[seq(from = 2, to = nrow(esm)*2, by = 2)]
#   day: month day year
esm$day <- unlist(strsplit(as.character(
  esm$survey_time), " "))[seq(from = 1, to = nrow(esm)*2, by = 2)]
#   time indices: first ever to last ever survey instance starting from 0
esm$time_idx <- as.numeric(ave(esm$V10, esm$V10, FUN = seq_along)) - 1
#   day indices: first ever to last ever survey day starting from 0
esm$day_idx <- ave(esm$day, esm$V10, FUN = function(x) {
  as.numeric(factor(x, levels = unique(x))) - 1
  })

# rearrange variables
esm <- esm %>%
  select(V10, time:day_idx,
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

# export
rio::export(esm, 
            paste0(here(), "/data/2024-01-15/esm_processed_2024-01-15.RDS"))
            