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

# I. META ----------------------------------------------------------------------

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

# function to recode likert variables
recode_func <- function(.dict, .data) {
  for(var in .dict[.dict$recode == "yes", "var"]) {
  
  # grap the value_label
  mapping_string <- .dict[.dict$var == var, "value_label"]
  
  # split the string into key value pairs
  pairs <- strsplit(mapping_string, ";\\s*")[[1]]
  values <- strsplit(pairs, "\\s*=\\s*")
  
  # trim white space
  new_values <- sapply(values, function(pair) trimws(pair[1]))
  old_values <- sapply(values, function(pair) trimws(pair[2]))
  
  # create mapping df
  mapping_df <- data.frame(new_value = as.character(new_values),
                           old_value = as.character(old_values)) %>%
    filter(!is.na(old_value))
  
  # update values in selected column
  .data <- .data %>%
    mutate(!!sym(var) := gsub(pattern = "\n", 
                              replacement = " ",
                              as.character(!!sym(var))))
  .data <- .data %>%
    mutate(!!sym(var) := case_when(
      !!sym(var) %in% mapping_df$old_value ~ 
        mapping_df$new_value[match(!!sym(var), mapping_df$old_value)],
      TRUE ~ !!sym(var)  # leave the value unchanged if not found in mapping_df
      ))
  .data <- .data %>%
    mutate(!!sym(var) := as.numeric(!!sym(var)))
  } # END for var LOOP
  
  # return recoded dataframe
  return(.data)

} # END recode_func DEF

# function to reverse score items
reverse_func <- function(.dict, .data){
  
  # grab the list of reverse keyed items and their max
  reverse_dict <- .dict %>%
    filter(key == -1) %>%
    select(var, scale_max)
  
  # reverse all appropriate items
  for (i in 1:nrow(reverse_dict)) {
    
    # grab the column name and scale_max value
    var        <- reverse_dict[i, "var"]
    scale_max  <- reverse_dict[i, "scale_max"]
    
    # reverse item in data
    .data[var] <- scale_max + 1 - .data[var]
  
  } # END for reverse row LOOP
  
  # return mutated dataframe
  return(.data)
  
} # END reverse_func DEF

# function to score scales
score_func <- function(.dict, .data) {
  
  # 1. higher-order scales #
  # loop through all unique scales in dict$scale
  for(current_scale in unique(.dict$scale[!is.na(.dict$scale)])) {
    
    # grab the scoring function
    score_method <- .dict %>%
      filter(scale == current_scale) %>%
      pull(scoring) %>%
      unique()
    
    # skip this scale if no scoring needed
    if(is.na(score_method)) next
    
    # grab the scale label
    scale_label <- .dict %>%
      filter(scale == current_scale) %>%
      pull(scale_label) %>%
      unique()
    
    # create a list of the corresponding items
    var_list <- .dict %>% 
      filter(scale == current_scale) %>%
      pull(var)
    
    # aggregate items depending on method
    if(score_method == "mean") {
      scored_vector <- rowMeans(.data[, var_list], na.rm = T)
    } else if(score_method == "sum") {
      scored_vector <- rowSums(.data[, var_list], na.rm = T)
    } # END if score_method STATEMENT
    
    # append new vector to original data frame
    .data <- cbind(.data, scored_vector)
    
    # rename to align with scale label
    names(.data)[names(.data) == "scored_vector"] <- scale_label
    
    } # END for current_scale LOOP
  
  # 2. subscales if any
  # loop through all unique scales in dict$subscale
  for(current_subscale in unique(.dict$subscale[!is.na(.dict$subscale)])) {
    
    # grab the scoring function
    score_method <- .dict %>%
      filter(subscale == current_subscale) %>%
      pull(scoring) %>%
      unique()
    
    # skip this subscale if no scoring needed
    if(is.na(score_method)) next
    
    # grab the scale label
    scale_label <- .dict %>%
      filter(subscale == current_subscale) %>%
      pull(subscale_label) %>%
      unique()
    
    # create a list of the corresponding items
    var_list <- .dict %>% 
      filter(subscale == current_subscale) %>%
      pull(var)
    
    # aggregate items depending on method
    if(score_method == "mean") {
      scored_vector <- rowMeans(.data[, var_list], na.rm = T)
    } else if(score_method == "sum") {
      scored_vector <- rowSums(.data[, var_list], na.rm = T)
    } # END if score_method STATEMENT
    
    # append new vector to original data frame
    .data <- cbind(.data, scored_vector)
    
    # rename to align with scale label
    names(.data)[names(.data) == "scored_vector"] <- scale_label
    
  } # END for current_scale LOOP
  
  # return scored dataframe
  return(.data)
}

# II. CLEANING -----------------------------------------------------------------

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

# III. SCALE SCORES ------------------------------------------------------------
esm <- score_func(.dict = dict_esm, .data = esm)
baseline <- score_func(.dict = dict_bl, .data = baseline)

# IV. EXPORT -------------------------------------------------------------------
# 1. export full cleaned data --------------------------------------------------
rio::export(esm, paste0(here(), "/data/2024-01-15/esm_cleaned_2024-01-15.RDS"))
rio::export(baseline, paste0(here(), "/data/2024-01-15/baseline_cleaned_2024-01-15.RDS"))

# 2. 2024-01-17: Linh RA Project Group 1 (MA, NH, MG) --------------------------
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
    Couple_ID, Participant_ID, 
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
                   "/data/RA-Group-1_2024-01-17.RData"))

# 3. 2024-01-17: Linh RA Project Group 2 (FA, KM, EN) --------------------------
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
    Couple_ID, Participant_ID, 
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
    Couple_ID, Participant_ID, time, day, time_idx, day_idx,
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
                   "/data/RA-Group-2_2024-01-17.RData"))
