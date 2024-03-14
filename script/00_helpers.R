############################################
## Dissertation Study 2                   ##
## Helper Functions for ESM Dyads Project ##
## Linh Nguyen                            ##
## nguyenllpsych@gmail.com                ##
## 2024-02-26                             ##
############################################

# Hypothesis Functions ---------------------------------------------------------

## Hypothesis 1: Baseline Assortment -------------------------------------------
h1_function <- function(
  # character vector of variables for bivariate correlations 
  var_list, 
  # data frame: each column is a profile, each row is the variable name
  prof_list,
  # data frame name
  .data) {
  
  # sort dataframe by ID to make sure couples match
  .data <- .data %>% arrange(Couple_ID) 
  
  ### BIVARIATE CORRELATIONS ###
  
  # initialize cor table
  cor_tab <- data.frame()
  
  # iterate through variable list
  for(var in var_list) {
    
    # extract vectors from data
    p1 <- .data[.data$P_num == 1, var, drop = T]
    p2 <- .data[.data$P_num == 2, var, drop = T]
    
    # extract correlation and p-value
    current_cor <- cor.test(p1, p2, method = "pearson",
                            alternative = "two.sided")
    
    # store values
    cor_tab <- rbind(cor_tab, c(var,
                                round(current_cor$estimate, 3), 
                                round(current_cor$p.value, 3),
                                round(current_cor$conf.int[1], 3),
                                round(current_cor$conf.int[2], 3)))
  } # END for var LOOP
  
  # rename cor_tab and ensure variable type
  if(!is.null(var_list)){
    names(cor_tab) <- c("variable", "correlation", "p_value", "LL", "UL")
    cor_tab$correlation <- as.numeric(cor_tab$correlation)
    cor_tab$p_value <- as.numeric(cor_tab$p_value)
    cor_tab$LL <- as.numeric(cor_tab$LL)
    cor_tab$UL <- as.numeric(cor_tab$UL)
  }
  
  ### PROFILE CORRELATIONS ###

  # initialize profile cor table
  prof_tab <- data.frame()
  
  # initialize full profile df to be merged back into df
  full_profile_df <- data.frame(
    Couple_ID = unique(.data$Couple_ID)
  )

  # iterate through profile list
  for(profile in names(prof_list)) {
    
    # variable names for each participant
    var_list <- unique(paste0(prof_list[, profile]))

    # extract raw vectors from data
    p1 <- .data %>%
      filter(P_num == 1) %>%
      select(all_of(var_list))
    p2 <- .data %>%
      filter(P_num == 2) %>%
      select(all_of(var_list))

    # compute gender-centered and standardized data
    centered_df <- std_df <- data.frame(
      P_num = rep(1:2, times = length(unique(.data$Couple_ID)))
    )
    for(ivar in var_list){
      male_mean   <- mean(.data[.data$sex == 0, ivar], na.rm=T)
      female_mean <- mean(.data[.data$sex == 1, ivar], na.rm=T)
      male_sd     <- sd(.data[.data$sex == 0, ivar], na.rm=T)
      female_sd   <- sd(.data[.data$sex == 1, ivar], na.rm=T)
      tmp <- .data %>%
        mutate(
          centered = case_when(
            sex == 0 ~ (!!sym(ivar) - male_mean),
            sex == 1 ~ (!!sym(ivar) - female_mean)),
          std = case_when(
            sex == 0 ~ (!!sym(ivar) - male_mean)/male_sd,
            sex == 1 ~ (!!sym(ivar) - female_mean)/female_sd)
          )
      centered_df <- cbind(centered_df, tmp %>% select(centered))
      names(centered_df)[names(centered_df) == "centered"] <- ivar
      std_df <- cbind(std_df, tmp %>% select(std))
      names(std_df)[names(std_df) == "std"] <- ivar
    } # END for ivar loop

    #extract centered and std dataframes for p1 and p2
    p1_centered <- centered_df %>% filter(P_num == 1) %>%
      select(all_of(var_list))
    p2_centered <- centered_df %>% filter(P_num == 2) %>%
      select(all_of(var_list))
    p1_std <- std_df %>% filter(P_num == 1) %>%
      select(all_of(var_list))
    p2_std <- std_df %>% filter(P_num == 2) %>%
      select(all_of(var_list))
        
    # RAW profile correlations
    raw   <- Hmisc::rcorr(t(p1), t(p2), type = "pearson")
    raw_r <- diag(raw$r[c(1:nrow(p1)), c((nrow(p1)+1):(nrow(p1)*2))])
    raw_p <- diag(raw$P[c(1:nrow(p1)), c((nrow(p1)+1):(nrow(p1)*2))])
    
    # GENDER-MEAN-CENTERED profile correlations
    centered   <- Hmisc::rcorr(t(p1_centered), t(p2_centered), type = "pearson")
    centered_r <- diag(centered$r[c(1:nrow(p1)), 
                                  c((nrow(p1)+1):(nrow(p1)*2))])
    centered_p <- diag(centered$P[c(1:nrow(p1)), 
                                  c((nrow(p1)+1):(nrow(p1)*2))])
    
    # GENDER-STANDARDIZED profile correlations
    std   <- Hmisc::rcorr(t(p1_std), t(p2_std), type = "pearson")
    std_r <- diag(std$r[c(1:nrow(p1)), c((nrow(p1)+1):(nrow(p1)*2))])
    std_p <- diag(std$P[c(1:nrow(p1)), c((nrow(p1)+1):(nrow(p1)*2))])
    
    # store values
    # proportions of couples with significant and positive profile correlations
    prof_tab <- rbind(prof_tab, 
                      c(profile,
                        round(mean(raw_p[raw_r > 0] < 0.05, na.rm = T), 5),
                        round(mean(centered_p[centered_r > 0] < 0.05, na.rm = T), 5),
                        round(mean(std_p[std_p > 0] < 0.05, na.rm = T), 5)))
    
    # store values of profile correlations in profile_df
    #   ncol = 1 + 3 X # profiles
    #     1 couple identifier
    #     3 profile correlations raw/centered/std for each profile
    #   nrow = # couples
    #   to be merged back into df

    profile_df <- data.frame(
      raw_r = raw_r,
      centered_r = centered_r,
      std_r = std_r
    )
    names(profile_df)[1] <- paste0(profile, "_raw_r")
    names(profile_df)[2] <- paste0(profile, "_centered_r")
    names(profile_df)[3] <- paste0(profile, "_std_r")
    
    # merge all profile_dfs together
    full_profile_df <- cbind(full_profile_df, profile_df)
  } # END for profile LOOP
  
  # rename prof_tab
  if(!is.null(prof_list)) {
    names(prof_tab) <- c("profile", 
                         "raw", "centered", "standardized")
  }
  
  # output
  if(!is.null(prof_list)) {
    return(list(bivariate  = cor_tab,
                profile    = prof_tab,
                profile_df = full_profile_df))
  } else {
    return(list(bivariate  = cor_tab))
  }
  
} # END h1_function


## Hypothesis 2: Perceived vs. Actual ------------------------------------------
h2_function <- function(
  # character vector of all personality variables with self/other reports
  #   naming convention: self_var and partner_var
  perception_list,
  
  # analytic dataframe
  .data) {
  
  # create dataframes to store results
  #   similarity_df: raw actual and perceived similarity
  #   compare_df: comparison between actual and perceived similarity
  similarity_df <- compare_df <- data.frame()
  
  # sort data to make sure couple id align
  .data <- .data %>%
    arrange(Couple_ID)
  
  # loop through self/other variables
  for (var in perception_list) {
    
    # extract vectors from data
    #   p1self:    P1's self-perception
    #   p1partner: P1's perception of their partner
    #   p2self:    P2's self-perception
    #   p2partner: P2's perception of their partner
    p1self    <- .data[.data$P_num == 1, paste0("self_", var), drop = T]
    p1partner <- .data[.data$P_num == 1, paste0("partner_", var), drop = T]
    p2self    <- .data[.data$P_num == 2, paste0("self_", var), drop = T]
    p2partner <- .data[.data$P_num == 2, paste0("partner_", var), drop = T]
    
    # compute correlation and p-value
    #   actual_sim: between both partners' self-reports
    #   perceived_sim_p1: P1's self perception and 
    #                     P1's perception of P2
    #   perceived_sim_p2: P2's self perception and 
    #                     P2's perception of P1
    actual_sim <- cor.test(p1self, p2self, method = "pearson",
                           alternative = "two.sided")    
    perceived_sim_p1 <- cor.test(p1self, p1partner, method = "pearson",
                                 alternative = "two.sided")    
    perceived_sim_p2 <- cor.test(p2self, p2partner, method = "pearson",
                                 alternative = "two.sided")    
    # store cor, ci, pval
    similarity_df <- rbind(
      similarity_df,
      # actual sim
      c("actual", var, 
        paste0(round(actual_sim$estimate, 3),
               " [", round(actual_sim$conf.int[1], 3),
               " - ", 
               round(actual_sim$conf.int[2], 3) ,"]"),
        round(actual_sim$p.value, 3)),
      c("P1-perceived", var,
        paste0(round(perceived_sim_p1$estimate, 3),
               " [", round(perceived_sim_p1$conf.int[1], 3),
               " - ", 
               round(perceived_sim_p1$conf.int[2], 3) ,"]"),
        round(perceived_sim_p1$p.value, 3)),
      c("P2-perceived", var,
        paste0(round(perceived_sim_p2$estimate, 3),
               " [", round(perceived_sim_p2$conf.int[1], 3),
               " - ", 
               round(perceived_sim_p2$conf.int[2], 3) ,"]"),
        round(perceived_sim_p2$p.value, 3))
    )
    
    # create data frame with all perception pairs
    sim_list <- as.data.frame(
      matrix(c("actual",           "P1-perceived",
               "actual",           "P2-perceived",
               "P1-perceived", "P2-perceived"),
             ncol = 2, byrow = TRUE)
    )
    
    # store current personality variable
    sim_list$personality <- var
    
    # fisher's z transformed of each similarity
    actual_z <- fisherz(rho = actual_sim$estimate)
    P1_z <- fisherz(rho = perceived_sim_p1$estimate)
    P2_z   <- fisherz(rho = perceived_sim_p2$estimate)
    
    # resolve infinity problem
    if(actual_z == Inf){actual_z <- 0.99999}
    if(P1_z == Inf){P1_z <- 0.99999}
    if(P2_z == Inf){P2_z <- 0.99999}
    if(actual_z == -Inf){actual_z <- -0.99999}
    if(P1_z == -Inf){P1_z <- -0.99999}
    if(P2_z == -Inf){P2_z <- -0.99999}
    
    # sample sizes
    actual_n <- actual_sim$parameter + 2
    P1_n <- perceived_sim_p1$parameter + 2
    P2_n   <- perceived_sim_p2$parameter + 2
    
    # z tests
    sim_list[1, "z_stat"] <- (actual_z - P1_z) / 
      sqrt( (1/(actual_n - 3)) + (1/(P1_n - 3)) )
    sim_list[2, "z_stat"] <- (actual_z - P2_z) / 
      sqrt( (1/(actual_n - 3)) + (1/(P2_n - 3)) )
    sim_list[3, "z_stat"] <- (P1_z - P2_z) / 
      sqrt( (1/(P1_n - 3)) + (1/(P2_n - 3)) )
    
    # significance
    sim_list$sig <- abs(sim_list$z_stat) > 1.96
    
    # rbind current sim_list to compare_df
    compare_df <- rbind(
      compare_df,
      sim_list
    )

  } # END for var LOOP
  
  # return results
  names(similarity_df) <- c("similarity", "personality",
                            "correlation", "p-value")
  return(list(similarity_df = similarity_df,
              compare_df = compare_df))
} # END h2_function

## Hypothesis 3: Dynamic Assortment --------------------------------------------
h3_function <- function(
  # character vector of variables
  var_list,
  
  # esm data frame
  .data,  
  
  # esm dictionary
  .dict
  ) {
  
  # initialize cor table
  cor_tab <- data.frame()

  # sort data frame to make sure coupleIDs are together
  .data <- .data %>% arrange(Couple_ID)
  
  # initialize within_df to store all momentary indices
  #   to be merged back to baseline dataframe
  within_df <- data.frame(
    Couple_ID = rep(unique(.data$Couple_ID), times = 2),
    P_num = rep(1:2, each = length(unique(.data$Couple_ID)))
  )
  
  # loop through variables
  for (ivar in var_list) {

    # sd calculation
    p1_sd <- tapply(X = .data[.data$P_num == 1, ivar],
                    INDEX = .data[.data$P_num == 1,"Couple_ID"],
                    FUN = function(x) sd(x, na.rm = T))
    p2_sd <- tapply(X = .data[.data$P_num == 2, ivar],
                    INDEX = .data[.data$P_num == 2,"Couple_ID"],
                    FUN = function(x) sd(x, na.rm = T))

    # sd correlation
    sd_cor <- cor.test(p1_sd, p2_sd, method = "pearson",
                       alternative = "two.sided")
    
    
    # pull scale min and max
    scale_min = .dict %>% 
      filter(var == ivar | scale_label == ivar) %>%
      pull(scale_min) %>% unique() %>% as.numeric()
    scale_max = .dict %>% filter(var == ivar | scale_label == ivar) %>%
      pull(scale_max) %>% unique() %>% as.numeric()

    # rvi calculation
    p1_rvi <- tapply(X = .data[.data$P_num == 1, ivar],
                     INDEX = .data[.data$P_num == 1,"Couple_ID"],
                     FUN = function(x) rvi_calc(vector = x,
                                                min = scale_min,
                                                max = scale_max))
    p2_rvi <- tapply(X = .data[.data$P_num == 2, ivar],
                     INDEX = .data[.data$P_num == 2,"Couple_ID"],
                     FUN = function(x) rvi_calc(vector = x,
                                                min = scale_min,
                                                max = scale_max))
  
    # rvi correlation
    rvi_cor <- cor.test(p1_rvi, p2_rvi, method = "pearson",
                        alternative = "two.sided")
  
    # mssd calculation  
    p1_mssd <- tapply(X = .data[.data$P_num == 1, ivar],
                      INDEX = .data[.data$P_num == 1,"Couple_ID"],
                      FUN = mssd)
    p2_mssd <- tapply(X = .data[.data$P_num == 2, ivar],
                      INDEX = .data[.data$P_num == 2,"Couple_ID"],
                      FUN = mssd)
  
    # mssd correlation  
    mssd_cor <- cor.test(p1_mssd, p2_mssd, method = "pearson",
                         alternative = "two.sided")
    
    # profile correlation
    #   initialize vectors of r, p, and n
    r_vector <- p_vector <- n_vector <- rep(NA, length(unique(.data$Couple_ID)))

    #   loop through couple
    for(idxcouple in 1:length(unique(.data$Couple_ID))) {
      
      # store current couple ID
      icouple <- unique(.data$Couple_ID)[idxcouple]
      
      # extract vectors of shared time points
      current_couple <- .data %>% filter(Couple_ID == icouple) %>%
        select(time_idx, P_num, !!sym(ivar)) %>%
        na.omit()
      common_time <- names(table(current_couple$time_idx)[
        table(current_couple$time_idx) == 2])
      p1_df <- current_couple %>% 
        filter(time_idx %in% common_time & P_num == 1) %>%
        arrange(time_idx) 
      p2_df <- current_couple %>% 
        filter(time_idx %in% common_time & P_num == 2) %>%
        arrange(time_idx)
      common_time <- intersect(p1_df$time_idx, p2_df$time_idx)
      p1_vector <- p1_df %>%
        filter(time_idx %in% common_time) %>%
        pull(!!sym(ivar))
      p2_vector <- p2_df %>%
        filter(time_idx %in% common_time) %>%
        pull(!!sym(ivar))
      
      # calculate profile correlations
      prof_cor <- cor.test(p1_vector, p2_vector, method = "pearson",
                           alternative = "two.sided")
      
      # store values in results vectors
      r_vector[idxcouple] <- round(prof_cor$estimate, 3)
      p_vector[idxcouple] <- round(prof_cor$p.value, 3)
      n_vector[idxcouple] <- prof_cor$parameter + 2
    } # END for idxcouple LOOP

    # store values for results table
    cor_tab <- rbind(cor_tab, c(ivar,
                                # sd
                                round(sd_cor$estimate, 3), 
                                round(sd_cor$p.value, 3),
                                round(sd_cor$conf.int[1], 3),
                                round(sd_cor$conf.int[2], 3),
                                # rvi
                                round(rvi_cor$estimate, 3), 
                                round(rvi_cor$p.value, 3),
                                round(rvi_cor$conf.int[1], 3),
                                round(rvi_cor$conf.int[2], 3),
                                # mssd
                                round(mssd_cor$estimate, 3), 
                                round(mssd_cor$p.value, 3),
                                round(mssd_cor$conf.int[1], 3),
                                round(mssd_cor$conf.int[2], 3),
                                # prof cor
                                round(mean(r_vector, na.rm=T), 3),
                                round(mean(p_vector[p_vector > 0] < .05,
                                           na.rm=T), 3),
                                round(mean(n_vector), 3)))
    
    # store momentary indices for all participants in dataframe
    within_df$sd   <- c(p1_sd, p2_sd)
    within_df$rvi  <- c(p1_rvi, p2_rvi)
    within_df$mssd <- c(p1_mssd, p2_mssd)
    names(within_df)[names(within_df) == "sd"]   <- paste0(ivar, "_sd")
    names(within_df)[names(within_df) == "rvi"]  <- paste0(ivar, "_rvi")
    names(within_df)[names(within_df) == "mssd"] <- paste0(ivar, "_mssd")
    
    # store within-couple correlations in dataframe
    within_df$cor <- rep(r_vector, times = 2)
    within_df$z   <- rep(fisherz(rho = r_vector), times = 2)
    within_df$p   <- rep(p_vector, times = 2)
    within_df$n   <- rep(n_vector, times = 2)
    names(within_df)[names(within_df) == "cor"]   <- paste0(ivar, "_r")
    names(within_df)[names(within_df) == "z"]     <- paste0(ivar, "_z")
    names(within_df)[names(within_df) == "p"]     <- paste0(ivar, "_p")
    names(within_df)[names(within_df) == "n"]     <- paste0(ivar, "_n")

  } # END for ivar LOOP
  
  # rename cor_tab output
  names(cor_tab) <- c("variable",
                      "r SD", "p SD", "LL SD", "UL SD",
                      "r RVI", "p RVI", "LL RVI", "UL RVI",
                      "r MSSD", "p MSSD", "LL MSSD", "UL MSSD",
                      "r avg", "proportion sig", "n avg")
  
  # return results
  return(list(cor_tab = cor_tab,
              within_df = within_df))
} # END h3_function DEF

## Hypothesis 4: Baseline Benefits ---------------------------------------------
h4_function <- function(
  
  # character vector of personality variables
  var_list, 
  
  # character vector of profile names
  #   should match in df: 
  #     profilename_raw_r, profilename_centered_r, profilename_std_r
  prof_list = NULL,
  
  # character for relationship quality variable
  quality_var, 
  
  # baseline dataframe
  .data) {
  
  # create data frame to store results
  # INTERACTION_TAB for multiple regression
  #   nrow = # personality variables X 2 participants
  #   ncol = 11
  #     2 identifiers for personality variables + P_num
  #     3 variables for actor effect b, t, and p-value
  #     3 variables for partner effect b, t, and p-value
  #     3 variables for interaction effect b, t, and p-value
  interaction_tab <- data.frame()
  # DIFFERENCE_TAB for absolute difference simple regression
  #   nrow = # personality variables X 2 participants
  #   ncol = 5
  #     2 identifiers for personality variables + P_num
  #     3 variables for linear slope b, t, and p-value
  difference_tab <- data.frame()
  # PROFILE_TAB for multiple regression
  #   nrow = # profiles X 2 participants
  #   ncol = 6
  #     2 identifiers for personality variables + P_num
  #     9 variables for (linear slope b, t, and p-value) x 3 profiles raw, cent, std
  profile_tab <- data.frame()
  
  # sort dataframe to ensure CoupleID allign
  .data <- .data %>% arrange(Couple_ID)
    
  # extract vector of each partner score on relationship quality variables
  p1_qual <- .data[.data$P_num == 1, quality_var, drop = T]
  p2_qual <- .data[.data$P_num == 2, quality_var, drop = T]
  
  # iterate through personality variables
  for (var in var_list) {
    # extract vector of each partner score on personality variables
    p1_var <- .data[.data$P_num == 1, var, drop = T]
    p2_var <- .data[.data$P_num == 2, var, drop = T]
    
    ### MULTIPLE REGRESSION ###
    
    # fit multiple regression models
    mod_1 <- summary(lm(p1_qual ~ p1_var * p2_var))$coefficients
    mod_2 <- summary(lm(p2_qual ~ p2_var * p1_var))$coefficients
    
    # extract coefficients
    actor_1   <- round(mod_1["p1_var", 
                             c("Estimate", "t value", "Pr(>|t|)")], 3)
    actor_2   <- round(mod_2["p2_var", 
                             c("Estimate", "t value", "Pr(>|t|)")], 3)
    partner_1 <- round(mod_1["p2_var", 
                             c("Estimate", "t value", "Pr(>|t|)")], 3)
    partner_2 <- round(mod_2["p1_var", 
                             c("Estimate", "t value", "Pr(>|t|)")], 3)
    int_1     <- round(mod_1["p1_var:p2_var", 
                             c("Estimate", "t value", "Pr(>|t|)")], 3)
    int_2     <- round(mod_2["p2_var:p1_var", 
                             c("Estimate", "t value", "Pr(>|t|)")], 3)
    
    # store values in interaction_tab
    interaction_tab <- rbind(
      interaction_tab,
      
      # first partner
      c(var, "P1", actor_1, partner_1, int_1),
      
      # second partner
      c(var, "P2", actor_2, partner_2, int_2)
    )
    
    ### ABSOLUTE DIFFERENCE ###
    
    # create a vector of absolute difference scores
    diff_var <- abs(p1_var - p2_var)
    
    # fit simple regression models
    mod_1 <- summary(lm(p1_qual ~ diff_var))$coefficients
    mod_2 <- summary(lm(p2_qual ~ diff_var))$coefficients
    
    # extract coefficients
    diff_1   <- round(mod_1["diff_var", 
                            c("Estimate", "t value", "Pr(>|t|)")], 3)
    diff_2   <- round(mod_2["diff_var", 
                            c("Estimate", "t value", "Pr(>|t|)")], 3)
    
    # store values in difference_tab
    difference_tab <- rbind(
      difference_tab,
      
      # first partner
      c(var, "P1", diff_1),
      
      # second partner
      c(var, "P2", diff_2)
    )
  } # END for var LOOP
  
  ### PROFILE REGRESSION ###
  
  # iterate through profile list
  for (prof in prof_list) {
    
    # extract profile correlations and transform to fisher's z scores
    # filter by P_num == 1 because it should be the same for both partners
    # but data are in long format
    profz_raw <- fisherz(
      rho = .data[.data$P_num == 1, paste0(prof, "_raw_r"), drop = T]
    )
    profz_centered <- fisherz(
      rho = .data[.data$P_num == 1, paste0(prof, "_centered_r"), drop = T]
    )
    profz_std <- fisherz(
      rho = .data[.data$P_num == 1, paste0(prof, "_std_r"), drop = T]
    )
    
    # fix inifinity problem
    profz_raw[profz_raw == Inf] <- 0.99999
    profz_centered[profz_centered == Inf] <- 0.99999
    profz_std[profz_std == Inf] <- 0.99999
    profz_raw[profz_raw == -Inf] <- -0.99999
    profz_centered[profz_centered == -Inf] <- -0.99999
    profz_std[profz_std == -Inf] <- -0.99999
    
    # fit simple regression models
    mod_raw_1 <- summary(lm(p1_qual ~ profz_raw))$coefficients
    mod_raw_2 <- summary(lm(p2_qual ~ profz_raw))$coefficients
    mod_cen_1 <- summary(lm(p1_qual ~ profz_centered))$coefficients
    mod_cen_2 <- summary(lm(p2_qual ~ profz_centered))$coefficients
    mod_std_1 <- summary(lm(p1_qual ~ profz_std))$coefficients
    mod_std_2 <- summary(lm(p2_qual ~ profz_std))$coefficients
    
    # extract coefficients
    raw_1   <- round(mod_raw_1["profz_raw", 
                              c("Estimate", "t value", "Pr(>|t|)")], 3)
    raw_2   <- round(mod_raw_2["profz_raw", 
                              c("Estimate", "t value", "Pr(>|t|)")], 3)
    cen_1   <- round(mod_cen_1["profz_centered", 
                              c("Estimate", "t value", "Pr(>|t|)")], 3)
    cen_2   <- round(mod_cen_2["profz_centered", 
                              c("Estimate", "t value", "Pr(>|t|)")], 3)
    std_1   <- round(mod_std_1["profz_std", 
                              c("Estimate", "t value", "Pr(>|t|)")], 3)
    std_2   <- round(mod_std_2["profz_std", 
                              c("Estimate", "t value", "Pr(>|t|)")], 3)
    # store values in profile_tab
    profile_tab <- rbind(
      profile_tab,
      
      # first partner
      c(prof, "P1", raw_1, cen_1, std_1),
      
      # second partner
      c(prof, "P2", raw_2, cen_2, std_2)
    )
  } # END for profile LOOP

  # rename interaction_tab columns
  names(interaction_tab) <- c("personality", "participant",
                              "actor_est", "actor_tval", "actor_pval",
                              "partner_est", "partner_tval", "partner_pval",
                              "int_est", "int_tval", "int_pval")
  # rename difference_tab columns
  names(difference_tab) <- c("personality", "participant",
                             "diff_est", "diff_tval", "diff_pval")
  
  # rename profile_tab columns
  if(!is.null(prof_list)) {
    names(profile_tab) <- c("profile", "participant",
                          "raw_est", "raw_tval", "raw_pval",
                          "cen_est", "cen_tval", "cen_pval",
                          "std_est", "std_tval", "std_pval")
  }
  
  # return results
  return(list(interaction_tab = interaction_tab,
              difference_tab = difference_tab,
              profile_tab = profile_tab))
  
} # END h4_function

## Hypothesis 5+6: APIM ----------------------------------------------------------
h5_function <- function(
  # character vector of all personality variables  
  var_list,
  
  # character for relationship quality variable
  quality_var,
  
  # analytic dataframe
  .data) {
  
  # create df to store results for actual and P1- and P2-perceived similarity
  est_df <- est_df_p1 <- est_df_p2 <- data.frame()
  
  # create a wide dataframe - 1 row = 1 couple
  wide_df <- .data %>% select(Couple_ID, P_num, self_neuro:partner_opena) %>%
    pivot_wider(id_cols = Couple_ID,
                names_from = P_num,
                values_from = c(self_neuro:partner_opena))
  
  # loop through personality variables
  for(var in var_list){
    wide_df <- wide_df %>%
      
      # create difference score for personality variables
      mutate(!!sym(paste0(var, "_diff")) := 
               abs(!!sym(paste0("self_", var, "_1")) - 
                   !!sym(paste0("self_", var, "_2")))) %>%
      
      # var_diff_p1: perceived dissimilarity between p1's self-perception 
      #              and p1's perception of p2
      # var_diff_p2: perceived dissimilarity between p2's self-perception
      #              and p2's perception of p1
      mutate(!!sym(paste0(var, "_diff_p1")) :=
               abs(!!sym(paste0("self_", var, "_1")) - 
                   !!sym(paste0("partner_", var, "_1"))),
             !!sym(paste0(var, "_diff_p2")) :=
               abs(!!sym(paste0("self_", var, "_2")) - 
                   !!sym(paste0("partner_", var, "_2"))))
    
    #### ACTUAL SIMILARITY ####
    
    # create APIM model object
    model <- paste(
      # P1 actor effect
      paste0(quality_var, "_1", " ~ a1*", "self_", var, "_1"),
      
      # P2 actor effect
      paste0(quality_var, "_2", " ~ a2*", "self_", var, "_2"),
      
      # P1 to P2 partner effect
      paste0(quality_var, "_2", " ~ p12*", "self_", var, "_1"),
      
      # P2 to P1 partner effect
      paste0(quality_var, "_1", " ~ p21*", "self_", var, "_2"),
      
      # dissimilarity to P1 effect
      paste0(quality_var, "_1", " ~ d1*", var, "_diff"),
      
      # dissimilarity to P2 effect
      paste0(quality_var, "_2", " ~ d2*", var, "_diff"),
      
      # P1 mean personality
      paste0("self_", var, "_1", " ~ m1*1"),
      
      # P2 mean personality
      paste0("self_", var, "_2", " ~ m2*1"),
      
      # dissimilarity mean
      paste0(var, "_diff", " ~ md*1"),
      
      # P1 intercept
      paste0(quality_var, "_1", " ~ i1*1"),
      
      # P2 intercept
      paste0(quality_var, "_2", " ~ i2*1"),
        
      # P1 variance for personality
      paste0("self_", var, "_1", " ~~ v1*", "self_", var, "_1"),
      
      # P2 variance for personality
      paste0("self_", var, "_2", " ~~ v2*", "self_", var, "_2"),
      
      # dissimilarity variance
      paste0(var, "_diff", " ~~ vd*", var, "_diff"),
      
      # P1 error variance for relationship quality
      paste0(quality_var, "_1", " ~~ e1*", quality_var, "_1"),
      
      # P2 error variance for relationship quality
      paste0(quality_var, "_2", " ~~ e2*", quality_var, "_2"),
      
      # covariance between P1 and P2 personality
      paste0("self_", var, "_1", " ~~ v12*", "self_", var, "_2"),
        
      # covariance between P1 personality and dissimilarity
      paste0("self_", var, "_1", " ~~ v1d*", var, "_diff"),
      
      # covariance between P2 personality and dissimilarity
      paste0("self_", var, "_2", " ~~ v2d*", var, "_diff"),
      
      # error covariance between P1 and P2 relationship quality
      paste0(quality_var, "_1", " ~~ e12*", quality_var, "_2"),
      
      # each line is separated by a line break
      sep = " \n "
    ) # END model DEF
    
    # fit lavaan model
    fit <- lavaan(model, data = wide_df, missing = "FIML")
    
    # standardized actor/partner/similarity solutions
    est <- as.data.frame(unclass(standardizedSolution(fit))) %>% 
      filter(op == "~") %>%
       mutate_if(is.numeric, function(x){round(x,3)})
    
    # store results in results df
    est_df <- rbind(
      est_df, est
      )
    
    #### PERCEIVED SIMILARITY ####
    
    # model_p1 includes perceptions of partner p1
    model_p1 <- paste(
      
      # P1 actor effect
      paste0(quality_var, "_1", " ~ a1*", "self_", var, "_1"),
      
      # P1-perception actor effect
      paste0(quality_var, "_2", " ~ ap1*", "partner_", var, "_1"),
      
      # P1 to P2 partner effect
      paste0(quality_var, "_2", " ~ p12*", "self_", var, "_1"),
      
      # P1-perception to P1 partner effect
      paste0(quality_var, "_1", " ~ p21*", "partner_", var, "_1"),
      
      # dissimilarity to P1 effect
      paste0(quality_var, "_1", " ~ d1*", var, "_diff_p1"),
      
      # dissimilarity to P2 effect
      paste0(quality_var, "_2", " ~ d2*", var, "_diff_p1"),
      
      # P1 mean personality
      paste0("self_", var, "_1", " ~ m1*1"),
      
      # P1-perception mean personality
      paste0("partner_", var, "_1", " ~ mp1*1"),
      
      # dissimilarity mean
      paste0(var, "_diff_p1", " ~ md1*1"),
      
      # P1 intercept
      paste0(quality_var, "_1", " ~ i1*1"),
      
      # P2 intercept
      paste0(quality_var, "_2", " ~ i2*1"),
        
      # P1 variance for personality
      paste0("self_", var, "_1", " ~~ v1*", "self_", var, "_1"),
      
      # P1-perception variance for personality
      paste0("partner_", var, "_1", " ~~ vp1*", "partner_", var, "_1"),
      # dissimilarity variance
      paste0(var, "_diff_p1", " ~~ vd1*", var, "_diff_p1"),
      
      # P1 error variance for relationship quality
      paste0(quality_var, "_1", " ~~ e1*", quality_var, "_1"),
      
      # P2 error variance for relationship quality
      paste0(quality_var, "_2", " ~~ e2*", quality_var, "_2"),
      
      # covariance between P1 and P1-perception personality
      paste0("self_", var, "_1", " ~~ v1p1*", "partner_", var, "_1"),
        
      # covariance between P1 personality and dissimilarity
      paste0("self_", var, "_1", " ~~ v1d1*", var, "_diff_p1"),
      
      # covariance between P1-perception personality and dissimilarity
      paste0("partner_", var, "_1", " ~~ vp1d1*", var, "_diff_p1"),
      
      # error covariance between P1 and P2 relationship quality
      paste0(quality_var, "_1", " ~~ e12*", quality_var, "_2"),
      
      # each line is separated by a line break
      sep = " \n "
    ) # END model_p1 DEF
    
    # model_p2 includes perceptions of partner p2
    model_p2 <- paste(
      
      # P2 actor effect
      paste0(quality_var, "_2", " ~ a2*", "self_", var, "_2"),
      
      # P2-perception actor effect
      paste0(quality_var, "_1", " ~ ap2*", "partner_", var, "_2"),
      
      # P2 to P1 partner effect
      paste0(quality_var, "_1", " ~ p21*", "self_", var, "_2"),
      
      # P2-perception to P2 partner effect
      paste0(quality_var, "_2", " ~ p21*", "partner_", var, "_2"),
      
      # dissimilarity to P1 effect
      paste0(quality_var, "_1", " ~ d1*", var, "_diff_p2"),
      
      # dissimilarity to P2 effect
      paste0(quality_var, "_2", " ~ d2*", var, "_diff_p2"),
      
      # P2 mean personality
      paste0("self_", var, "_2", " ~ m2*1"),
      
      # P2-perception mean personality
      paste0("partner_", var, "_2", " ~ mp2*1"),
      
      # dissimilarity mean
      paste0(var, "_diff_p2", " ~ md2*1"),
      
      # P1 intercept
      paste0(quality_var, "_1", " ~ i1*1"),
      
      # P2 intercept
      paste0(quality_var, "_2", " ~ i2*1"),
        
      # P2 variance for personality
      paste0("self_", var, "_2", " ~~ v2*", "self_", var, "_2"),
      
      # P2-perception variance for personality
      paste0("partner_", var, "_2", " ~~ vp2*", "partner_", var, "_2"),
      
      # dissimilarity variance
      paste0(var, "_diff_p2", " ~~ vd2*", var, "_diff_p2"),
      
      # P1 error variance for relationship quality
      paste0(quality_var, "_1", " ~~ e1*", quality_var, "_1"),
      
      # P2 error variance for relationship quality
      paste0(quality_var, "_2", " ~~ e2*", quality_var, "_2"),
      
      # covariance between P2 and P2-perception personality
      paste0("self_", var, "_2", " ~~ v2p2*", "partner_", var, "_2"),
        
      # covariance between P2 personality and dissimilarity
      paste0("self_", var, "_2", " ~~ v2d2*", var, "_diff_p2"),
      
      # covariance between P2-perception personality and dissimilarity
      paste0("partner_", var, "_2", " ~~ vp2d2*", var, "_diff_p2"),
      
      # error covariance between P1 and P2 relationship quality
      paste0(quality_var, "_1", " ~~ e12*", quality_var, "_2"),
      
      # each line is separated by a line break
      sep = " \n "
    ) # END model_p2 DEF
    
    # fit lavaan model
    fit_p1 <- lavaan(model_p1, data = wide_df, missing = "FIML")
    fit_p2 <- lavaan(model_p2, data = wide_df, missing = "FIML")
    
    # standardized actor/partner/similarity solutions
    est_p1 <- as.data.frame(unclass(standardizedSolution(fit_p1))) %>% 
      filter(op == "~") %>%
      mutate_if(is.numeric, function(x){round(x,3)}) %>%
      mutate("perception" = "P1") %>%
      relocate(perception)
    est_p2 <- as.data.frame(unclass(standardizedSolution(fit_p2))) %>% 
      filter(op == "~") %>%
      mutate_if(is.numeric, function(x){round(x,3)}) %>%
      mutate("perception" = "P2") %>%
      relocate(perception)
    
    # store results
    est_df_p1 <- rbind(
      est_df_p1, est_p1
    )
    est_df_p2 <- rbind(
      est_df_p2, est_p2
    )
  } # END for var LOOP
  
  # return results
  return(list(actual = est_df,
              perceived_p1 = est_df_p1,
              perceived_p2 = est_df_p2))
} # END h5_function

## Exploratory Analysis: Coupled Damped Oscillator -----------------------------
rties_function <- function() {
  
}

# Cleaning Functions -----------------------------------------------------------

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
    select(var, scale_max, scale_min)
  
  # reverse all appropriate items
  for (i in 1:nrow(reverse_dict)) {
    
    # grab the column name and scale_max value
    var        <- reverse_dict[i, "var"]
    scale_max  <- reverse_dict[i, "scale_max"]
    scale_min  <- reverse_dict[i, "scale_min"]
    
    # reverse item in data
    .data[var] <- scale_max + scale_min - .data[var]
  
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
      # return NA if any item is NA
      scored_vector <- rowSums(.data[, var_list])
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
      # return NA if any item is NA
      scored_vector <- rowSums(.data[, var_list])
    } # END if score_method STATEMENT
    
    # append new vector to original data frame
    .data <- cbind(.data, scored_vector)
    
    # rename to align with scale label
    names(.data)[names(.data) == "scored_vector"] <- scale_label
    
  } # END for current_scale LOOP
  
  # change any NaN to NA
  .data[] <- lapply(.data, function(x) ifelse(is.nan(x), NA, x))
  
  # change date back to POSIX code
  if("day" %in% names(.data)){
    .data$day <- as.Date(.data$day, origin = "1970-01-01")
  }
  
  # return scored dataframe
  return(.data)
}

# Miscellaneous Functions ------------------------------------------------------

# function to test significance, 2-tailed paired t-test
# return 0/1 for signif
signif <- function(var, data, var_p = "P_num", p1_p2 = c(1, 2)) {
  pval <- t.test(x = data[order(data$Couple_ID),][data[var_p] == p1_p2[1], var],
                 y = data[order(data$Couple_ID),][data[var_p] == p1_p2[2], var],
                 paired = TRUE,
                 alternative = "two.sided",
                 conf.level = 0.95)$p.value
  signif <- isTRUE(pval < 0.05)
  return(signif)
} # END signif


# function for histogram
plot_hist <- function(var, var_name, data, bin_width,
                      var_p = "P_num", p1_p2 = c(1, 2)) {
  # custom colors for female and males
  colors <- c("#E69F00", "#009E73")
  names(colors) <- c("p1", "p2")
  color_text <- glue::glue(
    'for <span style = color:{colors["p1"]}>**participant 1 scores**</span> ',
    'and ',
    '<span style = color:{colors["p2"]}>**participant 2 scores**</span>'
  )
  
  p <- ggplot(data = data, 
              aes(!!sym(var))) +
    geom_histogram(binwidth = bin_width, fill = "#710c0c") + 
    geom_vline(xintercept = 
                 mean(data[data[var_p] == p1_p2[1], var],
                      na.rm = TRUE),
               color = colors["p1"], size = 1) +
    geom_vline(xintercept = 
                 mean(data[data[var_p] == p1_p2[2], var],
                      na.rm = TRUE),
               color = colors["p2"], size = 1) +
    labs(
      title = paste("Distribution of", var_name, "<br>", color_text),
      x = var_name,
      y = NULL
    ) +
    theme_classic() +
    theme(
      plot.title = element_markdown()
    )
  return(p)
} # END plot_hist

# function for forest plot

plot_forest <- function(  
  
  # character vector of all personality variables with self/other reports
  #   naming convention: self_var and partner_var
  perception_list,
  
  # data frame for similarities results
  #   h6_function similarity_df format
  similarity_df,
  
  # perception type - default to P1 perception
  perception = "P1-perceived"
  ) {

  # clean up similarity df
  similarity_df <- similarity_df[similarity_df$similarity %in% 
                                   c("actual", perception),]
  similarity_df[similarity_df$similarity == perception, "personality"] <- ""
  similarity_df$correlation <- gsub("\\[|\\]", "", similarity_df$correlation)
  similarity_df <- separate(
    similarity_df, correlation, 
    into = c("correlation", "lower_bound", "extra", "upper_bound"), 
    sep = " ")
  
  # create df for forest plot
  plot_df <- tibble(
    variable = similarity_df$personality,
    mean = as.numeric(similarity_df$correlation),
    lower = as.numeric(similarity_df$lower_bound),
    upper = as.numeric(similarity_df$upper_bound),
    similarity = sub("0.", ".", as.character(
      round(as.numeric(similarity_df$correlation), 2)))
  )
  
  # create arguments with alternating styles for actual and perceived similarity
  num_shapes <- length(perception_list) * 2

  # Initialize empty lists to store gpar objects
  lines_list <- list()
  box_list <- list()
  
  # Generate gpar objects for lines and boxes
  for (i in 1:num_shapes) {
    # Alternate colors for lines
    line_color <- ifelse(i %% 2 == 0, "#710c0c", "#E69F00")
    
    # Alternate line styles
    line_style <- ifelse(i %% 2 == 0, "longdash", "solid")
    
    # Create gpar object for lines
    lines_list[[i]] <- gpar(col = line_color, lty = line_style)
    
    # Choose fill color for boxes
    box_fill <- ifelse(i %% 2 == 0, "#710c0c", "#E69F00")
    
    # Create gpar object for boxes
    box_list[[i]] <- gpar(fill = box_fill)
  }
  
  # Combine lists into fpShapesGp arguments
  plot_styles <- fpShapesGp(lines = lines_list, box = box_list)
  
  # create forest plot
  plot_df %>%
    forestplot(labeltext = c(variable, similarity),
               zero = 0, boxsize = 0.5,
               shapes_gp = plot_styles)
} # END plot_forest

# function to calculate relative variability index for SD
# from relativeVariability package
# https://ppw.kuleuven.be/okp/software/relative_variability/
rvi_calc <- function(vector, min, max){
  if(sum(!is.na(vector)) < 2) {
    rsd <- NaN
  } else {
    idx <- which(is.na(vector) == 0)
    vector <- vector[idx]
    M <- mean(vector)
    SD <- sd(vector)
    n <- length(vector)
    if (M == min || M == max) {
      mv = 0
    } else if (abs(min) > abs(max)){
      MINt <- -max
      max <- -min
      min <- MINt
      M <- -M
    }
    nMax <- floor((n * M - n * min)/(max - min))
    nMin <- n - 1 - nMax
    if (nMax == 0) {
      max <- 0
    }
    m <- n * M - nMin * min - nMax * max
    mv <- (nMin * (min - M)^2 + nMax * (max - M)^2 + (M - m)^2)/(n - 1)
    msd <- sqrt(mv)
    if (msd != 0) {
      rsd = SD/msd
    }
    else {
      rsd <- NaN
    }  
  }
  return(rsd)
}

# function to plot raw time series data
# from rties package
# adapted to change colors
plot_timeseries <- function(
    basedata, dyadId, obs_name, dist_name, time_name, dist0name = NULL, 
    dist1name = NULL, plot_obs_name = NULL, printPlots = T, .title = NULL) {
  
  basedata <- basedata[, c(dyadId, obs_name, dist_name, time_name)]
  names(basedata) <- c("dyad", "obs", "dist", "time")
  if (!is.numeric(basedata$dist)) {
    stop("the distinguishing variable must be a 0/1 numeric variable")
    }
  if (is.null(dist0name)) {
    dist0name <- "dist0"
    }
  if (is.null(dist1name)) {
    dist1name <- "dist1"
    }
  if (is.null(plot_obs_name)) {
    plot_obs_name <- "obs"
    }
  dist <- NULL
  plots <- lattice::xyplot(
    obs ~ time | as.factor(dyad), data = basedata, 
    group = dist, type = c("l"), ylab = plot_obs_name, 
    col = c("#710c0c", "#E69F00"),
    lty = c("longdash", "solid"),
    key = list(space = "right", text = list(c(dist1name, dist0name)),
               col = c("#710c0c", "#E69F00"),
               lty = c("longdash", "solid")), as.table = T,
    layout = c(3, 3),
    main = .title)
  if (printPlots == T) {
    print(plots)
  }
  return(plots)
  }
