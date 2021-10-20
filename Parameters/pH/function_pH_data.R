cont_ph_raw <- function(database) {
  
  
  require(tidyverse)
  require(RODBC)
  require(odeqIRtools)
  
  print("Fetch continuous pH data from IR database")
  #connect to IR database view as a general user
  # import bacteria data
  database = "IR_Dev"
  IR.sql <-   odbcConnect(database)
  
  
  Results_import_cont <-    sqlFetch(IR.sql, "VW_pHCont") %>%
    dplyr::filter(!is.na(Result_Numeric))
  Results_import_grab <-    sqlFetch(IR.sql, "VW_pH")%>%
    dplyr::filter(!is.na(Result_Numeric))
  
  odbcClose(IR.sql)
  
  print(paste("Fetched", nrow(Results_import_cont), "continuous results from", length(unique(Results_import_cont$MLocID)), "monitoring locations" ))
  print(paste("Fetched", nrow(Results_import_grab), "grab results from", length(unique(Results_import_grab$MLocID)), "monitoring locations" ))
  
  # Set factors to characters
  Results_import_cont %>% map_if(is.factor, as.character) %>% as_tibble -> Results_import_cont
  Results_import_grab %>% map_if(is.factor, as.character) %>% as_tibble -> Results_import_grab
  
  
  Results_import_grab <- odeqIRtools::data_aggregation(Results_import_grab)
  
  # Data censoring --------------------------------------------------------------------------------------------------
  #LAM commented this out as we don't censor data for pH
  # print("Data censor process")
  # # Get all the standards to be used when dealing with the censored data
  # Results_crit <- Results_import_cont %>%
  #   # Get lowest criteria value to set censored results
  #   mutate(lowest_crit = pmin(SS_Crit, Geomean_Crit, Perc_Crit, na.rm = TRUE))
  # 
  # Results_censored <- censor_data(Results_crit, crit = lowest_crit)
  # 
  # print("Data censor process")
  # # Get all the standards to be used when dealing with the censored data
  # Results_crit <- Results_import_grab %>%
  #   # Get lowest criteria value to set censored results
  #   mutate(lowest_crit = pmin(SS_Crit, Geomean_Crit, Perc_Crit, na.rm = TRUE))
  # 
  # Results_censored <- censor_data(Results_crit, crit = lowest_crit)
  # 
  # 
  # return(Results_censored)
  # 
  
  pH_list <- list(pH_cont = Results_import_cont,
                  ph_grab = Results_import_grab)
  
}