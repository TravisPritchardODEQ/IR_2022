require(rgdal)
require(RODBC)
library(tidyverse)
library(odeqIRtools)
library(openxlsx)




temp_data <- function(database) {
  print("Fetch temp data from IR database")
  #connect to IR database view as a general user
  # import Temperature data
  IR.sql <-   odbcConnect(database)
  
  
  # Get data from IR database where wqstd_code = 12, ResStatusName = Final, 
  # Join with Crit_Temp to get temperature Criteria and spawn ?
  Results_import <-    sqlFetch(IR.sql, "dbo.VW_Temperature") 
  
  
  odbcClose(IR.sql)
  
  
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations" ))
  
  # Set factors to characters
  Results_import %>% map_if(is.factor, as.character) %>% as_tibble -> Results_import
  
  Results_import <- odeqIRtools::data_aggregation(Results_import)
  
  
  # Data censoring --------------------------------------------------------------------------------------------------
  
  print("Data censor process")
  # Get all the standards to be used when dealing with the censored data
  
  
  Results_censored <- censor_data(Results_import, crit = Temp_Criteria)
  
  
  
  return(Results_censored)
  
}