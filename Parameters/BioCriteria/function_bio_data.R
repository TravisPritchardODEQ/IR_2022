<- function(database) {
  
  
  require(tidyverse)
  require(RODBC)
  require(odeqIRtools)
  
  print("Fetch continuous bio taxa data from IR database")
  #connect to IR database view as a general user
  # import bacteria data
  database = "IR_Dev" # use for testing 
  IR.sql <-   odbcConnect(database)
  
  
  Results_import <-    sqlFetch(IR.sql, "VW_BioCriteria") 
 
  odbcClose(IR.sql)
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations" ))
  
}

# There is no need for data review or censored data ?? 
  