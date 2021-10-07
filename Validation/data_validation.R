#' This script performs the data validation component of the IR process. Data in InputRaw is checked against the 1st and 
#' 99th percentiles of all data in AWQMS. Data outside of this range is exported in data_validation_manual_review.csv.
#' This table should be reviewed and any rejected data should be identified in the Unused data table in the IR database.
#' 
#' Percentiles are calculated using the entire AWQMS dataset. SQL code for generating the validation values can be 
#' found at \\deqHQ1\WQASSESSMENT\2022IRFiles\Code\Validation.
#' 
#' Percentiles are generated using the following parameters as grouping variables:
#' * Pollu_ID
#' * chr_uid
#' * Char_Name
#' * Char_Speciation
#' * Sample_Fraction
#' * IRWQSUnitName
#' * Statistical_Base


validate_data <- function(database = "IR_Dev"){

library(tidyverse)


Data_validation_values <- read.csv("Validation/Data_validation_values.csv") %>%
  mutate(Char_Speciation = ifelse(Char_Speciation == 'NULL', NA_character_, Char_Speciation ),
         Sample_Fraction = ifelse(Sample_Fraction == 'NULL', NA_character_, Sample_Fraction ),
         IRWQSUnitName  = ifelse(Char_Name == 'pH', NA_character_, IRWQSUnitName ),
         Statistical_Base = ifelse(Statistical_Base == 'NULL', NA_character_, Statistical_Base),
         p01 = as.numeric(p01),
         p99 = as.numeric(p99)
         )
  

    
 Data_validation_values[Data_validation_values == "NULL"] <- NA_character_

 
 
 print("Query InputRaw")
#connect to IR database view as a general user

IR.sql <-   DBI::dbConnect(odbc::odbc(), dsn = database)


getSQL <- function(filepath){
  con = file(filepath, "r")
  sql.string <- ""
  
  while (TRUE){
    line <- readLines(con, n = 1)
    
    if ( length(line) == 0 ){
      break
    }
    
    line <- gsub("\\t", " ", line)
    
    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    
    sql.string <- paste(sql.string, line)
  }
  
  close(con)
  return(sql.string)
}


column_order <- c("OrganizationID",  "Lat_DD", "Long_DD", "MonLocType", "HUC12_Name", "ELEV_Ft", 
                  "AU_ID", "AU_GNIS_Name", "GNIS_Name", "FishCode", "SpawnCode", "WaterTypeCode", "WaterBodyCode", 
                  "BacteriaCode", "DO_code", "ben_use_code", "pH_code", "DO_SpawnCode", "OWRD_Basin", "EcoRegion2", 
                  "EcoRegion3",  "Pollu_ID", "Calc_Crit", "Combine_Result_Cmnts", "Combine_Result", 
                  "act_id", "Activity_Type", "SampleMedia", "SampleSubmedia", "SampleStartDate", "SampleStartTime", 
                  "SampleStartTZ", "chr_uid",  "Char_Speciation", "Sample_Fraction", "CASNumber", 
                  "Result_UID", "Result_status", "Result_Numeric",  "ActDepthUnit", "Result_Depth", 
                  "act_depth_height", "Result_Depth_Unit", "Act_Depth_Top", "Act_Depth_Top_Unit", "Act_Depth_Bottom", 
                  "Act_Depth_Bottom_Unit", "Time_Basis", "Statistical_Base", "Statistic_N_Value", "act_sam_compnt_name",
                  "lab_Comments", "General_Comments", "Analytical_method", "QualifierAbbr", "QualifierTxt", "IDLType",
                  "IDLValue", "IDLUnit", "MDLType", "MDLValue", "MDLUnit", "MRLType", "MRLUnit", "MRLValue", "URLType",
                  "URLValue", "URLUnit", "TribalLand",  "Result_Unit",  "Result_Type",
                  "Result", "Reachcode", "Measure",
                  "MLocID", "StationDes", 
                  "wqstd_code",
                  "Char_Name",
                  "Result_Numeric",
                  "Result_Operator",
                  "IRResultNWQSunit",
                  "IRWQSUnitName",
                  "p01", "p99", "validation_result", "manual_validation_result")


IR_Res_qry <- getSQL("Validation/Dupdata/InputRaw limited to data views.sql")


InputRaw <- DBI::dbGetQuery(IR.sql, glue_sql(IR_Res_qry, .con = IR.sql))

DBI::dbDisconnect(IR.sql)

print("Join data to validation valies")

joined <- InputRaw %>%
  mutate(IRWQSUnitName = ifelse(IRWQSUnitName == "mg/L", "mg/l", IRWQSUnitName )) %>%
  left_join(Data_validation_values, by = c("Pollu_ID", "chr_uid", "Char_Name", "Char_Speciation", 
                                           "Sample_Fraction", "IRWQSUnitName", 'Statistical_Base'))



validation <- joined %>%
  mutate(validation_result = case_when(IRResultNWQSunit < p01 ~ "Below 1st percentile",
                                       IRResultNWQSunit > p99 ~ "Above 99th percentile",
                                       is.na(p01) | is.na(p99) ~ "No percentile data found"),
         manual_validation_result = "")

manual_check <- validation %>%
  filter(!is.na(validation_result)) %>%
  select(all_of(column_order))

print("Validation/data_validation_manual_review.csv" )

write.csv(manual_check, file = "Validation/data_validation_manual_review.csv",
          row.names = FALSE)

}
