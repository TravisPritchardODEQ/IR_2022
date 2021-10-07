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
  filter(!is.na(validation_result))

print("Write excel doc at Validation/data_validation_manual_review.csv", )

write.csv(manual_check, "Validation/data_validation_manual_review.csv",
          row.names = FALSE)

}
