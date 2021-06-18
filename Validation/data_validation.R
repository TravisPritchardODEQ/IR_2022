#' This script performs the data validation component of the IR process. Data in InputRaw is checked against the 1st and 
#' 99th percentiles of all data in AWQMS. Data outside of this range is exported in data_validation_manual_review.csv.
#' This table should be reviewed and any rejected data should be loaded in the UnusedData table in the IR database.
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


Data_validation_values <- read.csv("Validation/Data_validation_values.csv") %>%
  mutate(Char_Speciation = ifelse(Char_Speciation == 'NULL', NA_character_, Char_Speciation ),
         Sample_Fraction = ifelse(Sample_Fraction == 'NULL', NA_character_, Sample_Fraction ),
         IRWQSUnitName  = ifelse(Char_Name == 'pH', NA_character_, IRWQSUnitName ),
         Statistical_Base = ifelse(Statistical_Base == 'NULL', NA_character_, Statistical_Base),
         p01 = as.numeric(p01),
         p99 = as.numeric(p99)
         )
  

    
 Data_validation_values[Data_validation_values == "NULL"] <- NA_character_

#connect to IR database view as a general user

IR.sql <-   DBI::dbConnect(odbc::odbc(), dsn = "IR 2018")


InputRaw  <-   DBI::dbReadTable(IR.sql, "InputRaw")
#InputRaw <- DBI::dbFetch(InputRaw)

DBI::dbDisconnect(IR.sql)


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

write.csv(manual_check, "Validation/data_validation_manual_review.csv",
          row.names = FALSE)


test <- validation %>%
  filter(!is.na(validation_result))