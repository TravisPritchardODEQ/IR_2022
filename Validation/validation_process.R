library(tidyverse)


validation_excludes <- function(file, writexl = FALSE, xl_dir, writedb = TRUE, database = 'IR_Dev'){
  

# testing ---------------------------------------------------------------------------------------------------------

# file =   "//deqHQ1/WQASSESSMENT/2022IRFiles/Data issues/data_validation_manual_review.xlsx"
# writexl = FALSE
# xl_dir =  "//deqHQ1/WQASSESSMENT/2022IRFiles/Data issues/data validation"
# writedb = TRUE
# database = 'IR_Dev'

# import and format results ---------------------------------------------------------------------------------------

manual_validation_xlsx <- openxlsx::read.xlsx(file)


validation_unused_data <- manual_validation_xlsx %>%
  filter(manual_validation_result == 'exclude') %>%
  select(Result_UID, Char_Name, exclusion.rationale) %>%
  rename(Data_Review_Comment = exclusion.rationale)


# write xlsx if applicable ----------------------------------------------------------------------------------------
if(writexl){
openxlsx::write.xlsx(validation_unused_data, file = paste0(xl_dir, "/validation_excluded_data.xlsx"))
}


# write database --------------------------------------------------------------------------------------------------


if(writedb){
  
  IR.sql <-   DBI::dbConnect(odbc::odbc(), dsn = database)
  
  DBI::dbAppendTable(IR.sql, 'Unused_Results', validation_unused_data, row.names = NULL)
  
}


}


validation_excludes(file =  "//deqHQ1/WQASSESSMENT/2022IRFiles/Data issues/data_validation_manual_review.xlsx",
                    writexl = TRUE,
                    xl_dir =  "//deqHQ1/WQASSESSMENT/2022IRFiles/Data issues/data validation",
                    writedb = TRUE,
                    database = 'IR_Dev')
