library(tidyverse)
library(DBI)
library(openxlsx)

database = 'IR_Dev'

IR.sql <- DBI::dbConnect(odbc::odbc(),
                         database)

new_data_query <-  glue::glue_sql("SELECT DISTINCT [AU_ID],
                                    Pollu_ID,
                                    wqstd_code,
                                    Char_Name
                                      FROM [IntegratedReport].[dbo].[InputRaw]
                                        Where Result_UID not in (select Result_UID from InputRaw_2022_old)", 
                                .con = IR.sql)




new_data <- DBI::dbGetQuery(IR.sql, new_data_query)

new_data_2 <- new_data %>%
  mutate(assessment = case_when(wqstd_code %in% c(15, 16) ~ "Toxics",
                                TRUE ~ Char_Name),
         number = 1)

new_data_wide <- new_data_2 %>%
  select(AU_ID, Pollu_ID, assessment, number) %>%
  distinct() %>%
  select(-Pollu_ID) %>%
  pivot_wider(names_from = assessment, values_from = number, values_fn = sum )

write.xlsx(new_data_wide, "Validation/unassessed_data_summary.xlsx")
