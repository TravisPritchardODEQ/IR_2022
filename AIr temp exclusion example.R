

# Connect to the IR database
IR.sql <-   DBI::dbConnect(odbc::odbc(), "IR_Dev")

IR_Res_qry <- "SELECT TOP (1000) *
     
  FROM [IntegratedReport].[dbo].[VW_Temperature]
  "

IR_res <- DBI::dbGetQuery(IR.sql, glue::glue_sql(IR_Res_qry, .con = IR.sql))

library(odeqIRtools)

test <- air_temp_exclusion(IR_res, date_col =  'SampleStartDate') 
