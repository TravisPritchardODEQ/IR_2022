
#Joins dataframe with pollutant and assessment type

join_pollu_assess <- function(df, database = "IR_Dev"){

#testing
# database <- "IR_Dev"
# df <- bacteria_fresh_WS_station_GNIS_rollup


con <- DBI::dbConnect(odbc::odbc(), database)


pollu_query <- glue::glue_sql("SELECT [Pollu_ID]
      ,[Pollutant_DEQ WQS] AS Pollutant
  FROM [IntegratedReport].[dbo].[LU_Pollutant]", .con = con)

#Query to get ancillary data
pollutant <- DBI::dbGetQuery(con, pollu_query) %>%
  mutate(Pollu_ID = as.character(Pollu_ID))

assessment_query <- glue::glue_sql("SELECT [wqstd_code]
      ,[wqstd] as Assessment
  FROM [IntegratedReport].[dbo].[LU_Wqstd_Code]", .con = con)


assessment <- DBI::dbGetQuery(con, assessment_query) %>%
  mutate(wqstd_code = as.character(wqstd_code))


pollutant_assessment0 <- df %>%
  left_join(pollutant, by = "Pollu_ID") %>%
  left_join(assessment, Joining, by = "wqstd_code")

num_col <-  ncol(pollutant_assessment0)
num_col_2 <- num_col-2

pollutant_assessment <- pollutant_assessment0[,c(1, 2, 
                                                 num_col-1,
                                                 num_col, 
                                                 3:num_col_2)]

return(pollutant_assessment)

}