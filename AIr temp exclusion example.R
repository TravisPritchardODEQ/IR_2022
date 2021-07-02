

# Connect to the IR database
IR.sql <-   DBI::dbConnect(odbc::odbc(), "IR 2018")

IR_Res_qry <- "SELECT TOP (1000) [OrganizationID]
      ,[MLocID]
      ,[StationDes]
      ,[MonLocType]
      ,[Lat_DD]
      ,[Long_DD]
      ,[SampleStartDate]
      ,[SampleStartTime]
      ,[SampleStartTZ]
      ,[chr_uid]
      ,[Char_Name]
      ,[Result_UID]
      ,[Result_status]
      ,[Result_Type]
      ,[Result]
      ,[Result_Numeric]
      ,[Result_Operator]
      ,[Result_Unit]
      ,[Unit_UID]
      ,[IRResultNWQSunit]
      ,[IRPollUnit_ID]
      ,[IRWQSUnitName]
      ,[ResultCondName]
      ,[Time_Basis]
      ,[Statistical_Base]
      ,[Statistic_N_Value]
     
  FROM [IntegratedReport].[dbo].[ResultsRawWater2018]
  Where Char_Name LIKE '%Temp%' AND Statistical_Base = '7DADM' AND SampleStartDate > '2011-01-01' "

IR_res <- DBI::dbGetQuery(IR.sql, glue::glue_sql(IR_Res_qry, .con = IR.sql))



test <- air_station_lookup(IR_res) %>%
  left_join(select(OR_air_temp, Air_temp_station, Date, Air_Temp_daily_max, 
                   air_temp_exclusion_value, above_exclusion_1d, above_exclusion_7d), 
            by = c('Air_Station' = 'Air_temp_station','SampleStartDate' = 'Date' ) )
