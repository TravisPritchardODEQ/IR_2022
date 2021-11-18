library(lubridate)
library(tidyverse)

# DO delist check

AU_ID <- c('OR_SR_1709000506_02_103930')



DO_data_qry <- "SELECT [OrganizationID]
      ,[MLocID]
      ,[StationDes]
      ,[GNIS_Name]
      ,[AU_GNIS_Name]
      ,[MonLocType]
      ,[HUC12_Name]
      ,[ELEV_Ft]
      ,[AU_ID]
      ,[WaterTypeCode]
      ,[WaterBodyCode]
      ,[ben_use_code]
      ,[OWRD_Basin]
      ,[wqstd_code]
      ,[Pollu_ID]
      ,[Char_Name]
      ,[SampleMedia]
      ,[SampleSubmedia]
      ,[Sample_Fraction]
      ,[Result_status]
      ,[Statistical_Base]
      ,[Time_Basis]
      ,[SampleStartDate]
      ,[SampleStartTime]
      ,[SampleStartTZ]
      ,[Result_Depth]
      ,[Act_Depth_Top]
      ,[act_depth_height]
      ,[ActDepthUnit]
      ,[Activity_Type]
      ,[Result_Type]
      ,[Result_UID]
      ,[Result_Numeric]
      ,[Result_Unit]
      ,[IRResultNWQSunit]
      ,[Result_Operator]
      ,[IRWQSUnitName]
      ,[lab_Comments]
      ,[General_Comments]
      ,[QualifierAbbr]
      ,[QualifierTxt]
      ,[DO_code]
      ,[DO_Class]
      ,[DO_SpawnCode]
      ,[SpawnStart]
      ,[SpawnEnd]
      ,[crit_30D]
      ,[crit_7Mi]
      ,[crit_Min]
      ,[crit_Instant]
  FROM [IntegratedReport].[dbo].[VW_DO]
Where AU_ID in  ({AU_ID*})"
# Connect to database
con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")

# Create query language
qry <- glue::glue_sql(DO_data_qry, .con = con)

#
# Query the database
data_fetch <- DBI::dbGetQuery(con, qry)



# Continuous ------------------------------------------------------------------------------------------------------


# Other -----------------------------------------------------------------------------------------------------------


min_data_assess <- data_fetch %>%
  filter(Statistical_Base == "Minimum") %>%
  mutate(SampleStartDate = ymd(SampleStartDate),
         SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",year(SampleStartDate) ), SpawnStart ),
         SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", year(SampleStartDate)), SpawnEnd ),
         SpawnStart = mdy(SpawnStart),
         SpawnEnd=mdy(SpawnEnd),
         # If Spawn dates span a calendar year, account for year change in spawn end date
         SpawnEnd = if_else(SpawnEnd < SpawnStart & SampleStartDate >= SpawnEnd, SpawnEnd + lubridate::years(1), # add a year if in spawn period carrying to next year
                            SpawnEnd),
         SpawnStart = if_else(SpawnEnd < SpawnStart & SampleStartDate <= SpawnEnd, SpawnStart - lubridate::years(1), # subtract a year if in spawn period carrying from previous year
                              SpawnStart),
         in_spawn = ifelse(SampleStartDate >= SpawnStart & SampleStartDate <= SpawnEnd & !is.na(SpawnStart), 1, 0 ),
         spawn_length = SpawnEnd - SpawnStart + 1,
         critstart = mdy(paste0("6/1/",year(SampleStartDate) )),
         critend = mdy(paste0("9/30/",year(SampleStartDate) )),
         is.crit = ifelse(SampleStartDate >= critstart & SampleStartDate <= critend, 1, 0 ),
         critical_length = critend - critstart + 1,
         year = year(SampleStartDate)) %>%
  group_by(AU_ID, Statistical_Base, year, spawn_length, critical_length) %>%
  summarise(num_days = n_distinct(SampleStartDate),
            num_critical_days = n_distinct(SampleStartDate[is.crit == 1]),
            num_spawn_days = n_distinct(SampleStartDate[in_spawn == 1])) %>%
  mutate(meets_yer_round_delisting = case_when(num_critical_days >= critical_length*.8 ~ 1,
                                                  TRUE ~ 0),
         meets_spawn_delisting = case_when(num_spawn_days >= spawn_length*.8 ~ 1,
                                           TRUE ~ 0)) %>%
  group_by(AU_ID) %>%
  summarise(years_critrical = sum(meets_yer_round_delisting),
            years_spawning = sum(meets_spawn_delisting))






# Watershed Unit --------------------------------------------------------------------------------------------------


min_data_assess <- data_fetch %>%
  filter(Statistical_Base == "Minimum") %>%
  mutate(SampleStartDate = ymd(SampleStartDate),
         SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",year(SampleStartDate) ), SpawnStart ),
         SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", year(SampleStartDate)), SpawnEnd ),
         SpawnStart = mdy(SpawnStart),
         SpawnEnd=mdy(SpawnEnd),
         # If Spawn dates span a calendar year, account for year change in spawn end date
         SpawnEnd = if_else(SpawnEnd < SpawnStart & SampleStartDate >= SpawnEnd, SpawnEnd + lubridate::years(1), # add a year if in spawn period carrying to next year
                            SpawnEnd),
         SpawnStart = if_else(SpawnEnd < SpawnStart & SampleStartDate <= SpawnEnd, SpawnStart - lubridate::years(1), # subtract a year if in spawn period carrying from previous year
                              SpawnStart),
         in_spawn = ifelse(SampleStartDate >= SpawnStart & SampleStartDate <= SpawnEnd & !is.na(SpawnStart), 1, 0 ),
         spawn_length = SpawnEnd - SpawnStart + 1,
         critstart = mdy(paste0("6/1/",year(SampleStartDate) )),
         critend = mdy(paste0("9/30/",year(SampleStartDate) )),
         is.crit = ifelse(SampleStartDate >= critstart & SampleStartDate <= critend, 1, 0 ),
         critical_length = critend - critstart + 1,
         year = year(SampleStartDate)) %>%
  group_by(AU_ID, MLocID, Statistical_Base, year, spawn_length, critical_length) %>%
  summarise(num_days = n_distinct(SampleStartDate),
            num_critical_days = n_distinct(SampleStartDate[is.crit == 1]),
            num_spawn_days = n_distinct(SampleStartDate[in_spawn == 1])) %>%
  mutate(meets_yer_round_delisting = case_when(num_critical_days >= critical_length*.8 ~ 1,
                                               TRUE ~ 0),
         meets_spawn_delisting = case_when(num_spawn_days >= spawn_length*.8 ~ 1,
                                           TRUE ~ 0)) %>%
  group_by(AU_ID, MLocID) %>%
  summarise(years_critrical = sum(meets_yer_round_delisting),
            years_spawning = sum(meets_spawn_delisting))



# Instant Watershed -----------------------------------------------------------------------------------------------

# Year round ------------------------------------------------------------------------------------------------------


min_data_assess <- data_fetch %>%
  filter(is.na(Statistical_Base)) %>%
  mutate(SampleStartDate = ymd(SampleStartDate),
         SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",year(SampleStartDate) ), SpawnStart ),
         SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", year(SampleStartDate)), SpawnEnd ),
         SpawnStart = mdy(SpawnStart),
         SpawnEnd=mdy(SpawnEnd),
         # If Spawn dates span a calendar year, account for year change in spawn end date
         SpawnEnd = if_else(SpawnEnd < SpawnStart & SampleStartDate >= SpawnEnd, SpawnEnd + lubridate::years(1), # add a year if in spawn period carrying to next year
                            SpawnEnd),
         SpawnStart = if_else(SpawnEnd < SpawnStart & SampleStartDate <= SpawnEnd, SpawnStart - lubridate::years(1), # subtract a year if in spawn period carrying from previous year
                              SpawnStart),
         in_spawn = ifelse(SampleStartDate >= SpawnStart & SampleStartDate <= SpawnEnd & !is.na(SpawnStart), 1, 0 ),
         spawn_length = SpawnEnd - SpawnStart + 1,
         critstart = mdy(paste0("6/1/",year(SampleStartDate) )),
         critend = mdy(paste0("9/30/",year(SampleStartDate) )),
         is.crit = ifelse(SampleStartDate >= critstart & SampleStartDate <= critend, 1, 0 ),
         critical_length = critend - critstart + 1,
         year = year(SampleStartDate),
         month = month(SampleStartDate)) %>%
  group_by(AU_ID,  Statistical_Base, year, month, spawn_length, critical_length) %>%
  summarise(num_days = n_distinct(SampleStartDate),
            num_critical_days = n_distinct(SampleStartDate[is.crit == 1]),
            num_spawn_days = n_distinct(SampleStartDate[in_spawn == 1])) %>%
  filter(month %in% c(6,7,8,9)) %>%
  group_by(AU_ID,   year, spawn_length, critical_length) %>%
  summarise(meets_yer_round_delisting = case_when(n_distinct(month[num_days >= 2]) == 4 ~ 1,
                                               TRUE ~ 0)) %>%
  group_by(AU_ID) %>%
  summarise(years_critrical = sum(meets_yer_round_delisting))


# Spawn -----------------------------------------------------------------------------------------------------------


min_data_assess2 <- data_fetch %>%
  filter(is.na(Statistical_Base)) %>%
  mutate(SampleStartDate = ymd(SampleStartDate),
         SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",year(SampleStartDate) ), SpawnStart ),
         SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", year(SampleStartDate)), SpawnEnd ),
         SpawnStart = mdy(SpawnStart),
         SpawnEnd=mdy(SpawnEnd),
         # If Spawn dates span a calendar year, account for year change in spawn end date
         SpawnEnd = if_else(SpawnEnd < SpawnStart & SampleStartDate >= SpawnEnd, SpawnEnd + lubridate::years(1), # add a year if in spawn period carrying to next year
                            SpawnEnd),
         SpawnStart = if_else(SpawnEnd < SpawnStart & SampleStartDate <= SpawnEnd, SpawnStart - lubridate::years(1), # subtract a year if in spawn period carrying from previous year
                              SpawnStart),
         in_spawn = ifelse(SampleStartDate >= SpawnStart & SampleStartDate <= SpawnEnd & !is.na(SpawnStart), 1, 0 ),
         spawn_length = SpawnEnd - SpawnStart + 1,
         critstart = mdy(paste0("6/1/",year(SampleStartDate) )),
         critend = mdy(paste0("9/30/",year(SampleStartDate) )),
         is.crit = ifelse(SampleStartDate >= critstart & SampleStartDate <= critend, 1, 0 ),
         critical_length = critend - critstart + 1,
         year = year(SampleStartDate),
         month = month(SampleStartDate)) %>%
  group_by(AU_ID, MLocID, Statistical_Base, year, month, spawn_length, SpawnStart, SpawnEnd, critical_length) %>%
  summarise(num_days = n_distinct(SampleStartDate),
            num_critical_days = n_distinct(SampleStartDate[is.crit == 1]),
            num_spawn_days = n_distinct(SampleStartDate[in_spawn == 1])) %>%
  mutate(spawnstartmonth = month(SpawnStart),
         spawnendmonth = month (SpawnEnd)) %>%
  mutate(spawn_month = case_when(between(month,spawnstartmonth,spawnendmonth) ~ 1,
                                 TRUE ~ 0),
         spawn_months_length = spawnendmonth -spawnstartmonth + 1) %>%
  filter(spawn_month == 1 )%>%
  group_by(AU_ID, MLocID,  year) %>%
  summarise(meets_spawn_delisting = case_when(n_distinct(month[num_spawn_days >= 2]) == spawn_months_length ~ 1,
                                                  TRUE ~ 0)) %>%
  group_by(AU_ID, MLocID) %>%
  summarise(years_critrical = sum(meets_spawn_delisting))


