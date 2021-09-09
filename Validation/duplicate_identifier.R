#Work in progress for duplicate checker and data aggregation




library(DBI)
library(odbc)
library(glue)
library(tidyverse)
library(openxlsx)





# Connect to the IR database
IR.sql <-   DBI::dbConnect(odbc::odbc(), "IR_Dev")

# Pull selected data from InputRaw.
IR_Res_qry <-
  "Select  
OrganizationID, 
[MLocID], Activity_Type, [SampleStartDate],[SampleStartTime],[Char_Name], Sample_Fraction, Char_Speciation,
Pollu_ID, [Statistical_Base],  Result_UID, [IRResultNWQSunit], AU_ID, act_depth_height, Result_Depth, Analytical_method,
Result_Unit, QualifierAbbr, wqstd_code
 ,[MDLType]
 ,[MDLValue]
 ,[MDLUnit]
 ,[MRLType]
 ,[MRLUnit]
 ,[MRLValue]
 ,[URLType]
 ,[URLValue]
 ,[URLUnit]
  FROM [IntegratedReport].[dbo].[InputRaw]" 

IR_res <- DBI::dbGetQuery(IR.sql, glue_sql(IR_Res_qry, .con = IR.sql))



# Straight Duplicates ---------------------------------------------------------------------------------------------

#These are the values that are suspected duplicates

straight_duplicates <- IR_res %>%
  filter(AU_ID != '99') %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(MLocID,
           Char_Name,
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Statistical_Base,
           IRResultNWQSunit,
           act_depth_height,
           Result_Depth,
           Analytical_method,
           Result_Unit,
           wqstd_code,
           Sample_Fraction,
           Char_Speciation) %>%
  mutate(num = n(),
            num_distinct_results = n_distinct(IRResultNWQSunit),
            num_resUID = n_distinct(Result_UID)) %>%
  mutate(group_num =cur_group_id()) %>%
  filter(num > 1) %>%
  ungroup() %>%
  arrange(MLocID, SampleStartDate, Char_Name)


#different result UIDs. These are likely duplicates in AWQMS. They can be removed through adding resUID to unused data  
dup_diff_resuid <- straight_duplicates %>%
  filter(num_resUID > 1 & num_distinct_results == 1) %>%
  arrange(group_num)

# write.xlsx(dup_diff_resuid, file = "Validation/Dupdata/same_date_time_method_result.xlsx")



# same day/time/method different result ----------------------------------------------------------------------------------

day_time_dups <- IR_res %>%
  filter(AU_ID != '99') %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(MLocID,
           Char_Name,
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Statistical_Base,
           act_depth_height,
           Result_Depth,
           Analytical_method,
           wqstd_code,
           Sample_Fraction,
           Char_Speciation) %>%
  mutate(num = n(),
         num_distinct_results = n_distinct(IRResultNWQSunit),
         num_resUID = n_distinct(Result_UID)) %>%
  mutate(group_num =cur_group_id()) %>%
  filter(num > 1,
         num_distinct_results > 1) %>%
  ungroup() %>%
  arrange(MLocID, SampleStartDate, Char_Name)


# write.xlsx(day_time_dups, file = "Validation/Dupdata/same_date_time_method_diff_result.xlsx")


# Different Methods -----------------------------------------------------------------------------------------------
#Keep one with lowest MRL? HIghest DQL?
#!!!!! Add speciation to group

diff_methods <-  IR_res %>%
  filter(AU_ID != '99') %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(MLocID,
           Char_Name,
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Statistical_Base,
           act_depth_height,
           Result_Depth,
           wqstd_code,
           Sample_Fraction,
           Char_Speciation) %>%
  mutate(num_in_group = n(),
         num_distinct_results = n_distinct(IRResultNWQSunit),
         num_resUID = n_distinct(Result_UID),
         num_analytical_methods = n_distinct(Analytical_method)) %>%
  mutate(group_num =cur_group_id()) %>%
  filter(num_analytical_methods > 1) %>%
  ungroup() %>%
  arrange(MLocID, SampleStartDate, Char_Name)

# write.xlsx(diff_methods, file = "Validation/Dupdata/diff_method.xlsx")

# Different Depths ------------------------------------------------------------------------------------------------

#Keep most shallow?

diff_depths <-  IR_res %>%
  filter(AU_ID != '99') %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(MLocID,
           Char_Name,
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Statistical_Base,
           wqstd_code,
           Sample_Fraction,
           Analytical_method,
           Char_Speciation)%>%
  mutate(num_in_group = n(),
         num_distinct_results = n_distinct(IRResultNWQSunit),
         num_resUID = n_distinct(Result_UID),
         num_depths = n_distinct(Result_Depth),
         num_act_depth = n_distinct(act_depth_height)) %>%
  mutate(group_num =cur_group_id()) %>%
  filter(num_depths > 1 | num_act_depth > 1) %>%
  ungroup() %>%
  arrange(MLocID, SampleStartDate, Char_Name)


# write.xlsx(diff_methods, file = "Validation/Dupdata/diff_depths.xlsx")



# Different time ------------------------------------------------------------------------------------------------



diff_time <-  IR_res %>%
  filter(AU_ID != '99') %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(MLocID,
           Char_Name,
           Activity_Type, 
           SampleStartDate,
           Statistical_Base,
           act_depth_height,
           Result_Depth,
           wqstd_code,
           Sample_Fraction,
           Analytical_method,
           Char_Speciation)%>%
  mutate(num_in_group = n(),
         num_distinct_results = n_distinct(IRResultNWQSunit),
         num_resUID = n_distinct(Result_UID),
         num_times = n_distinct(SampleStartTime)) %>%
  mutate(group_num =cur_group_id()) %>%
  filter(num_times > 1 ) %>%
  ungroup() %>%
  arrange(MLocID, SampleStartDate, Char_Name)

# write.xlsx(diff_methods, file = "Validation/Dupdata/diff_time.xlsx")


# cont pH dups ----------------------------------------------------------------------------------------------------



# Pull selected data from InputRaw.
IR_cont_Res_qry <-
  "SELECT [OrganizationID]
      ,[org_name]
      ,[MLocID]
      ,AU_ID
      ,[Equipment_ID]
      ,[Media]
      ,[Char_Name]
      ,[Depth]
      ,[Depth_Unit]
      ,[Result_Date]
      ,[Result_Time]
      ,[Result_Numeric]
      ,[Result_Unit]
      ,[Result_Status]
      ,[DQL]
      ,[Comments]
  FROM [IntegratedReport].[dbo].[ResultsRawCont_pH]" 

IR_cont_res <- DBI::dbGetQuery(IR.sql, glue_sql(IR_cont_Res_qry, .con = IR.sql))


cont_dupes <- IR_cont_res %>%
  filter(AU_ID != '99') %>%
  mutate(Depth = ifelse(Depth == 'NA', NA, Depth )) %>%
  group_by(MLocID, Char_Name,Result_Date,  Result_Time) %>%
  mutate(num_in_group = n(),
         num_distinct_results = n_distinct(Result_Numeric),
         num_depths = n_distinct(Depth)) %>%
  mutate(group_num =cur_group_id()) %>%
  filter(num_in_group > 1)
  



