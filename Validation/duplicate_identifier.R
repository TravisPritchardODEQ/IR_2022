#Work in progress for duplicate checker and data aggregation




library(DBI)
library(odbc)
library(glue)
library(tidyverse)
library(openxlsx)





# Connect to the IR database
IR.sql <-   DBI::dbConnect(odbc::odbc(), "IR 2018")

# Pull selected data from InputRaw.
IR_Res_qry <-
  "Select  
OrganizationID, 
[MLocID], Activity_Type, [SampleStartDate],[SampleStartTime],[Char_Name],
Pollu_ID, [Statistical_Base],  Result_UID, [IRResultNWQSunit], AU_ID, act_depth_height, Result_Depth, Analytical_method,
Result_Unit, QualifierAbbr
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
  group_by(OrganizationID, 
           MLocID,
           Char_Name,
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Statistical_Base,
           IRResultNWQSunit,
           act_depth_height,
           Result_Depth,
           Analytical_method,
           Result_Unit) %>%
  mutate(num = n(),
            num_distinct_results = n_distinct(IRResultNWQSunit),
            num_resUID = n_distinct(Result_UID)) %>%
  filter(num > 1) %>%
  ungroup() %>%
  arrange(MLocID, SampleStartDate, Char_Name)

#Duplicate result UIDs. These are duplicate values in every way. Some method of removing duplicates must be found. 
dup_same_resuid <- straight_duplicates %>%
  filter(num_resUID == 1 & num_distinct_results == 1)

#different result UIDs. These are likely duplcaites in AWQMS. THey can be removed through adding resUID to unsed data  
dup_diff_resuid <- straight_duplicates %>%
  filter(num_resUID > 1 & num_distinct_results == 1)



# Different Methods -----------------------------------------------------------------------------------------------
#Keep one with lowest MRL? HIghest DQL?

diff_methods <-  IR_res %>%
  filter(AU_ID != '99') %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(OrganizationID, 
           MLocID,
           Char_Name,
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Statistical_Base,
           IRResultNWQSunit,
           act_depth_height,
           Result_Depth,
           Result_Unit) %>%
  mutate(num_in_group = n(),
         num_distinct_results = n_distinct(IRResultNWQSunit),
         num_resUID = n_distinct(Result_UID),
         num_analytical_methods = n_distinct(Analytical_method)) %>%
  filter(num_analytical_methods > 1) %>%
  ungroup() %>%
  arrange(MLocID, SampleStartDate, Char_Name)



# Different Depths ------------------------------------------------------------------------------------------------

#Keep most shallow?

diff_depths <-  IR_res %>%
  filter(AU_ID != '99') %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(OrganizationID, 
           MLocID,
           Char_Name,
           Activity_Type, 
           SampleStartDate,
           Statistical_Base,
           IRResultNWQSunit,
           Analytical_method) %>%
  mutate(num_in_group = n(),
         num_distinct_results = n_distinct(IRResultNWQSunit),
         num_resUID = n_distinct(Result_UID),
         num_depths = n_distinct(Result_Depth),
         num_act_depth = n_distinct(act_depth_height)) %>%
  filter(num_depths > 1 | num_act_depth > 1) %>%
  ungroup() %>%
  arrange(MLocID, SampleStartDate, Char_Name)



