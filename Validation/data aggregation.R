# Data aggregation ------------------------------------------------------------------------------------------------

options(scipen = 999999)

# Connect to the IR database
IR.sql <-   DBI::dbConnect(odbc::odbc(), "IR_Dev")

# Function to import custom input raw script
# This script queries input raw, but includes all the paramter view conditions. 
# This allows us to check duplciates only on data used in the parameter assessments

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


IR_res_db <- DBI::dbGetQuery(IR.sql, glue_sql(IR_Res_qry, .con = IR.sql))


# Read in identified exclusions -----------------------------------------------------------------------------------



IR_res <- IR_res_db 



# Different org ---------------------------------------------------------------------------------------------------


diff_orgs <-  IR_res %>%
  filter(AU_ID != '99') %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(MLocID,
           Char_Name,
           SampleStartDate,
           Statistical_Base,
           wqstd_code,
           Sample_Fraction,
           Char_Speciation,
           Time_Basis)%>%
  mutate(num_orgs = n_distinct(OrganizationID),
         num_in_group = n(),
         num_distinct_results = n_distinct(IRResultNWQSunit),
         num_resUID = n_distinct(Result_UID),
         num_depths = n_distinct(Result_Depth),
         num_act_depth = n_distinct(act_depth_height)) %>%
  mutate(group_num =paste0("diff-org-", cur_group_id())) %>%
  filter(num_orgs > 1) %>%
  arrange(group_num)


#write.xlsx(diff_orgs, file = "Validation/Dupdata/different_orgs.xlsx")

# Different time ------------------------------------------------------------------------------------------------


# These get median
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
  mutate(group_num =paste0("diff-time-", cur_group_id())) %>%
  filter(num_times > 1 ) %>%
  ungroup() %>%
  arrange(MLocID, SampleStartDate, Char_Name)

#write.xlsx(diff_time, file = "Validation/Dupdata/diff_time.xlsx")


# Others ----------------------------------------------------------------------------------------------------------
#This MUST be run after data duplication is finished

other_agg <- IR_res_db %>%
  filter(AU_ID != '99') %>%
  filter(!Result_UID %in% aggregate_data$Result_UID) %>%
  group_by(MLocID,
           Char_Name,
           SampleStartDate,
           Statistical_Base,
           wqstd_code,
           Sample_Fraction,
           Char_Speciation) %>%
  mutate(group_num =paste0("other_agg-", cur_group_id())) %>% 
  mutate(num = n()) %>%
  filter(num > 1) %>%
  arrange(group_num) 




# create aggregation datafile ------------------------------------------------------------------------------------------------

agg_diff_orgs <- diff_orgs %>%
  ungroup() %>%
  select(Result_UID, Char_Name, wqstd_code, group_num,  IRResultNWQSunit, IRWQSUnitName)

agg_diff_time <- diff_time %>%
  ungroup() %>%
  select(Result_UID, Char_Name,wqstd_code, group_num,  IRResultNWQSunit, IRWQSUnitName)

agg_other_agg <- other_agg %>%
  ungroup() %>%
  select(Result_UID, Char_Name,wqstd_code, group_num,  IRResultNWQSunit, IRWQSUnitName)

aggregate_data <- agg_diff_orgs %>%
  bind_rows(agg_diff_time) %>%
  bind_rows(agg_other_agg) %>%
  ungroup() %>%
  group_by(group_num) %>%
  mutate(H_concentration = ifelse(Char_Name == 'pH', 10^(-IRResultNWQSunit), NA )) %>% 
  mutate(mean_result = ifelse(Char_Name == 'pH', round(-log10(median(H_concentration)), 2), median(IRResultNWQSunit))) %>%
  mutate(unit = IRWQSUnitName) %>%
  select(Result_UID, group_num, Char_Name,wqstd_code, mean_result, unit)




# Test dataset ----------------------------------------------------------------------------------------------------

dup_test <- IR_res_db %>%
  filter(AU_ID != '99') %>%
  filter(!Result_UID %in% aggregate_data$Result_UID) %>%
  group_by(MLocID,
           Char_Name,
           SampleStartDate,
           Statistical_Base,
           wqstd_code,
           Sample_Fraction,
           Char_Speciation) %>%
  mutate(group_num =cur_group_id()) %>%
  mutate(num = n()) %>%
  filter(num > 1) %>%
  arrange(group_num)

write.xlsx(dup_test, file = "aggregate_data_error_table.xlsx")

