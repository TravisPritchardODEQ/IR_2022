library(tidyverse)
library(odeqIRtools) 
library(odbc)
library(DBI)
library(glue)
library(deqalcrit)


aluminum_assessment <- function(database){
# Testing ---------------------------------------------------------------------------------------------------------

#database <- "IR_Dev"  

# Database fetch --------------------------------------------------------------------------------------------------

# Connect to the IR database
IR.sql <-   DBI::dbConnect(odbc::odbc(), database)

# Pull selected data from InputRaw.
IR_Res_qry <-
  "Select  
*
  FROM [IntegratedReport].[dbo].[VW_Aluminum]" 

Results_import <- DBI::dbGetQuery(IR.sql, glue::glue_sql(IR_Res_qry, .con = IR.sql))

Results_import_no_NAs <- Results_import %>%
  filter(!is.na(MLocID))

print(paste("Returned", nrow(Results_import), 
            "results from", length(unique(Results_import$MLocID)), "monitoring locations"))

Results_import_no_NAs <- odeqIRtools::data_aggregation(Results_import_no_NAs)

#Create a vector of monitoring locations with metals data. This list is used as a filter for the hardness query
mlocs <- unique(Results_import_no_NAs$MLocID)




# Get ancillary data ----------------------------------------------------------------------------------------------

print("Fetch ancillary data from IR database")

ancillary_params <- c("Organic carbon",
                      "pH",
                      'Hardness, Ca, Mg',
                      'Hardness, non-carbonate',
                      'Calcium',
                      "Magnesium",
                      "Conductivity",
                      "Specific conductance")

ancillary_qry <- glue::glue_sql("SELECT [MLocID]
                                ,[SampleStartDate]
                                ,chr_uid
                                ,[Char_Name]
                                ,[Char_Speciation]
                                ,SampleMedia
                                ,SampleSubmedia
                                ,[Sample_Fraction]
                                ,[IRResultNWQSunit]
                                ,[Result_Depth]
                                ,[IRWQSUnitName]
                                FROM [IntegratedReport].[dbo].[ResultsRawWater]
                                WHERE Char_Name in ({ancillary_params*}) 
                                AND (Statistical_Base IS NULL)
                                AND MLocID in ({mlocs*})
                                AND IRResultNWQSunit IS NOT NULL", .con = IR.sql)

#Query to get ancillary data
Results_ancillary <- DBI::dbGetQuery(IR.sql, ancillary_qry)

unique(Results_ancillary$Char_Name)

ancillary_data <- Results_ancillary %>%
  filter(!(Char_Name == 'Organic carbon' & Sample_Fraction == 'Bed Sediment')) %>%
  mutate(Char_Name = ifelse(chr_uid %in% c(544, 100331), 'Alkalinity',
                            ifelse(chr_uid %in% c(1097, 1099), 'Hardness',
                                   ifelse(chr_uid == 2174 & Sample_Fraction %in% c("Total", 'Total Recoverable','Suspended')  , 'TOC',
                                          ifelse(chr_uid == 2174 & Sample_Fraction == "Dissolved", 'DOC', Char_Name ))))) %>%
  mutate(IRResultNWQSunit = ifelse(IRResultNWQSunit == 'ug/l', IRResultNWQSunit / 1000, IRResultNWQSunit),
         Result_Unit = ifelse(IRWQSUnitName == 'ug/l', "mg/L", IRWQSUnitName)) %>%
  mutate(Simplified_Sample_fraction = ifelse(Sample_Fraction %in% c("Total", "Extractable",
                                                                    "Total Recoverable","Total Residual",
                                                                    "None", "volatile", "Semivolatile",
                                                                    "Acid Soluble", "Suspended")  |
                                               is.na(Sample_Fraction), 'Total',
                                             ifelse(Sample_Fraction == "Dissolved"  |
                                                      Sample_Fraction == "Filtered, field"  |
                                                      Sample_Fraction == "Filtered, lab"  , "Dissolved", "Error"))) %>%
  group_by(MLocID,  SampleStartDate,Char_Name,SampleMedia, SampleSubmedia ) %>%
  mutate(Has_dissolved = ifelse(min(Simplified_Sample_fraction) == "Dissolved", 1, 0 )) %>%
  ungroup() %>%
  filter((Has_dissolved == 1 & Simplified_Sample_fraction == "Dissolved") | Has_dissolved == 0) %>%
  select(-Has_dissolved) %>%
  #mutate(Char_Name = paste0(Char_Name, "-", Simplified_Sample_fraction)) %>%
  group_by(MLocID,  SampleStartDate,Char_Name,SampleMedia, SampleSubmedia ) %>%
  summarise(result = max(IRResultNWQSunit)) %>%
  arrange(MLocID, SampleStartDate) %>%
  pivot_wider(names_from = Char_Name, values_from = result) 


 colnames(ancillary_data) <- make.names(names(ancillary_data), unique = TRUE, allow_ = TRUE)
 



 ancillary_data_calculated <- ancillary_data %>%
   rename(Specific_conductance = Specific.conductance) %>%
   mutate(Specific_conductance = case_when(!is.na(Specific_conductance) ~ Specific_conductance,
                                       is.na(Specific_conductance) & !is.na(Conductivity) ~ Conductivity,
                                       TRUE ~ NA_real_),
          DOC = case_when(!is.na(DOC) ~ DOC,
                           is.na(DOC) & !is.na(TOC) ~ TOC * 0.83,
                           TRUE ~ NA_real_),
          DOC_cmt = case_when(!is.na(DOC) ~ NA_character_,
                              is.na(DOC) & !is.na(TOC) ~ "Calculated DOC from TOC",
                              TRUE ~ "No organic carbon data- Used default DOC"),
          Hardness =  case_when(!is.na(Hardness) ~ Hardness,
                                 !is.na(Calcium) & !is.na(Magnesium) ~  2.497*Calcium + 4.1189*Magnesium,
                                 !is.na(Specific_conductance) ~ exp(1.06*log(Specific_conductance) - 1.26),
                                 TRUE ~ NA_real_),
          Hardness_cmt =  case_when(!is.na(Hardness) ~ NA_character_,
                                    !is.na(Calcium) & !is.na(Magnesium) ~ "Hardness based on Ca and Mg",
                                    !is.na(Specific_conductance) ~ "Hardness based on sp conductivity",
                                    TRUE ~ "No hardness value")
   )


Al_fractions <- Results_import_no_NAs %>%
  mutate(Simplified_Sample_fraction = case_when(Sample_Fraction %in% c("Total", "Extractable",
                                                                    "Total Recoverable","Total Residual",
                                                                    "None", "volatile", "Semivolatile",
                                                                    "Acid Soluble", "Suspended")  ~ 'Total',
                                                Sample_Fraction == "Dissolved"  |
                                                      Sample_Fraction == "Filtered, field"  |
                                                      Sample_Fraction == "Filtered, lab"  ~ "Dissolved", 
                                                Sample_Fraction == "Bioavailable" ~ "Bioavailable",
                                                TRUE ~ "ERROR")) %>%
  group_by(MLocID, SampleStartDate) %>%
  mutate(has_bioavailable = case_when(any(Simplified_Sample_fraction == "Bioavailable", na.rm = TRUE) ~ 1,
                                      TRUE ~ 0),
         has_total = case_when(any(Simplified_Sample_fraction == "Total", na.rm = TRUE) ~ 1,
                                       TRUE ~ 0)) %>%
  ungroup() %>%
  mutate(keep = case_when(has_bioavailable == 1 & Simplified_Sample_fraction == "Bioavailable" ~ 1,
                          has_bioavailable == 0 & has_total == 1 & Simplified_Sample_fraction == "Total" ~ 1,
                          has_bioavailable == 0 & has_total == 0 & Simplified_Sample_fraction == "Dissolved" ~ 1,
                          TRUE ~ 0)) %>%
  arrange(MLocID, SampleStartDate) %>%
  filter(keep == 1) %>%
  select(-keep, -has_bioavailable, -has_total)

al_ancillary_combined <- Al_fractions %>%
  left_join(ancillary_data_calculated, by = c("MLocID", "SampleMedia", "SampleSubmedia", "SampleStartDate"))

default_DOC <- al_ancillary_combined %>%
  ungroup() %>%
  filter(is.na(DOC)) %>%
  select(MLocID, Lat_DD, Long_DD) %>%
  distinct(MLocID, .keep_all = TRUE) %>%
  rowwise() %>%
  mutate(def_DOC = deqalcrit::al_default_DOC(Lat_DD, Long_DD)) %>%
  select(MLocID, def_DOC) %>%
  ungroup() 

al_ancillary_combined_2 <- al_ancillary_combined %>%
  ungroup() %>%
  left_join(default_DOC, by = "MLocID") %>%
  mutate(DOC = case_when(!is.na(DOC) ~ DOC,
                         TRUE ~ def_DOC),
         al_ancillary_cmt = case_when(!is.na(DOC_cmt) & !is.na(Hardness_cmt) ~ str_c(DOC_cmt, Hardness_cmt,  sep = "; " ),
                                      !is.na(DOC_cmt) & is.na(Hardness_cmt) ~ str_c(DOC_cmt, sep = "; " ),
                                      is.na(DOC_cmt) & !is.na(Hardness_cmt) ~ str_c(Hardness_cmt, sep = "; " ),
                                      TRUE ~ NA_character_))


  
print("Beginning AL criteria calculations")
Al_criteria <- deqalcrit::al_crit_calculator(al_ancillary_combined_2,
                                  ph_col = "pH", hardness_col = "Hardness", DOC_col = "DOC", verbose = FALSE)

al_criteria_excursions <- Al_criteria %>%
  mutate(default_crit = case_when(stringr::str_detect(Flag, 'Default Criteria Used') ~ "Yes",
                                  TRUE ~ "No"),
         excursion = case_when(IRResultNWQSunit > Final_CCC ~ 1,
                               IRResultNWQSunit <= Final_CCC ~ 0,
                               TRUE ~ NA_real_))

# this is the data file -------------------------------------------------------------------------------------------



# Categorization --------------------------------------------------------------------------------------------------

AL_tox_aluminum_assess_fun <- function(df_data = al_criteria_excursions, AU_type){

# Testing ---------------------------------------------------------------------------------------------------------
# df_data = al_criteria_excursions
# 
# AU_type = 'WS'


# 
if(AU_type == "other"){  
  group1 <- c('AU_ID', 'GNIS_Name', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name' )
  
  
  inverse <- TRUE
  
  
} else if (AU_type == "WS"){
  group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name', 'Pollu_ID', 'wqstd_code', 'Char_Name' )
  
  
  inverse <- FALSE
}


Results_tox_AL_aluminum_cats <- df_data %>%
  filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
  group_by_at(group1) %>%
  #Summarise data
  summarise(num_samples = n(),
            percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > Final_CCC )/num_samples * 100),
            num_fraction_types = n_distinct(Simplified_Sample_fraction),
            num_samples_bioavailable_fraction =sum(Simplified_Sample_fraction == "Bioavailable"), 
            num_samples_total_fraction = sum(Simplified_Sample_fraction == "Total"),
            num_Samples_dissolved_fraction = sum(Simplified_Sample_fraction == "Dissolved"),
            num_excursions_all = sum(excursion),
            num_excursions_bioavailable_fraction = sum(excursion[Simplified_Sample_fraction == "Bioavailable"]),
            num_excursions_bioavail_calc_crit =  sum(excursion[Simplified_Sample_fraction == "Bioavailable" & default_crit == "No"]),
            num_excursions_bioavail_default_crit= sum(excursion[Simplified_Sample_fraction == "Bioavailable" & default_crit == "Yes"]),
            num_excursions_total_fraction = sum(excursion[Simplified_Sample_fraction == "Total"]),
            num_excursions_dissolved_fraction = sum(excursion[Simplified_Sample_fraction == "Dissolved"]),
            critical_excursions_bioavail = binomial_excursions(num_samples_bioavailable_fraction, type = "Toxics"),
            num_samples_crit_excursion_3B = num_samples_total_fraction + num_excursions_dissolved_fraction,
            critical_excursions_3B = binomial_excursions(num_samples_crit_excursion_3B, type = "Toxics")) %>%
  # Assign categories
  mutate(IR_category = case_when(percent_3d == 100 ~ "3D",
                                 num_samples_bioavailable_fraction >= 10 & num_excursions_bioavail_calc_crit >= 10  
                                    & num_excursions_bioavail_calc_crit >= critical_excursions_bioavail ~ '5',
                                 
                                 num_samples_bioavailable_fraction >= 10 & num_excursions_bioavail_calc_crit >= 10  
                                    & num_excursions_bioavail_calc_crit < critical_excursions_bioavail ~ '2',
                                 
                                 num_samples_bioavailable_fraction > 0 & num_samples_bioavailable_fraction < 10 &
                                   num_excursions_bioavail_calc_crit >= critical_excursions_bioavail ~ '5',
                                 
                                 num_samples_bioavailable_fraction < 10 & num_excursions_bioavailable_fraction + num_excursions_total_fraction >= 1 ~ '3B',
                                 
                                 TRUE ~ '3'),
         Rationale =  case_when(percent_3d == 100 ~ paste0("Insufficient data: All results are non-detects with detection limits above criteria- ", num_samples, " total samples"),
                                num_samples_bioavailable_fraction >= 10 & num_excursions_bioavail_calc_crit >= 10  
                                & num_excursions_bioavail_calc_crit >= critical_excursions_bioavail ~ paste0("Impaired: ", num_excursions_bioavail_calc_crit, 
                                                                                                             " excursions of calculated criteria in bioavailable fraction above critical excursion value of ",
                                                                                                             critical_excursions_bioavail, ". ",num_samples_bioavailable_fraction, " bioavailable samples." ),
                                
                                num_samples_bioavailable_fraction >= 10 & num_excursions_bioavail_calc_crit >= 10  
                                & num_excursions_bioavail_calc_crit < critical_excursions_bioavail ~  paste0("Attaining: ", num_excursions_bioavail_calc_crit, 
                                                                                                             " excursions of calculated criteria in bioavailable fraction below critical excursion value of ",
                                                                                                             critical_excursions_bioavail, ". ",num_samples_bioavailable_fraction, " bioavailable samples." ),
                                
                                num_samples_bioavailable_fraction > 0 & num_samples_bioavailable_fraction < 10 &
                                  num_excursions_bioavail_calc_crit >= critical_excursions_bioavail ~ paste0("Impaired: ", num_excursions_bioavail_calc_crit, 
                                                                                                             " excursions of calculated criteria in bioavailable fraction above critical excursion value of ",
                                                                                                             critical_excursions_bioavail, ". ",num_samples_bioavailable_fraction, " bioavailable samples." ),
                                
                                num_samples_bioavailable_fraction < 10 & num_excursions_bioavailable_fraction + num_excursions_total_fraction >= 1 ~ paste0("Insufficient data: ", 
                                                                                                                                                            num_excursions_bioavailable_fraction + num_excursions_total_fraction,
                                                                                                                                                            " total excursions of criteria. ",
                                                                                                                                                            num_excursions_bioavailable_fraction, 
                                                                                                                                                            " excursions of ",
                                                                                                                                                            num_samples_bioavailable_fraction, " bioavailable samples, ",
                                                                                                                                                            num_excursions_total_fraction,
                                                                                                                                                            " excursions of ",
                                                                                                                                                            num_samples_total_fraction, 
                                                                                                                                                            " total fraction samples. "),
                                
                                TRUE ~ paste0("Insufficient data: ", 
                                              num_excursions_bioavailable_fraction + num_excursions_total_fraction + num_Samples_dissolved_fraction,
                                              " total excursions of criteria. ",
                                              num_excursions_bioavailable_fraction, 
                                              " excursions of ",
                                              num_samples_bioavailable_fraction, " bioavailable samples, ",
                                              num_excursions_total_fraction,
                                              " excursions of ",
                                              num_samples_total_fraction, 
                                              " total fraction samples, ",
                                              num_excursions_dissolved_fraction,
                                              " excursions of ",
                                              num_Samples_dissolved_fraction,
                                              " dissolved fraction samples. ",
                                              num_samples, 
                                              " total samples."))
                                 
                                   
                                 
                                )
Results_tox_AL_aluminum_cats <- join_prev_assessments(Results_tox_AL_aluminum_cats, AU_type = AU_type)

}

AL_Tox_AL_WS <- AL_tox_aluminum_assess_fun(df_data = al_criteria_excursions, AU_type = "WS")
AL_Tox_AL_other <-AL_tox_aluminum_assess_fun(df_data = al_criteria_excursions, AU_type = "other") %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code))


WS_AU_rollup <- AL_Tox_AL_WS %>%
  select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  Char_Name, IR_category, Rationale) %>%
  ungroup() %>%
  group_by(AU_ID, Char_Name, Pollu_ID, wqstd_code) %>%
  summarise(IR_category_AU = max(IR_category),
            Rationale_AU = str_c(MLocID, ": ", Rationale, collapse =  " ~ " ) ) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code))

WS_AU_rollup <- join_prev_assessments(WS_AU_rollup, AU_type = 'other')

Results_tox_Al_AL <- list(data =     al_criteria_excursions,
                          AL_Tox_AL_WS = AL_Tox_AL_WS,
                          AL_Tox_AL_other = AL_Tox_AL_other,
                          AL_Tox_AL_WS_rollup = WS_AU_rollup)

return(Results_tox_Al_AL)

}