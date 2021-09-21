library(tidyverse)
library(odeqIRtools) 
library(odbc)
library(DBI)
library(glue)
library(deqalcrit)



# Testing ---------------------------------------------------------------------------------------------------------

database <- "IR_Dev"  

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
  mutate(Simplified_sample_fraction = ifelse(Sample_Fraction %in% c("Total", "Extractable",
                                                                    "Total Recoverable","Total Residual",
                                                                    "None", "volatile", "Semivolatile",
                                                                    "Acid Soluble", "Suspended")  |
                                               is.na(Sample_Fraction), 'Total',
                                             ifelse(Sample_Fraction == "Dissolved"  |
                                                      Sample_Fraction == "Filtered, field"  |
                                                      Sample_Fraction == "Filtered, lab"  , "Dissolved", "Error"))) %>%
  group_by(MLocID,  SampleStartDate,Char_Name,SampleMedia, SampleSubmedia ) %>%
  mutate(Has_dissolved = ifelse(min(Simplified_sample_fraction) == "Dissolved", 1, 0 )) %>%
  ungroup() %>%
  filter((Has_dissolved == 1 & Simplified_sample_fraction == "Dissolved") | Has_dissolved == 0) %>%
  select(-Has_dissolved) %>%
  #mutate(Char_Name = paste0(Char_Name, "-", Simplified_sample_fraction)) %>%
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
  mutate(Simplified_sample_fraction = case_when(Sample_Fraction %in% c("Total", "Extractable",
                                                                    "Total Recoverable","Total Residual",
                                                                    "None", "volatile", "Semivolatile",
                                                                    "Acid Soluble", "Suspended")  ~ 'Total',
                                                Sample_Fraction == "Dissolved"  |
                                                      Sample_Fraction == "Filtered, field"  |
                                                      Sample_Fraction == "Filtered, lab"  ~ "Dissolved", 
                                                Sample_Fraction == "Bioavailable" ~ "Bioavailable",
                                                TRUE ~ "ERROR")) %>%
  group_by(MLocID, SampleStartDate) %>%
  mutate(has_bioavailable = case_when(any(Simplified_sample_fraction == "Bioavailable", na.rm = TRUE) ~ 1,
                                      TRUE ~ 0),
         has_total = case_when(any(Simplified_sample_fraction == "Total", na.rm = TRUE) ~ 1,
                                       TRUE ~ 0)) %>%
  ungroup() %>%
  mutate(keep = case_when(has_bioavailable == 1 & Simplified_sample_fraction == "Bioavailable" ~ 1,
                          has_bioavailable == 0 & has_total == 1 & Simplified_sample_fraction == "Total" ~ 1,
                          has_bioavailable == 0 & has_total == 0 & Simplified_sample_fraction == "Dissolved" ~ 1,
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
  mutate(excursion = case_when(IRResultNWQSunit > Final_CCC ~ 1,
                               IRResultNWQSunit <= Final_CCC ~ 0,
                               TRUE ~ NA_real_))