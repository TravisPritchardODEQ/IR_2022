

fresh_contact_rec <- function(df, write_excel = TRUE){


  

# Testing and development settings --------------------------------------------------------------------------------

  
  #df <- Bacteria_results  
  #write_excel = TRUE  
library(runner)
library(openxlsx)

fresh_contact <- df %>%
  filter(Bacteria_code == 2,
         Char_Name == "Escherichia coli") 

if(length(unique(fresh_contact$AU_ID)) == 0) {
  stop("No E coli Data")
} 

# Geometric mean calculations --------------------------------------------

#Analyze by monitoring location 
#Add column (d) which is a rolling 90 day period dataframe of date and results
#Calculate geomena if >= 5 samples in 90 day period
#Number of samples in 90 day period (count_90)
#String of unique dates in 90 day period
  #This is used to create ratioanale
#Comment if geomean is able to be calculated

#Pull it back into main datafrmae with unnest_wider

# Create 2 columns for reviewer assistance
  #geomean_excursion - If the geomean associated with that day is an excursion of geomean crit
  #single_sample_excursion- If that day's result is an excursion of the single sample crit


fresh_contact_geomeans <- fresh_contact %>%
  group_by(MLocID) %>%
  arrange(SampleStartDate) %>%
  dplyr::mutate(d = runner(x = data.frame(SampleStartDate  = SampleStartDate,
                                          Result_cen = Result_cen),
                           k = "90 days",
                           lag = 0,
                           idx = SampleStartDate,
                           f = function(x) list(x))) %>%
  dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                 dplyr::summarise(geomean = dplyr::case_when(length(SampleStartDate) >= 5 ~  geo_mean(Result_cen),
                                                                             TRUE ~ NA_real_),
                                                  count_90 = dplyr::n_distinct(SampleStartDate),
                                                  dates_90 = stringr::str_c(unique(SampleStartDate),  collapse = "; "),
                                                  comment = dplyr::case_when(length(SampleStartDate) < 5 ~  'Not enough values to calculate geometric mean',
                                                                            TRUE ~ NA_character_) )
  )) %>%
  tidyr::unnest_wider(d) %>%
  arrange(AU_ID, MLocID, SampleStartDate )  %>%
  mutate(geomean_excursion = case_when(is.na(geomean) ~ "no: < 5 samples in 90 day period",
                                       geomean >  Geomean_Crit ~ "yes",
                                       TRUE ~ "no"),
         single_sample_excursion = ifelse(Result_cen > SS_Crit, "yes", "no" ))
  



# Categorization --------------------------------------------------------------------------------------------------


# Watershed unit categorization -----------------------------------------------------------------------------------


fresh_AU_summary_WS0 <-  fresh_contact_geomeans %>%
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  arrange(MLocID) %>%
  ungroup() %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code ) %>%
  summarise(OWRD_Basin = first(OWRD_Basin), 
            Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
            max.value  = max(Result_cen),
            num_Samples = as.numeric(n()),
            num_ss_excursions = as.numeric(sum(Result_cen > SS_Crit)),
            critical_excursions = binomial_excursions(num_Samples, "Conventionals"),
            SS_Crit = max(SS_Crit),
            Geomean_Crit = max(Geomean_Crit),
            geomean_over = ifelse(!is.na(Max_Geomean) &
                                    Max_Geomean > Geomean_Crit, 1, 0),
            geomean_exceed_date_periods = ifelse(geomean_over == 1 ,str_c(na.omit(unique(dates_90[geomean > Geomean_Crit]), collapse = " AND ")),NA),
            ss_exceed_date_periods = str_c(unique(SampleStartDate[Result_cen > SS_Crit]), collapse =  "; ")
  ) %>%
  mutate(IR_category = case_when(geomean_over == 1 ~ "5",
                                 num_Samples >= 5 & num_ss_excursions > critical_excursions ~  "5",
                                 is.na(Max_Geomean) & max.value < SS_Crit & num_Samples < 5 ~ "3",
                                 is.na(Max_Geomean) & max.value > SS_Crit & num_Samples < 5 ~ "3B",
                                 !is.na(Max_Geomean) & Max_Geomean <= Geomean_Crit ~ "2",
                                 is.na(Max_Geomean) & num_Samples >= 5 & num_ss_excursions <= critical_excursions ~ "2",
                                 TRUE ~ "ERROR"), 
         Rationale = case_when(geomean_over == 1 ~ paste(MLocID, ": Geomeans exceed criteria value of", Geomean_Crit, "for time periods",geomean_exceed_date_periods, "-",num_Samples, "total samples"  ),
                        num_Samples >= 5 & num_ss_excursions > critical_excursions ~  paste(MLocID, ": Single samples exceed criteria value of",
                                                                                            SS_Crit, num_ss_excursions, 'times on', 
                                                                                            ss_exceed_date_periods, "-",num_Samples, "total samples" ),
                        is.na(Max_Geomean) & max.value < SS_Crit & num_Samples < 5 ~ paste(MLocID, ": Insufficient Data -",
                                                                                           num_Samples, 
                                                                                           "samples with no exceedances", "-",num_Samples, "total samples" ),
                        is.na(Max_Geomean) & max.value > SS_Crit & num_Samples < 5 ~ paste(MLocID, ": Insufficient Data - ",
                                                                                           num_Samples, 
                                                                                           "samples with", 
                                                                                           num_ss_excursions, 
                                                                                           "excursions", "-",num_Samples, "total samples"  ),
                        !is.na(Max_Geomean) & Max_Geomean <= Geomean_Crit ~ paste(MLocID, ": Attaining- No geomean above criteria", "-",num_Samples, "total samples" ),
                        is.na(Max_Geomean) & num_Samples >= 5 & num_ss_excursions <= critical_excursions ~ paste(MLocID, ": Attaining- No geomean calculated. Single sample excursions below binomial", "-",num_Samples, "total samples"  ),
                        TRUE ~ "ERROR"
  )
  ) %>%
  #Set category as ordered factor to allow for easier rollup
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE)) %>%
  select(-geomean_over, -geomean_exceed_date_periods, -ss_exceed_date_periods)

fresh_AU_summary_WS <- join_prev_assessments(fresh_AU_summary_WS0, AU_type = "WS")


# WS AU rollup ----------------------------------------------------------------------------------------------------

WS_AU_rollup0 <- fresh_AU_summary_WS %>%
  ungroup() %>%
  group_by(AU_ID, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise(IR_category_AU = max(IR_category),
            Rationale_AU = str_c(Rationale,collapse =  " ~ " ) ) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code ))

WS_AU_rollup <- join_prev_assessments(WS_AU_rollup0, AU_type = "Other")
# Non- watershed unit categorization ---------------------------------------------------------------------------------------------------



fresh_AU_summary_no_WS0 <-  fresh_contact_geomeans %>%
  filter(!str_detect(AU_ID, "WS")) %>%
  ungroup() %>%
  group_by(AU_ID, Pollu_ID, wqstd_code ) %>%
  summarise(OWRD_Basin = first(OWRD_Basin), 
            Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
            max.value  = max(Result_cen),
            num_Samples = as.numeric(n()),
            num_ss_excursions = as.numeric(sum(Result_cen > SS_Crit)),
            critical_excursions = binomial_excursions(num_Samples, "Conventionals"),
            SS_Crit = max(SS_Crit),
            Geomean_Crit = max(Geomean_Crit),
            mlocs = str_c(unique(MLocID), collapse = "; "),
            mlocs_ss_exceed = str_c(unique(MLocID[Result_cen > SS_Crit]), collapse = "; "),
            geomean_over = ifelse(!is.na(Max_Geomean) &
                                    Max_Geomean > Geomean_Crit, 1, 0),
            mlocs_geomean_exceed = ifelse(geomean_over == 1, str_c(unique(na.omit(MLocID[geomean > Geomean_Crit]), collapse = "; ")),NA),
            geomean_exceed_date_periods = ifelse(geomean_over == 1 ,str_c(na.omit(unique(dates_90[geomean > Geomean_Crit]), collapse = "; ")),NA),
            ss_exceed_date_periods = str_c(unique(SampleStartDate[Result_cen > SS_Crit]), collapse =  "; ")) %>%
  mutate(IR_category = case_when(geomean_over == 1 ~ "5",
                                 num_Samples >= 5 & num_ss_excursions >= critical_excursions ~  "5",
                                 is.na(Max_Geomean) & max.value < SS_Crit & num_Samples < 5 ~ "3",
                                 is.na(Max_Geomean) & max.value > SS_Crit & num_Samples < 5 ~ "3B",
                                 !is.na(Max_Geomean) & Max_Geomean <= Geomean_Crit ~ "2",
                                 is.na(Max_Geomean) & num_Samples >= 5 & num_ss_excursions < critical_excursions ~ "2",
                                 TRUE ~ "ERROR"),
         Rationale = case_when(geomean_over == 1 ~ paste0("Impaired: Geomeans at ", mlocs_geomean_exceed, " exceed criteria value of ", Geomean_Crit, " for time periods ",geomean_exceed_date_periods, "- ",num_Samples, " total samples"  ),
                               num_Samples >= 5 & num_ss_excursions > critical_excursions ~  paste0("Impaired: Single samples exceed criteria value of ",
                                                                                                   SS_Crit, " ", num_ss_excursions, ' times on ', 
                                                                                                   ss_exceed_date_periods, " at ", mlocs_ss_exceed,   "- ",num_Samples, " total samples" ),
                               is.na(Max_Geomean) & max.value < SS_Crit & num_Samples < 5 ~ paste0("Insufficient Data: ",
                                                                                                  num_Samples, 
                                                                                                   " samples with no exceedances", "- ",num_Samples, " total samples at ", mlocs ),
                               is.na(Max_Geomean) & max.value > SS_Crit & num_Samples < 5 ~ paste0("Insufficient Data: ",
                                                                                                  num_Samples, 
                                                                                                  " samples with ", 
                                                                                                  num_ss_excursions, 
                                                                                                  " excursions", "- ",num_Samples, " total samples at ", mlocs  ),
                               !is.na(Max_Geomean) & Max_Geomean <= Geomean_Crit ~ paste0("Attaining: No geomean above criteria", "- ",num_Samples, " total samples at ", mlocs ),
                               is.na(Max_Geomean) & num_Samples >= 5 & num_ss_excursions <= critical_excursions ~ paste0("Attaining: No geomean calculated. Single sample excursions below binomial", "- ",num_Samples, " total samples at", mlocs  ),
                               TRUE ~ "ERROR"
         )) %>%
  select(-geomean_over, -geomean_exceed_date_periods, -ss_exceed_date_periods) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code ))

fresh_AU_summary_no_WS <- join_prev_assessments(fresh_AU_summary_no_WS0, AU_type = "Other")

# Create excel doc ------------------------------------------------------------------------------------------------
if(write_excel){

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Fresh Bacteria Data")
addWorksheet(wb, sheetName = "WS station categorization")
addWorksheet(wb, sheetName = "WS AU categorization")
addWorksheet(wb, sheetName = "Other AU categorization")

header_st <- createStyle(textDecoration = "Bold", border = "Bottom")

writeData(wb = wb, sheet = "Fresh Bacteria Data", x = fresh_contact_geomeans, headerStyle = header_st)
writeData(wb = wb, sheet = "WS station categorization", x = fresh_AU_summary_WS, headerStyle = header_st)
writeData(wb = wb, sheet = "WS AU categorization", x = WS_AU_rollup, headerStyle = header_st)
writeData(wb = wb, sheet = "Other AU categorization", x = fresh_AU_summary_no_WS, headerStyle = header_st )


print("Writing excel doc")
saveWorkbook(wb, "Parameters/Outputs/bacteria freshwater contact.xlsx", overwrite = TRUE) 

}



# Function output list --------------------------------------------------------------------------------------------


bacteria_freshwater <-list(fresh_bacteria_data=as.data.frame(fresh_contact_geomeans),
                           ws_station_categorization=as.data.frame(fresh_AU_summary_WS),
                           ws_au_categorization=as.data.frame(WS_AU_rollup),
                           other_au_categorization=as.data.frame(fresh_AU_summary_no_WS))

return(bacteria_freshwater)
}
