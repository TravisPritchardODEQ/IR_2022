library(lubridate)
library(runner)


# Testing and development settings --------------------------------------------------------------------------------


df <- Bacteria_results
write_excel = TRUE

Coastal <- df %>%
  filter(Bacteria_code %in%  c(1, 3),
         Char_Name == "Enterococcus")

if(length(unique(Coastal$AU_ID)) == 0) {
  stop("No Enterococcus Data")
} 

# NON Watershed unit categorization -----------------------------------------------------------------------------------
coast_contact_geomeans_no_WS <- Coastal %>%
  filter(str_detect(AU_ID, "WS", negate = TRUE)) %>%
  mutate(Geomean_Crit = 35,
         SS_Crit = 130 ) %>%
  group_by(AU_ID) %>%
  arrange(SampleStartDate) %>%
  dplyr::mutate(d = runner(x = data.frame(SampleStartDate  = SampleStartDate,
                                          Result_cen = Result_cen,
                                          Geomean_Crit = Geomean_Crit,
                                          SS_Crit = SS_Crit),
                           k = "90 days",
                           lag = 0,
                           idx = SampleStartDate,
                           f = function(x) list(x)))%>%
  dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                 dplyr::summarise(geomean = dplyr::case_when(n_distinct(SampleStartDate) >= 5 ~  geo_mean(Result_cen),
                                                                             TRUE ~ NA_real_),
                                                  count_90 = dplyr::n_distinct(SampleStartDate),
                                                  dates_90 = stringr::str_c(unique(SampleStartDate),  collapse = "; "),
                                                  date_range_90 = paste(min(SampleStartDate), "-", max(SampleStartDate)),
                                                  geomean_excur = ifelse(!all(is.na(geomean)) & geomean > max(Geomean_Crit), 1, 0),
                                                  #ss_excur_90 = ifelse(!is.na(Result_cen) & Result_cen > SS_Crit, 1, 0),
                                                  ss_count_excur_90 = n_distinct(SampleStartDate[Result_cen > SS_Crit]),
                                                  percent_excur_90 = ifelse(count_90 >= 5, n_distinct(SampleStartDate[Result_cen > SS_Crit])/ count_90, NA_real_) ,
                                                  ss_excur_dates = case_when(ss_count_excur_90 > 0 ~ stringr::str_c(unique(SampleStartDate [Result_cen > SS_Crit]),  collapse = "; ") ,
                                                                             ss_count_excur_90 == 0 ~ NA_character_) ,
                                                  comment = dplyr::case_when(length(SampleStartDate) < 5 ~  'Not enough values to calculate geometric mean',
                                                                             TRUE ~ NA_character_) 
                                 )
                               
  )) %>%
  tidyr::unnest_wider(d) %>%
  arrange(AU_ID, SampleStartDate )  %>%
  mutate(geomean_excursion = case_when(is.na(geomean) ~ "no: < 5 samples in 90 day period",
                                       geomean >  Geomean_Crit ~ "yes",
                                       TRUE ~ "no"),
         single_sample_excursion = ifelse(Result_cen > SS_Crit, "yes", "no" ),
         single_sample_90_day_above_10perc_excursion = case_when(is.na(percent_excur_90) ~ "no",
                                                                 percent_excur_90 > 0.1 ~ "yes",
                                                                 TRUE ~ "no" ))


# Categorization --------------------------------------------------------------------------------------------------




coast_AU_summary_no_WS <-  coast_contact_geomeans_no_WS %>%
  filter(str_detect(AU_ID, "WS", negate = TRUE)) %>%
  arrange(MLocID) %>%
  ungroup() %>%
  group_by(AU_ID,  Pollu_ID, wqstd_code, OWRD_Basin ) %>%
  summarise(num_Samples = as.numeric(n()),
            Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
            max.value  = max(Result_cen),
            num_ss_excursions = as.numeric(sum(Result_cen > SS_Crit)),
            max_ss_excursions_90_day = max(ss_count_excur_90, na.rm = TRUE),
            max_ss_percent_excursion_90_day = ifelse(is.finite(max(percent_excur_90, na.rm = TRUE)),  
                                                                round(max(percent_excur_90, na.rm = TRUE), 2), 
                                                                NA_real_),
            n_90day_percent_exceed = sum(percent_excur_90 > 0.10, na.rm = TRUE),
            geomeans_calculated = sum(!is.na(geomean)),
            geomean_excursions = ifelse(!is.na(Max_Geomean) &
                                    Max_Geomean > max(Geomean_Crit), "yes", "no"),
            n_geomean_excursions = sum(geomean_excur),
            geomean_exceed_date_periods = case_when(geomean_excursions == "yes" ~ str_c(na.omit(unique(date_range_90[geomean > Geomean_Crit])), collapse = "; ")),
            mlocs_geomean_exceed = ifelse(geomean_excursions == "yes", str_c(unique(na.omit(MLocID[geomean > Geomean_Crit]), collapse = "; ")),NA),
            ss_exceed_date_periods = str_c(unique(SampleStartDate[Result_cen > SS_Crit]), collapse =  "; ")
              ) %>%
  mutate(IR_category = case_when(geomean_excursions == "yes" ~ "5",
                                 max_ss_percent_excursion_90_day > 0.10 ~ "5",
                                 geomeans_calculated == 0 & num_ss_excursions > 0 ~ "3B",
                                 geomeans_calculated == 0 & num_ss_excursions == 0 ~ "3",
                                 geomeans_calculated >= 1 & geomean_excursions == "no" & max_ss_percent_excursion_90_day <= 0.10 ~ '2',
                                 TRUE ~ "ERROR"),
         Rationale = case_when(geomean_excursions == "yes" ~ paste0(n_geomean_excursions, 
                                                                    " geometric means exceed geomean criteria in time periods ",
                                                                    geomean_exceed_date_periods),
                               max_ss_percent_excursion_90_day > 0.10 ~ paste0(n_geomean_excursions, 
                                                                               " geometric means exceed geomean criteria. Single sample exceedance rate > 10% ",
                                                                               n_90day_percent_exceed, " times"),
                               geomeans_calculated == 0 & num_ss_excursions > 0 ~ paste0("No 90 day period has at least 5 results. ", 
                                                                                         num_ss_excursions, " single sample excursions"),
                               geomeans_calculated == 0 & num_ss_excursions == 0 ~ paste0("No 90 day period has at least 5 results. ", 
                                                                                          num_ss_excursions, " single sample excursions"),
                               geomeans_calculated >= 1 & geomean_excursions == "no" & max_ss_percent_excursion_90_day <= 0.10 ~ "No excursions of geometric mean. No 90 day period > 10% single sample exceedance rate",
                               TRUE ~ "ERROR"),
         ) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code ))



# Watershed unit categorization -----------------------------------------------------------------------------------



coast_contact_geomeans_WS <- Coastal %>%
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  mutate(Geomean_Crit = 35,
         SS_Crit = 130 ) %>%
  group_by(MLocID, AU_ID) %>%
  arrange(SampleStartDate) %>%
  dplyr::mutate(d = runner(x = data.frame(SampleStartDate  = SampleStartDate,
                                          Result_cen = Result_cen,
                                          Geomean_Crit = Geomean_Crit,
                                          SS_Crit = SS_Crit),
                           k = "90 days",
                           lag = 0,
                           idx = SampleStartDate,
                           f = function(x) list(x)))%>%
  dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                 dplyr::summarise(geomean = dplyr::case_when(n_distinct(SampleStartDate) >= 5 ~  geo_mean(Result_cen),
                                                                             TRUE ~ NA_real_),
                                                  count_90 = dplyr::n_distinct(SampleStartDate),
                                                  dates_90 = stringr::str_c(unique(SampleStartDate),  collapse = "; "),
                                                  date_range_90 = paste(min(SampleStartDate), "-", max(SampleStartDate)),
                                                  geomean_excur = ifelse(!all(is.na(geomean)) & geomean > max(Geomean_Crit), 1, 0),
                                                  #ss_excur_90 = ifelse(!is.na(Result_cen) & Result_cen > SS_Crit, 1, 0),
                                                  ss_count_excur_90 = n_distinct(SampleStartDate[Result_cen > SS_Crit]),
                                                  percent_excur_90 = ifelse(count_90 >= 5, n_distinct(SampleStartDate[Result_cen > SS_Crit])/ count_90, NA_real_) ,
                                                  ss_excur_dates = case_when(ss_count_excur_90 > 0 ~ stringr::str_c(unique(SampleStartDate [Result_cen > SS_Crit]),  collapse = "; ") ,
                                                                             ss_count_excur_90 == 0 ~ NA_character_) ,
                                                  comment = dplyr::case_when(length(SampleStartDate) < 5 ~  'Not enough values to calculate geometric mean',
                                                                             TRUE ~ NA_character_) 
                                 )
                               
  )) %>%
  tidyr::unnest_wider(d) %>%
  arrange(MLocID, SampleStartDate)  %>%
  mutate(geomean_excursion = case_when(is.na(geomean) ~ "no: < 5 samples in 90 day period",
                                       geomean >  Geomean_Crit ~ "yes",
                                       TRUE ~ "no"),
         single_sample_excursion = ifelse(Result_cen > SS_Crit, "yes", "no" ),
         single_sample_90_day_above_10perc_excursion = case_when(is.na(percent_excur_90) ~ "no",
                                                                 percent_excur_90 > 0.1 ~ "yes",
                                                                 TRUE ~ "no" ))


# Categorization --------------------------------------------------------------------------------------------------




coast_AU_summary_WS <-  coast_contact_geomeans_WS %>%
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  arrange(MLocID) %>%
  ungroup() %>%
  group_by(MLocID, AU_ID, Pollu_ID, wqstd_code, OWRD_Basin ) %>%
  summarise(num_Samples = as.numeric(n()),
            Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
            max.value  = max(Result_cen),
            num_ss_excursions = as.numeric(sum(Result_cen > SS_Crit)),
            max_ss_excursions_90_day = max(ss_count_excur_90, na.rm = TRUE),
            max_ss_percent_excursion_90_day = ifelse(is.finite(max(percent_excur_90, na.rm = TRUE)),  
                                                     round(max(percent_excur_90, na.rm = TRUE), 2), 
                                                     NA_real_),
            n_90day_percent_exceed = sum(percent_excur_90 > 0.10, na.rm = TRUE),
            geomeans_calculated = sum(!is.na(geomean)),
            geomean_excursions = ifelse(!is.na(Max_Geomean) &
                                          Max_Geomean > max(Geomean_Crit), "yes", "no"),
            n_geomean_excursions = sum(geomean_excur),
            geomean_exceed_date_periods = case_when(geomean_excursions == "yes" ~ str_c(na.omit(unique(date_range_90[geomean > Geomean_Crit])), collapse = "; ")),
            mlocs_geomean_exceed = ifelse(geomean_excursions == "yes", str_c(unique(na.omit(MLocID[geomean > Geomean_Crit]), collapse = "; ")),NA),
            ss_exceed_date_periods = str_c(unique(SampleStartDate[Result_cen > SS_Crit]), collapse =  "; ")
  ) %>%
  mutate(IR_category = case_when(geomean_excursions == "yes" ~ "5",
                                 max_ss_percent_excursion_90_day > 0.10 ~ "5",
                                 geomeans_calculated == 0 & num_ss_excursions > 0 ~ "3B",
                                 geomeans_calculated == 0 & num_ss_excursions == 0 ~ "3",
                                 geomeans_calculated >= 1 & geomean_excursions == "no" & max_ss_percent_excursion_90_day <= 0.10 ~ '2',
                                 TRUE ~ "ERROR"),
         Rationale = case_when(geomean_excursions == "yes" ~ paste0(n_geomean_excursions, 
                                                                    " geometric means exceed geomean criteria in time periods ",
                                                                    geomean_exceed_date_periods),
                               max_ss_percent_excursion_90_day > 0.10 ~ paste0(n_geomean_excursions, 
                                                                               " geometric means exceed geomean criteria. Single sample exceedance rate > 10% ",
                                                                               n_90day_percent_exceed, " times"),
                               geomeans_calculated == 0 & num_ss_excursions > 0 ~ paste0("No 90 day period has at least 5 results. ", 
                                                                                         num_ss_excursions, " single sample excursions"),
                               geomeans_calculated == 0 & num_ss_excursions == 0 ~ paste0("No 90 day period has at least 5 results. ", 
                                                                                          num_ss_excursions, " single sample excursions"),
                               geomeans_calculated >= 1 & geomean_excursions == "no" & max_ss_percent_excursion_90_day <= 0.10 ~ "No excursions of geometric mean. No 90 day period > 10% single sample exceedance rate",
                               TRUE ~ "ERROR"),
  ) %>%
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE))


WS_AU_rollup <- coast_AU_summary_WS %>%
  ungroup() %>%
  group_by(AU_ID, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise(IR_category_AU = max(IR_category),
            Rationale = str_c(Rationale,collapse =  " ~ " ) ) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code ))


if(write_excel){
  
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = "Coast Bacteria Data_WS")
  addWorksheet(wb, sheetName = "WS station categorization")
  addWorksheet(wb, sheetName = "WS AU categorization")
  addWorksheet(wb, sheetName = "Coast Bacteria Data_Other")
  addWorksheet(wb, sheetName = "Other AU categorization")
  
  header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
  freezePane(wb, "Coast Bacteria Data_WS", firstRow = TRUE) 
  freezePane(wb, "WS station categorization", firstRow = TRUE)
  freezePane(wb, "WS AU categorization", firstRow = TRUE)
  freezePane(wb,  "Coast Bacteria Data_Other", firstRow = TRUE)
  freezePane(wb, "Other AU categorization", firstRow = TRUE)

  
  writeData(wb = wb, sheet = "Coast Bacteria Data_WS", x = coast_contact_geomeans_WS, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS station categorization", x = coast_AU_summary_WS, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS AU categorization", x = WS_AU_rollup, headerStyle = header_st)
  writeData(wb = wb, sheet =  "Coast Bacteria Data_Other", x = coast_contact_geomeans_no_WS, headerStyle = header_st)
  writeData(wb = wb, sheet = "Other AU categorization", x = coast_AU_summary_no_WS, headerStyle = header_st )
  
  
  print("Writing excel doc")
  saveWorkbook(wb, "Parameters/Bacteria/bacteria coast contact.xlsx", overwrite = TRUE) 
  
}


