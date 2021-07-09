


# Testing and development settings --------------------------------------------------------------------------------


df <- Bacteria_results
write_excel = TRUE

Coastal <- df %>%
  filter(Bacteria_code %in%  c(1, 3),
         Char_Name == "Enterococcus")

if(length(unique(Coastal$AU_ID)) == 0) {
  stop("No Enterococcus Data")
} 


coast_contact_geomeans <- Coastal %>%
  mutate(Geomean_Crit = 35,
         SS_Crit = 130 ) %>%
  group_by(MLocID) %>%
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
  arrange(AU_ID, MLocID, SampleStartDate )  %>%
  mutate(geomean_excursion = case_when(is.na(geomean) ~ "no: < 5 samples in 90 day period",
                                       geomean >  Geomean_Crit ~ "yes",
                                       TRUE ~ "no"),
         single_sample_excursion = ifelse(Result_cen > SS_Crit, "yes", "no" ),
         single_sample_90_day_above_10perc_excursion = case_when(is.na(percent_excur_90) ~ "no",
                                                                 percent_excur_90 > 0.1 ~ "yes",
                                                                 TRUE ~ "no" ))


# Categorization --------------------------------------------------------------------------------------------------


# NON Watershed unit categorization -----------------------------------------------------------------------------------

coast_AU_summary_no_WS <-  coast_contact_geomeans %>%
  filter(str_detect(AU_ID, "WS", negate = TRUE)) %>%
  arrange(MLocID) %>%
  ungroup() %>%
  group_by(AU_ID,  Pollu_ID, wqstd_code ) %>%
  summarise(OWRD_Basin = first(OWRD_Basin), 
            num_Samples = as.numeric(n()),
            Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
            max.value  = max(Result_cen),
            num_ss_excursions = as.numeric(sum(Result_cen > SS_Crit)),
            max_ss_excursions_90_day = max(ss_count_excur_90),
            max_ss_percent_excursion_90_day = max(percent_excur_90),
            geomean_over = ifelse(!is.na(Max_Geomean) &
                                    Max_Geomean > max(Geomean_Crit), 1, 0),
            geomean_exceed_date_periods = case_when(geomean_over == 1 ~ str_c(na.omit(unique(dates_90[geomean > Geomean_Crit])), collapse = " and ")),
            mlocs_geomean_exceed = ifelse(geomean_over == 1, str_c(unique(na.omit(MLocID[geomean > Geomean_Crit]), collapse = "; ")),NA),
            
  )
