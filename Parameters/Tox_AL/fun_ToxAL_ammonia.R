# Note, this was done using the salmonid species present criteria equations. Need to adjust for no salmonid species

ToxAL_Ammonia <- function(database){
  
  
  print("Fetch Ammonia data from IR database")
  
  #open connection to database
  con <- DBI::dbConnect(odbc::odbc(), database)
  
  #Build query language to get Pentachlorophenol data out. this grabs the IR 2018 db view [dbo].[VW_Pentachlorophenol]
  
  db_qry <- glue::glue_sql( "SELECT *
                          FROM [IntegratedReport].[dbo].[VW_Ammonia_AL]
                          WHERE AU_ID != '99'", .con = con)
  
  # Send query to database and return with the data
  Results_import <-  DBI::dbGetQuery(con, db_qry)
  
  Results_import_no_NAs <- Results_import %>%
    filter(!is.na(MLocID))
  
  print(paste("Returned", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations"))
  
  #Create a vector of monitoring locations with metals data. This list is used as a filter for the hardness query
  mlocs <- unique(Results_import_no_NAs$MLocID)
  
  # chr_uids for ammonia ancillary data
  # Temp = 2849
  # pH = 1648
  
  print("Fetch ancillary data from IR database")
  ancillary_qry <- glue::glue_sql("SELECT [MLocID]
                                ,[SampleStartDate]
                                ,[Char_Name]
                                ,[Char_Speciation]
                                ,[Sample_Fraction]
                                ,[IRResultNWQSunit]
                                ,[Result_Depth]
                                ,[IRWQSUnitName]
                                FROM [IntegratedReport].[dbo].[InputRaw]
                                WHERE chr_uid in (2849, 1648) 
                                AND (Statistical_Base IS NULL)
                                AND MLocID in ({mlocs*})
                                AND IRResultNWQSunit IS NOT NULL", .con = con)
  
  
  
  #Query to get ancillary data
  Results_ancillary <- DBI::dbGetQuery(con, ancillary_qry)
  
  
  print("Joining ancillary data")
  
  # Spread the data from long format to wide format
  spread <- Results_ancillary %>%
    filter(!Sample_Fraction %in% c("Suspended")) %>%
    group_by(MLocID, SampleStartDate,Char_Name,Result_Depth  ) %>%
    summarise(result = first(IRResultNWQSunit)) %>%
    arrange(MLocID, SampleStartDate) %>%
    spread(key = Char_Name, value = result) %>%
    rename(Temp = "Temperature, water") %>%
    mutate(pH = ifelse(pH < 6.5, 6.5, 
                       ifelse(pH > 9.0, 9.0, pH )))
  
  
  # Join table together
  # Calculate crit
  # Enter fish codes for non-slmonid fish use
  ammonia_data <- Results_import %>%
    left_join(spread, by = c('MLocID', 'SampleStartDate', 'Result_Depth')) %>%
    filter(!is.na(pH) & !is.na(Temp)) %>%
    mutate(crit = ifelse(FishCode %in% c(11, 21, 99), 0.7249 * (0.0114 / (1 + 10^7204-pH)) + (1.6181 / (1 + 10 ^(pH-7204)) * min(51.93, 23.13*10^(0.036*(20-Temp)))),  
                         pmin((0.275/(1 + 10 ^ (7.204 - pH))) + (39.0/ (1 + 10 ^(pH - 7.204 ))) , 
                              0.7249 * ((0.0114/ ( 1 + 10^(7.204 - pH))) + (1.6181 / (1 + 10 ^ (pH - 7.204))) * (23.12*10^(0.036*(20-Temp))))) )  
    )
  
  Results_censored <- censor_data(ammonia_data, criteria_col =  crit ) %>%
    mutate(Simplfied_Sample_Fraction = ifelse(Sample_Fraction %in% c("Dissolved", "Filterable"),  "Dissolved", "Total" )) %>%
    group_by(MLocID, SampleStartDate, SampleStartTime ,Char_Name,Result_Depth ) %>%
    mutate(has_total = ifelse(max(Simplfied_Sample_Fraction) == "Total", 1, 0 )) %>%
    ungroup() %>%
    filter((has_total == 1 & Simplfied_Sample_Fraction == "Total") |
             (has_total == 0 & Simplfied_Sample_Fraction == "Dissolved") ) %>%
    mutate(excursion = ifelse(Result_cen > crit, 1, 0 ))
  
  
  ammonia_cat_fun <- function(Results_censored, AU_type){
    
    
    if(AU_type == "other"){  
      group1 <- c('AU_ID', 'AU_GNIS_Name', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name', 'Crit_fraction' )
      
      group2 <- c('AU_ID', 'Char_Name')
      inverse <- TRUE
      
      
    } else if (AU_type == "WS"){
      group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name', 'Crit_fraction')
      
      group2 <- c('AU_ID', 'MLocID', 'Char_Name')
      inverse <- FALSE
    }
    
    
    Results_tox_Ammonia_categories <- Results_censored %>%
      filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
      group_by_at(group1) %>%
      #Summarise data
      summarise( num_samples = n(),
                percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > crit )/num_samples * 100),
                num_fraction_types = n_distinct(Simplfied_Sample_Fraction),
                num_samples_total_fraction = sum(Simplfied_Sample_Fraction == "Total"),
                num_Samples_dissolved_fraction = sum(Simplfied_Sample_Fraction == "Dissolved"),
                num_excursions_all = sum(excursion),
                num_excursions_total_fraction = sum(excursion[Simplfied_Sample_Fraction == "Total"]),
                num_excursions_dissolved_fraction = sum(excursion[Simplfied_Sample_Fraction == "Dissolved"]),
                num_samples_crit_excursion_calc =  num_samples_total_fraction + num_excursions_dissolved_fraction, 
                critical_excursions = binomial_excursions(num_samples_crit_excursion_calc, type = "Toxics")) %>%
      # Assign categories
      mutate(IR_category = case_when(percent_3d == 100 ~ "3D",
                                     num_samples_crit_excursion_calc > 2 & num_excursions_all >= critical_excursions ~ "5",
                                     num_samples_crit_excursion_calc < 10 & num_excursions_all > 0 ~ "3B",
                                     num_samples_crit_excursion_calc < 10 & num_excursions_all == 0 ~ "3",
                                     num_samples_crit_excursion_calc >= 10 &num_excursions_all < critical_excursions~  "2" ),
             Rationale =  case_when(percent_3d == 100 ~paste0("Insufficient data: ", "All results are non-detects with detection limits above criteria- ", num_samples, " total samples"),
                                    num_samples_crit_excursion_calc > 2 & num_excursions_all >= critical_excursions ~   paste0("Imapired: ",num_excursions_all,
                                                                                                                               " excursion of criteria with ",
                                                                                                                               num_samples, " total samples. ",
                                                                                                                               num_samples_total_fraction, " results of 'total fraction'."),
                                    num_samples_crit_excursion_calc < 10 & num_excursions_all > 0 ~ paste0("Insufficient data: ", num_excursions_all,
                                                                                                           " excursion of criteria with ",
                                                                                                           num_samples, " total samples. ",
                                                                                                           num_samples_total_fraction, " results of 'total fraction'."),
                                    num_samples_crit_excursion_calc < 10 & num_excursions_all == 0 ~ paste0("Insufficient data: ", num_excursions_all,
                                                                                                            " excursion of criteria with ",
                                                                                                            num_samples, " total samples. ",
                                                                                                            num_samples_total_fraction, " results of 'total fraction'."),
                                    num_samples_crit_excursion_calc >= 10 &num_excursions_all < critical_excursions~  paste0("Attaining: ", num_excursions_all, " excursions is less than ",
                                                                                                                             critical_excursions, " needed to list- ",
                                                                                                                             num_samples, " total samples. ",
                                                                                                                             num_samples_total_fraction, " results of 'total fraction'.") )) %>%
      mutate(IR_category = factor(IR_category, levels=c("3D", "3", "3B", "2", "5" ), ordered=TRUE))
    
    Results_tox_Ammonia_categories <- join_prev_assessments(Results_tox_Ammonia_categories, AU_type = AU_type)
             
  
             return(Results_tox_Ammonia_categories)
               
  }
  
  
  AL_tox_Ammonia_WS <- ammonia_cat_fun(Results_censored = Results_censored, AU_type = "WS" )
  AL_tox_Ammonia_other <- ammonia_cat_fun(Results_censored = Results_censored,AU_type = "other" ) %>%
    mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code))
  
  WS_AU_rollup <- AL_tox_Ammonia_WS %>%
    select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin, Char_Name, IR_category, Rationale) %>%
    ungroup() %>%
    group_by(AU_ID, Char_Name, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
    summarise(IR_category_AU = max(IR_category),
              Rationale_AU = str_c(MLocID, ": ", Rationale, collapse =  " ~ " ) ) %>%
    mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code))
  WS_AU_rollup <- join_prev_assessments(WS_AU_rollup, AU_type = 'other')
    
  AL_ammonia_list <- list(data = Results_censored,
                          AL_tox_Ammonia_WS =AL_tox_Ammonia_WS,
                          AL_tox_Ammonia_other = AL_tox_Ammonia_other,
                          AL_tox_Ammonia_WS_rollup = WS_AU_rollup)
  return(AL_ammonia_list)
  
  
}