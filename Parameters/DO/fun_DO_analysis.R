




# Testing and setup -----------------------------------------------------------------------------------------------

df <- Results_censored_DO
write_excel <- TRUE



# Year round analysys ---------------------------------------------------------------------------------------------


# Year-Round COntinuous -------------------------------------------------------------------------------------------


# Setup data

# add spawn start and end dates as dates, include indicator if actdate is within spawn
# add critical period start and end dates, include indicator is actdate is within critperiod
Results_spawndates <- df %>%
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
         critstart = mdy(paste0("6/1/",year(SampleStartDate) )),
         critend = mdy(paste0("9/30/",year(SampleStartDate) )),
         is.crit = ifelse(SampleStartDate >= critstart & SampleStartDate <= critend, 1, 0 )) %>%
  filter(!is.null(OWRD_Basin) & DO_code %in% c(2,3,4))


# Summarize available data to get a list of AU's to be analyzed using cont. data
results_cont_summary <- Results_spawndates %>%
  filter(Statistical_Base == "30DADMean") %>%
  group_by(AU_ID, year(SampleStartDate)) %>%
  summarise(tot_30d_metrics = n(),
            crit_30d_periods = sum(is.crit)) %>%
  filter(crit_30d_periods >= 15,
         !is.na(AU_ID)) %>%
  pull(AU_ID) 


# Initial Continuous criteria analysis --------------------------------------------

# This initial analysis is used to see where we need to calculate DO Sat 
# Calculating the 30DADMean DO SAt is computationally expensive
# so we only calculate it at locations where it woudl influnce the
# IR category

# filter down to AUs that are to be evaluated with cont metrics
# Filter down to only 30-D, 7-Mi, and daily minimums
# Flag various violations
continuous_data_analysis <- Results_spawndates %>%
  filter(AU_ID %in% results_cont_summary) %>%
  filter(Statistical_Base %in% c("30DADMean", "7DADMin", "Minimum")) %>%
  mutate(Violation = ifelse(Statistical_Base == "30DADMean" & IRResultNWQSunit < crit_30D, 1, 
                            ifelse(Statistical_Base == "7DADMin" & IRResultNWQSunit < crit_7Mi, 1, 
                                   ifelse(Statistical_Base == "Minimum" & IRResultNWQSunit < crit_Min, 1, 0 )))) 


# Other Units -------------------------------------------------------------------------------------------------



# Run through initial categorization
# This all gets redone in the end
# Where percent saturation would make a difference, set category as "Check percent Sat"
continuous_data_categories_other <- continuous_data_analysis %>%
  filter(str_detect(AU_ID, "WS", negate = TRUE)) %>%
  group_by(AU_ID, DO_Class) %>%
  summarise(Total_violations = sum(Violation),
            Sum_30D_violations = sum(Violation [Statistical_Base == "30DADMean"]),
            Sum_7mi_violations = sum(Violation [Statistical_Base == "7DADMin"]),
            Sum_abs_min_violations = sum(Violation [Statistical_Base == "Minimum"])) %>%
  mutate(IR_category = case_when(DO_Class != "Cold Water" & Sum_30D_violations >= 2 ~ "5",
                                 DO_Class != "Cold Water" & Sum_7mi_violations >= 2 ~ "5",
                                 DO_Class != "Cold Water" & Sum_abs_min_violations >= 2 ~ "5",
                                 DO_Class == "Cold Water" & Sum_7mi_violations >= 2 ~ "5",
                                 DO_Class == "Cold Water" & Sum_abs_min_violations >= 2 ~ "5",
                                 DO_Class == "Cold Water" & Sum_30D_violations >= 2 &
                                   Sum_7mi_violations < 2 & Sum_abs_min_violations < 2 ~ "Check percent Sat",
                                 Sum_30D_violations < 2 & Sum_7mi_violations < 2 & Sum_abs_min_violations < 2 ~ "2",
                                 TRUE ~ "Error" ))
           
           


# Datatable of results that need percent saturation
cont_perc_sat_check_other <- continuous_data_categories_other %>%
  filter(AU_ID %in% unique(subset(continuous_data_categories_other, IR_category == "Check percent Sat" )$AU_ID) )


# Query Database --------------------------------------------------------------------------------------------------

if(nrow(cont_perc_sat_check_other) > 0){
  
  # List of monitoring locations that need OD sat 
  # This list is used for the sql query that follows
  continuous_mon_locs <- unique(cont_perc_sat_check$MLocID)
  
  
  # Get data from database --------------------------------------------------
  
  print("querying the IR database to get data for DO sat calculations ")
  
  # Get DO IR_database to calculate percent sat --------
  
  con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")
  
  DOSatQry <- "SELECT [MLocID], [SampleStartDate],[SampleStartTime],[Statistical_Base],[IRResultNWQSunit] as DO_sat
FROM [IntegratedReport].[dbo].[ResultsRawWater2018]
WHERE   Char_Name = 'Dissolved oxygen saturation' AND 
MLocID in ({continuous_mon_locs*}) AND 
Statistical_Base = 'Mean'"
  
  Dosqry <- glue::glue_sql(DOSatQry, .con = con)
  DO_sat_AWQMS <- DBI::dbGetQuery(con, Dosqry)
  
  
  Doqry <- "SELECT * 
FROM            VW_DO
WHERE        (Statistical_Base = 'Mean') AND MLocID in ({continuous_mon_locs*})"
  
  
  
  Doqry <- glue::glue_sql(Doqry, .con = con)
  
  perc_sat_DO <- DBI::dbGetQuery(con, Doqry)
  
  #Get temperature data from database
  
  tempqry <- "SELECT * 
FROM            VW_Temp_4_DO
WHERE        (Statistical_Base = 'Mean') AND MLocID in ({continuous_mon_locs*})"
  
  tempqry <- glue::glue_sql(tempqry, .con = con)
  
  perc_sat_temp <-  DBI::dbGetQuery(con, tempqry)
  
  # Disconnect from database
  DBI::dbDisconnect(con)
  
  print("Finished database query")
  
  
  # Join --------------------------------------------------------------------
  
  # Pare down table to be used in join
  perc_sat_temp_join <- perc_sat_temp %>%
    select(MLocID, IRResultNWQSunit, SampleStartDate, SampleStartTime, Statistical_Base) %>%
    rename(Temp_res = IRResultNWQSunit)
  
  
  
  perc_sat_DO <- perc_sat_DO %>%
    left_join(DO_sat_AWQMS, by =c('MLocID', 'SampleStartDate','SampleStartTime','Statistical_Base'  ))
  
  # Rename the result to DO_res and join with the temperature
  # Calculate DOsat
  DO_sat <- perc_sat_DO %>%
    rename(DO_res =  IRResultNWQSunit) %>%
    left_join(perc_sat_temp_join, by = c('MLocID', 'SampleStartDate', 'SampleStartTime', 'Statistical_Base')) %>%
    mutate(DO_sat = ifelse(is.na(DO_sat),DOSat_calc(DO_res, Temp_res, ELEV_Ft ), DO_sat),
           ma.DOS.mean30 = "") %>%
    mutate(DO_sat = ifelse(DO_sat > 100, 100, DO_sat ))
  
  # calculate 30-D averages
  
  
  
  #Create list that will be used to get data out of the loop
  monloc_do_list <- list()
  
  #Set loop for each monitoring location
  print("Beginning DO sat Calculations")
  
  for(i in 1:length(unique(DO_sat$MLocID))){
    
    print(paste("Station", i, "of", length(unique(DO_sat$MLocID))))
    
    #Name of station be be used in this loop iteration
    station = unique(DO_sat$MLocID)[i]
    
    #Filter dataset to only look at 1 monitoring location at a time
    daydat_station <- DO_sat %>%
      filter(MLocID == station) %>%
      mutate(startdate30 = as.Date(SampleStartDate) -30) %>%
      arrange(SampleStartDate)
    
    # Begin 30-d moving averages loop
    print("Begin 30 day moving averages" )
    pb <- txtProgressBar(min = 0, max = nrow(daydat_station), style = 3)
    
    for(l in 1:nrow(daydat_station)){
      
      #Beginning of 30 day window
      start30 <- daydat_station$startdate30[l]
      # End of 30 day window
      end30 <- daydat_station$SampleStartDate[l] 
      
      
      # For each row in table, crate a new datatable for taht row plus all
      # Results that are in the 30 day window
      station_30day <- daydat_station %>%
        filter(SampleStartDate <= end30 & SampleStartDate >= start30) 
      
      
      # If there are at least 29 values in the 30 day window
      # Calculate the average DO-Sat
      # Otherwise use NA
      ma.mean30 <- ifelse(length(unique(station_30day$SampleStartDate)) >= 29, mean(station_30day$DO_sat), NA )
      
      
      # Pass the 30-d DO Sat vaule back into the single monitoring location table
      # the l >+ 30 prevents the 29th day being used. 
      daydat_station[l,"ma.DOS.mean30"] <- ifelse(l >= 30, round(ma.mean30, 2), NA)
      setTxtProgressBar(pb, l)
    } #end of 30day loop
    
    # Assign dataset filtered to 1 monitoring location to a list for combining outside of for loop
    monloc_do_list[[i]] <- daydat_station
    
  }
  
  print("Finished DO Sat Calculations")
  
  # Bind rows to get DO_sat averages
  
  DO_sat_avgs <-  bind_rows(monloc_do_list)  
  
  
  # Join DOsat to 30_D metrics -----------------------------------------------
  
  # Add Statistical_Base to the DO Sat table
  # Create Date field to be used for the join
  # The Activity start dates were slighly different causing problems
  # (1/1/1900 vs 1/1/1900 00:00)
  DO_sat_join <- DO_sat_avgs %>%
    mutate(Statistical_Base = "30DADMean",
           Date = as.Date(SampleStartDate)) %>%
    select(MLocID, ma.DOS.mean30, Date,Statistical_Base) 
  
  
  # Join DO Sat back into the original data table and recalculate violations
  yr_round_cont_DO_data_analysis_other <- continuous_data_analysis_other %>%
    mutate(Date = as.Date(SampleStartDate)) %>%
    left_join(DO_sat_join, by = c('MLocID', 'Date', 'Statistical_Base')) %>%
    mutate(Violation = case_when(DO_Class == "Cold Water"& Statistical_Base == "30DADMean" & IRResultNWQSunit < crit_30D & 
                                   (ma.DOS.mean30 < 90 | is.na(ma.DOS.mean30)) ~ 1,
                                 DO_Class != "Cold Water"& Statistical_Base == "30DADMean" & IRResultNWQSunit < crit_30D ~  1,
                                 Statistical_Base == "7DADMin" & IRResultNWQSunit < crit_7Mi ~ 1,
                                 Statistical_Base == "Minimum" & IRResultNWQSunit < crit_Min ~ 1,
                                 TRUE ~ 0))
  
  
  yr_round_cont_DO_data_analysis_other <- yr_round_cont_DO_data_analysis_other %>%
    rename(DO_sat = ma.DOS.mean30)
  
  
} else {
  
  yr_round_cont_DO_data_analysis_other <- continuous_data_analysis %>%
    filter(str_detect(AU_ID, "WS", negate = TRUE)) %>%
    mutate(Date = as.Date(SampleStartDate)) %>%
    mutate(Violation = case_when(DO_Class == "Cold Water"& Statistical_Base == "30DADMean" & IRResultNWQSunit < crit_30D ~ 1,
                                 DO_Class != "Cold Water"& Statistical_Base == "30DADMean" & IRResultNWQSunit < crit_30D ~ 1,
                                 Statistical_Base == "7DADMin" & IRResultNWQSunit < crit_7Mi ~ 1,
                                 Statistical_Base == "Minimum" & IRResultNWQSunit < crit_Min ~ 1,
                                 TRUE ~ 0 ))
  
}


#datatfile = yr_round_cont_DO_data_analysis_other

yr_round_cont_data_categories_other <- yr_round_cont_DO_data_analysis_other %>%
  group_by(AU_ID, GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin, DO_Class) %>%
  summarise(Total_excursions = sum(Violation),
            Sum_30D_excursions = sum(Violation [Statistical_Base == "30DADMean"]),
            Sum_7mi_excursions = sum(Violation [Statistical_Base == "7DADMin"]),
            Sum_abs_min_excursions = sum(Violation [Statistical_Base == "Minimum"])) %>%
  mutate(IR_category = case_when(Sum_30D_excursions >= 2 ~ "5",
                                 Sum_7mi_excursions >= 2 ~ "5",
                                 Sum_abs_min_excursions >= 2 ~ "5",
                                 Sum_30D_excursions < 2 &
                                   Sum_7mi_excursions < 2 &
                                   Sum_abs_min_excursions < 2 ~ "2",
                                 TRUE ~ "ERROR"),
         Rationale = case_when(Sum_30D_excursions >= 2 ~ paste0(Sum_30D_excursions, " valid excursions of 30-D metric"),
                               Sum_7mi_excursions >= 2 ~ paste0(Sum_7mi_excursions, " valid excursions of 7-mi metric. ",
                                                                Sum_30D_excursions, " valid excursions of 30-D metric"),
                               Sum_abs_min_excursions >= 2 ~ paste0(Sum_abs_min_excursions, " excursions of alternate minimum criteria. ",
                                                                    Sum_7mi_excursions, " valid excursions of 7-mi metric. ",
                                                                    Sum_30D_excursions, " valid excursions of 30-D metric"),
                               Sum_30D_excursions < 2 &
                                 Sum_7mi_excursions < 2 &
                                 Sum_abs_min_excursions < 2 ~  paste0("Attaining: ",
                                                                      Sum_30D_excursions, " valid excursions of 30-D metric. ",
                                                                      Sum_7mi_excursions, " valid excursions of 7-mi metric. ",
                                                                      Sum_abs_min_excursions, " excursions of alternate minimum criteria. "
                                                                      ),
                               TRUE ~ "ERROR"))
           
           



