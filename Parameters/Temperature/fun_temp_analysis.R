

library(lubridate)
library(runner)
library(odeqIRtools)



fun_temp_analysis <- function(df, write_excel = TRUE){

# Testing ---------------------------------------------------------------------------------------------------------

# df <- Results_censored_temp
# write_excel = TRUE




# Preliminary data prep ------------------------------------------------------------------------------------------------

#calculate in/out critical period
#calculate in/out spawn period
#calculate prelim (non-air temp exlcusion) criteria violations
print("Begin initial temp analysis")

temp_analysis <- df %>%
  filter(!FishCode %in% c('10','11','22','23')) %>%
  mutate(SampleStartDate = ymd(SampleStartDate),
         # Add columns for Critcal period start and end date
         Crit_period_start = mdy(paste0("7/1/",year(SampleStartDate))),
         Cirt_period_end = mdy(paste0("9/30/",year(SampleStartDate))),
         # Append spawn start and end dates with year
         Start_spawn = ifelse(!is.na(SpawnStart), paste0(SpawnStart,"/",year(SampleStartDate)), NA ) ,
         End_spawn = ifelse(!is.na(SpawnEnd), paste0(SpawnEnd,"/",year(SampleStartDate)), NA ),
         # Make spwnmn start and end date date format
         Start_spawn = mdy(Start_spawn),
         End_spawn = mdy(End_spawn),
         # If Spawn dates span a calendar year, account for year change in spawn end date
         End_spawn = if_else(End_spawn < Start_spawn & SampleStartDate >= End_spawn, 
                             End_spawn + lubridate::years(1), # add a year if in spawn period carrying to next year
                             End_spawn),
         Start_spawn = if_else(End_spawn < Start_spawn & SampleStartDate <= End_spawn, Start_spawn - lubridate::years(1), # subtract a year if in spawn period carrying from previous year
                               Start_spawn),
         SampleStartDate = ymd(SampleStartDate), 
         # Flag for results in critical period
         In_crit_period = ifelse(SampleStartDate >=Crit_period_start & SampleStartDate <= Cirt_period_end, 1, 0 ),
         # Print if result is in spawn or out of spawn
         Spawn_type = ifelse((SampleStartDate >= Start_spawn & SampleStartDate <= End_spawn & !is.na(Start_spawn)),  "Spawn", "Not_Spawn"),
         # Flag if result violates standard,  use 13 for during spawn dates, else use criteria
         year_round_Violation = ifelse(Result_cen > Temp_Criteria, 1, 0),
         # Flag for is violation was in spawn period
         Spawn_Violation = ifelse(Spawn_type == "Spawn" & Result_cen > 13, 1, 0 )
  ) %>%
  arrange(SampleStartDate, SampleStartTime) %>%
  filter(!is.na(AU_ID))


# Air temp exclusion ----------------------------------------------------------------------------------------------


print("Begin air temp exclusion analysis")
temp_air_exclusion0 <- air_temp_exclusion(temp_analysis, date_col =  'SampleStartDate') 

temp_air_exclusion <- temp_air_exclusion0 %>%
  mutate(year_round_excursion = case_when(above_exclusion_7d == 'Yes' & year_round_Violation == 1 ~ 0,
                                          TRUE ~ year_round_Violation),
         spawn_excursion = case_when(above_exclusion_7d == 'Yes' & Spawn_Violation == 1  ~ 0,
                                          TRUE ~ Spawn_Violation))


# Excursions ---------------------------------------------------------------------------------------------


# Watershed Unit --------------------------------------------------------------------------------------------------


# watershed unit 3 year excursion rollup
  # Grouped by mloc
ws_3_year <- temp_air_exclusion %>%
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  dplyr::mutate(d = runner(x = data.frame(SampleStartDate  = SampleStartDate,
                                          year_round_excursion = year_round_excursion,
                                          spawn_excursion = spawn_excursion),
                           k = "3 years",
                           lag = 0,
                           idx = SampleStartDate,
                           f = function(x) list(x))) %>%
  dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                 dplyr::summarise(excursions_3yr = sum(year_round_excursion),
                                                  excursions_spawn_3yr = sum(spawn_excursion), 
                                                  samples_crit_period = sum(In_crit_period),
                                                  samples_spawn = sum(Spawn_type == "Spawn")) 
  )) %>%
  tidyr::unnest_wider(d)

# other unit 3 year excursion rollup
# Grouped by au_id

other_3_year <- temp_air_exclusion %>%
  filter(str_detect(AU_ID, "WS", negate = TRUE)) %>%
  group_by(AU_ID,  Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  dplyr::mutate(d = runner(x = data.frame(SampleStartDate  = SampleStartDate,
                                          year_round_excursion = year_round_excursion,
                                          spawn_excursion = spawn_excursion),
                           k = "3 years",
                           lag = 0,
                           idx = SampleStartDate,
                           f = function(x) list(x))) %>%
  dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                 dplyr::summarise(excursions_3yr = sum(year_round_excursion),
                                                  excursions_spawn_3yr = sum(spawn_excursion), 
                                                  samples_crit_period = sum(In_crit_period),
                                                  samples_spawn = sum(Spawn_type == "Spawn")) 
  )) %>%
  tidyr::unnest_wider(d)



# Year Round categorization ---------------------------------------------------------------------------------------


# Watershed Units -------------------------------------------------------------------------------------------------
print('begin Year Round wastershed unit categorization')

crit_period_check <- ws_3_year %>%
  mutate(year =  year(SampleStartDate)) %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin, year) %>%
  summarise(num_crit = sum(In_crit_period)) %>%
  ungroup() %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise(distinct_years_sufficient_crit_period = n_distinct(year[num_crit > .8 * 92 ]))



temp_IR_categories_WS <- ws_3_year %>%
  left_join(crit_period_check) %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise( Temp_Criteria = first(Temp_Criteria),
             data_period_start = min(SampleStartDate),
             data_period_end = max(SampleStartDate),
             total_valid_excursions = sum(year_round_excursion),
             total_air_exclusions = sum(year_round_Violation) -sum(year_round_excursion) ,
             max_3yr_excursions = max(excursions_3yr),
             max_3yr_results_in_crit_period = max(samples_crit_period),
             distinct_years = n_distinct(year(SampleStartDate)),
             distinct_years_sufficient_crit_period = max(distinct_years_sufficient_crit_period),
             total_results = n()
             ) %>%
  mutate(period = "year_round",
         IR_category = case_when(max_3yr_excursions >= 2 ~ "5",
                   max_3yr_excursions == 1 ~ "3B",
                   max_3yr_excursions == 0 & distinct_years_sufficient_crit_period < 1 ~ "3",
                   TRUE ~ '2'),
         Rationale = case_when(max_3yr_excursions >= 2 ~ paste0(MLocID, ": ","Impaired: ", total_valid_excursions, 
                                                                " valid excursions of ", Temp_Criteria, 
                                                                "° criteria. ", total_air_exclusions, 
                                                                " excursions marked invald due to air temp exclusion rule- ",
                                                                total_results, " total results"),
                               max_3yr_excursions == 1 ~ paste0(MLocID, ": Insufficient data:", total_valid_excursions, 
                                                                " valid excursions of ", Temp_Criteria, 
                                                                "° criteria. ", total_air_exclusions, 
                                                                " excursions marked invald due to air temp exclusion rule- ",
                                                                total_results, " total results"),
                               max_3yr_excursions == 0 & distinct_years_sufficient_crit_period < 1 ~ paste0(MLocID, ": Insufficient data: insufficient data collected during critical warm period- ",
                                                                                                      total_results, " total results"),
                               TRUE ~ paste0(MLocID, ": Attaining: No 7DADM excursions- ",
                                                                total_results, " total results"))) %>%
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE))

temp_IR_categories_WS <- join_prev_assessments(temp_IR_categories_WS, AU_type = "WS")

WS_AU_rollup <- temp_IR_categories_WS %>%
  ungroup() %>%
  group_by(AU_ID, Pollu_ID, wqstd_code,  OWRD_Basin, period) %>%
  summarise(IR_category_AU = max(IR_category),
            Rationale_AU = str_c(Rationale,collapse =  " ~ " ) ) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))

WS_AU_rollup <- join_prev_assessments(WS_AU_rollup, AU_type = "Other")
# Other unit year round -------------------------------------------------------------------------------------------


print("Begin Year Round Other AU categorization")

crit_period_check <- other_3_year %>%
  mutate(year =  year(SampleStartDate)) %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin, year) %>%
  summarise(num_crit = sum(In_crit_period)) %>%
  ungroup() %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise(distinct_years_sufficient_crit_period = n_distinct(year[num_crit > .8 * 92 ]))



temp_IR_categories_other <- other_3_year %>%
  left_join(crit_period_check) %>%
  group_by(AU_ID,  Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise( Temp_Criteria = first(Temp_Criteria),
             data_period_start = min(SampleStartDate),
             data_period_end = max(SampleStartDate),
             total_valid_excursions = sum(year_round_excursion),
             total_air_exclusions = sum(year_round_Violation) -sum(year_round_excursion) ,
             max_3yr_excursions = max(excursions_3yr),
             max_3yr_results_in_crit_period = max(samples_crit_period),
             distinct_years = n_distinct(year(SampleStartDate)),
             distinct_years_sufficient_crit_period = max(distinct_years_sufficient_crit_period),
             total_results = n()
           
  ) %>%
  mutate(period = "year_round",
         IR_category =  case_when(max_3yr_excursions >= 2 ~ "5",
                                  max_3yr_excursions == 1 ~ "3B",
                                  max_3yr_excursions == 0 & distinct_years_sufficient_crit_period < 1 ~ "3",
                                  max_3yr_excursions == 1 ~ "3B",
                                  TRUE ~ '2'),
         Rationale = case_when(max_3yr_excursions >= 2 ~ paste0("Impaired: ", total_valid_excursions, 
                                                                " valid excursions of criteria. ", total_air_exclusions, 
                                                                " excursions marked invald due to air temp exclusion rule"),
                               max_3yr_excursions == 1 ~ paste0("Insufficient data:", total_valid_excursions, 
                                                                " valid excursions of criteria. ", total_air_exclusions, 
                                                                " excursions marked invald due to air temp exclusion rule"),
                               max_3yr_excursions == 0 & distinct_years_sufficient_crit_period < 1~ paste0("Insufficient data: insufficient data collected during critical warm period"),
                               TRUE ~ 'Attaining: No 7DADM excursions'))%>%
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE)) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))

temp_IR_categories_other <- join_prev_assessments(temp_IR_categories_other, AU_type = "Other")


# Spawning --------------------------------------------------------------------------------------------------------


# Watershed units -------------------------------------------------------------------------------------------------

print("Begin Spawning watershed unit categorization")

crit_period_check <- ws_3_year %>%
  filter(Spawn_type == "Spawn") %>%
  mutate(spawn_length = as.double(difftime(End_spawn,Start_spawn,unit="days"))) %>%
  mutate(year =  year(SampleStartDate)) %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code, Spawn_type,spawn_length, OWRD_Basin, year) %>%
  summarise(num_spawn = sum(Spawn_type == "Spawn")) %>%
  ungroup() %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise(distinct_years_sufficient_spawn_period = n_distinct(year[num_spawn > .8 * min(spawn_length)]))




temp_IR_categories_WS_spawn <- ws_3_year %>%
  filter(Spawn_type == "Spawn") %>%
  mutate(spawn_length = as.double(difftime(End_spawn,Start_spawn,unit="days"))) %>%
  left_join(crit_period_check) %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise( Temp_Criteria = 13,
             data_period_start = min(SampleStartDate),
             data_period_end = max(SampleStartDate),
             total_valid_excursions = sum(spawn_excursion),
             total_air_exclusions = sum(Spawn_Violation) -sum(spawn_excursion) ,
             max_3yr_excursions = max(excursions_spawn_3yr),
             max_3yr_results_in_spawn_period = max(samples_spawn),
             distinct_years = n_distinct(year(SampleStartDate)),
             spawn_period_length = first(spawn_length),
             distinct_years_sufficient_spawn_period = max(distinct_years_sufficient_spawn_period),
             total_results = n()
  ) %>%
  mutate(period = "spawn",
         IR_category = case_when(max_3yr_excursions >= 2 ~ "5",
                                 max_3yr_excursions == 1 ~ "3B",
                                 max_3yr_excursions == 0 & distinct_years_sufficient_spawn_period < 1 ~ "3",
                                 TRUE ~ '2'),
         Rationale = case_when(max_3yr_excursions >= 2 ~ paste0(MLocID, ": ", "Impaired: ", total_valid_excursions, 
                                                                " valid excursions of ", Temp_Criteria, 
                                                                "° criteria. ", total_air_exclusions, 
                                                                " excursions marked invald due to air temp exclusion rule- ",
                                                                total_results, " total results"),
                               max_3yr_excursions == 1 ~ paste0(MLocID, ": Insufficient data:", total_valid_excursions, 
                                                                " valid excursions of ", Temp_Criteria, 
                                                                "° criteria. ", total_air_exclusions, 
                                                                " excursions marked invald due to air temp exclusion rule- ",
                                                                total_results, " total results"),
                               max_3yr_excursions == 0 & distinct_years_sufficient_spawn_period < 1 ~ paste0(MLocID, ": Insufficient data: insufficient data collected during spawn period- ",
                                                                                                 total_results, " total results"),
                               TRUE ~ paste0(MLocID, ": Attaining: No 7DADM excursions- ",
                                             total_results, " total results"))) %>%
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE))

temp_IR_categories_WS_spawn <- join_prev_assessments(temp_IR_categories_WS_spawn, AU_type = "WS")


WS_AU_rollup_spawn <- temp_IR_categories_WS_spawn %>%
  ungroup() %>%
  group_by(AU_ID, Pollu_ID, wqstd_code,  OWRD_Basin, period) %>%
  summarise(IR_category_AU = max(IR_category),
            Rationale_AU = str_c(Rationale,collapse =  " ~ " ) ) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))

WS_AU_rollup_spawn <- join_prev_assessments(WS_AU_rollup_spawn, AU_type = "Other")


# Spawn other -----------------------------------------------------------------------------------------------------

print("Begin spawning other AU categorization")




crit_period_check <- other_3_year %>%
  filter(Spawn_type == "Spawn") %>%
  mutate(spawn_length = as.double(difftime(End_spawn,Start_spawn,unit="days"))) %>%
  mutate(year =  year(SampleStartDate)) %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code, Spawn_type,spawn_length, OWRD_Basin, year) %>%
  summarise(num_spawn = sum(Spawn_type == "Spawn")) %>%
  ungroup() %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise(distinct_years_sufficient_spawn_period = n_distinct(year[num_spawn > .8 * min(spawn_length)]))

temp_IR_categories_other_spawn <- other_3_year %>%
  filter(Spawn_type == "Spawn") %>%
  mutate(spawn_length = as.double(difftime(End_spawn,Start_spawn,unit="days"))) %>%
  left_join(crit_period_check) %>%
  group_by(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise( Temp_Criteria = 13,
             data_period_start = min(SampleStartDate),
             data_period_end = max(SampleStartDate),
             total_valid_excursions = sum(spawn_excursion),
             total_air_exclusions = sum(Spawn_Violation) -sum(spawn_excursion) ,
             max_3yr_excursions = max(excursions_spawn_3yr),
             max_3yr_results_in_spawn_period = max(samples_spawn),
             distinct_years = n_distinct(year(SampleStartDate)),
             spawn_period_length = first(spawn_length),
             distinct_years_sufficient_spawn_period = max(distinct_years_sufficient_spawn_period),
             total_results = n()
  ) %>%
  mutate(period = "spawn",
         IR_category = case_when(max_3yr_excursions >= 2 ~ "5",
                                 max_3yr_excursions == 1 ~ "3B",
                                 max_3yr_excursions == 0 & distinct_years_sufficient_spawn_period < 1 ~ "3",
                                 TRUE ~ '2'),
         Rationale = case_when(max_3yr_excursions >= 2 ~ paste0("Impaired: ", total_valid_excursions, 
                                                                " valid excursions of ", Temp_Criteria, 
                                                                "° criteria. ", total_air_exclusions, 
                                                                " excursions marked invald due to air temp exclusion rule- ",
                                                                total_results, " total results"),
                               max_3yr_excursions == 1 ~ paste0("Insufficient data:", total_valid_excursions, 
                                                                " valid excursions of ", Temp_Criteria, 
                                                                "° criteria. ", total_air_exclusions, 
                                                                " excursions marked invald due to air temp exclusion rule- ",
                                                                total_results, " total results"),
                               max_3yr_excursions == 0 & distinct_years_sufficient_spawn_period < 1 ~ paste0("Insufficient data: insufficient data collected during spawn period- ",
                                                                                                     total_results, " total results"),
                               TRUE ~ paste0("Attaining: No 7DADM excursions- ",
                                             total_results, " total results"))) %>%
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE)) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))

temp_IR_categories_other_spawn <- join_prev_assessments(temp_IR_categories_other_spawn, AU_type = "Other")

if(write_excel){
  
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = "Temperature Data")
  addWorksheet(wb, sheetName = "YrRnd WS station cat")
  addWorksheet(wb, sheetName = "YrRnd WS AU cat")
  addWorksheet(wb, sheetName = "YrRnd Other AU cat")
  addWorksheet(wb, sheetName = "Spawn WS station cat")
  addWorksheet(wb, sheetName = "Spawn WS AU cat")
  addWorksheet(wb, sheetName = "Spawn Other AU cat")
  
  header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
  freezePane(wb, "Temperature Data", firstRow = TRUE) 
  freezePane(wb, "YrRnd WS station cat", firstRow = TRUE)
  freezePane(wb, "YrRnd WS AU cat", firstRow = TRUE)
  freezePane(wb, "YrRnd Other AU cat", firstRow = TRUE)
  freezePane(wb, "Spawn WS station cat", firstRow = TRUE)
  freezePane(wb, "Spawn WS AU cat", firstRow = TRUE)
  freezePane(wb, "Spawn WS AU cat", firstRow = TRUE)
  
  writeData(wb = wb, sheet = "Temperature Data", x = temp_air_exclusion, headerStyle = header_st)
  writeData(wb = wb, sheet = "YrRnd WS station cat", x = temp_IR_categories_WS, headerStyle = header_st)
  writeData(wb = wb, sheet = "YrRnd WS AU cat", x = WS_AU_rollup , headerStyle = header_st)
  writeData(wb = wb, sheet = "YrRnd Other AU cat", x = temp_IR_categories_other, headerStyle = header_st )
  writeData(wb = wb, sheet = "Spawn WS station cat", x = temp_IR_categories_WS_spawn , headerStyle = header_st )
  writeData(wb = wb, sheet = "Spawn WS AU cat", x = WS_AU_rollup_spawn, headerStyle = header_st )
  writeData(wb = wb, sheet = "Spawn Other AU cat", x = temp_IR_categories_other_spawn, headerStyle = header_st )
  
  print("Writing excel doc")
  saveWorkbook(wb, "Parameters/Outputs/temperature.xlsx", overwrite = TRUE) 
  
}


temperature <-list(temperature=as.data.frame(temp_air_exclusion),
                   year_round_ws_station_categorization=as.data.frame(temp_IR_categories_WS),
                   year_round_ws_au_categorization=as.data.frame(WS_AU_rollup),
                   year_round_other_au_categorization=as.data.frame(temp_IR_categories_other),
                   spawn_ws_station_categorization=as.data.frame(temp_IR_categories_WS_spawn),
                   spawn_ws_au_categorization=as.data.frame(WS_AU_rollup_spawn),
                   spawn_other_au_categorization=as.data.frame(temp_IR_categories_other_spawn))

return(temperature)
}