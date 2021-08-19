## L.Merrick 8/5/2021 
#function to assess continuous data using the ten-ten 

cont_data <- Results_import_cont
ten_ten <- function(cont_data){
cont_pH_ten_ten <- cont_data%>%
                    mutate(pH_violation = ifelse(Result_Numeric < pH_Min | Result_Numeric > pH_Max, 1, 0 ),
                           pH_violation_high = ifelse(Result_Numeric > pH_Max, 1, 0 ),
                           pH_violation_low = ifelse(Result_Numeric < pH_Min, 1, 0 )) %>%
  group_by(MLocID,Result_Date) %>%
  summarise(num_Samples = n(),
            num_violation = sum(pH_violation),
            num_violation_high = sum(pH_violation_high),
            num_violation_low = sum(pH_violation_low),
            pH_low_crit = min(pH_Min),
            pH_high_crit = max(pH_Max),
            pH_code = first(pH_code))  %>%
  mutate(critical_excursions = binomial_excursions(num_Samples,type = "Conventionals"),
         daily_ten =  case_when(
           num_violation >= critical_excursions ~ 1,
           num_violation < critical_excursions ~ 0)) %>% 
  group_by(MLocID) %>% 
  summarise(num_days = n(),
            total_excursions = sum(daily_ten)) %>% 
  mutate(critical_excursions_day = binomial_excursions(num_days,type = "Conventionals"),
         IR_category_TT =  case_when(num_days < 5 & total_excursions >= 2 ~ 'Cat3B',
                                     num_days < 5 & total_excursions < 2 ~ 'Cat3',
                                     num_days >= 5 & total_excursions >= critical_excursions_day ~ 'Cat5',
                                     num_days >= 5 & total_excursions < critical_excursions_day ~ 'Cat2',
                                     TRUE ~ 'ERROR')) }

