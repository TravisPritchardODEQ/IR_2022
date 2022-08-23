library(tidyverse)
library(openxlsx)

AU_all <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Final List/AU_all_rollup.xlsx")



map_display <- AU_all %>%
  mutate(pollutant_strd = case_when(!is.na(period) ~ paste0(Pollutant, "- ", period),
                                    wqstd_code == 15 ~  paste0(Pollutant, "- Aquatic Life Toxics"),
                                    wqstd_code == 16 ~  paste0(Pollutant, "- Human Health Toxics"),
                                    TRUE ~ Pollutant
  )) %>%
  group_by(AU_ID) %>%
  summarise(AU_status = case_when(any(str_detect(AU_parameter_category, '5') | str_detect(AU_parameter_category, '4'))~ 'Impaired',
                                  any(str_detect(AU_parameter_category, '2')) ~ "Attaining",
                                  all(str_detect(AU_parameter_category, '3')) ~ "Insufficient Data",
                                  TRUE ~ "ERROR"
  ),
  year_last_assessed = max(year_assessed, na.rm = TRUE),
  Year_listed = min(Year_listed,  na.rm = TRUE),
  Cat_5_count = length(pollutant_strd[AU_parameter_category == '5']),
  Cat_4_count = length(pollutant_strd[str_detect(AU_parameter_category, '4')]),
  Impaired_count = Cat_5_count + Cat_4_count,
  Impaired_parameters = str_flatten(pollutant_strd[!is.na(AU_parameter_category) & (str_detect(AU_parameter_category, '5') | str_detect(AU_parameter_category, '4'))], ", "),
  Cat_5_parameters =  str_flatten(pollutant_strd[!is.na(AU_parameter_category) & (str_detect(AU_parameter_category, '5') )], ", "),
  Cat_4_parameters =  str_flatten(pollutant_strd[!is.na(AU_parameter_category) & str_detect(AU_parameter_category, '4')], ", "),                                                                        
  Cat_2_count = length(pollutant_strd[AU_parameter_category == '2']),
  Attaining_parameters = str_flatten(pollutant_strd[!is.na(AU_parameter_category) & AU_parameter_category == '2'], ", "),
  Cat_3_count = length(pollutant_strd[AU_parameter_category == '3']),
  Cat_3B_count = length(pollutant_strd[AU_parameter_category == '3B']),
  Cat_3D_count = length(pollutant_strd[AU_parameter_category == '3D']),
  Cat_3_count_total = sum(Cat_3_count, Cat_3B_count, Cat_3D_count),
  Insuffcient_parameters = str_flatten(pollutant_strd[!is.na(AU_parameter_category) & str_detect(AU_parameter_category, '3')], ", ")
  )


write.xlsx(map_display,"C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Final List/map_display_v2.xlsx" )
