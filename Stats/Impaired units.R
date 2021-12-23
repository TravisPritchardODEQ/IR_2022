library(tidyverse)
library(openxlsx)


AU_all_rollup <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup outputs/AU_all_rollup.xlsx",
                           sheet = "AU_all")



impaired_new <- AU_all_rollup %>%
  filter(str_detect( AU_final_status, "[54]") & 
           Year_listed == '2022')

impaired_parameter_summary <- impaired_new %>%
  group_by(Pollutant) %>%
  summarise(num_impairements = sum(str_detect( AU_final_status, "[54]"))) %>%
  arrange(desc(num_impairements))

write.csv(impaired_parameter_summary, file = 'impaired_parameter_summary.csv')


top_5 <- impaired_parameter_summary %>%
  head(n = 5) %>%
  pull(Pollutant) %>%
  wordlist()


top_5_vector <- impaired_parameter_summary %>%
  head(n = 5) %>%
  pull(Pollutant)


total_2022_assessments <- AU_all_rollup %>%
  filter(assessed_2022 == "Yes")

length(unique(total_2022_assessments$AU_ID))


library(sf)


rivcoast_AU <- st_read("//deqHQ1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_Assessment/WQ_2018_20_IntegratedReport_FINAL/WQ_Assessment_2018_20.gdb",
                        layer = "AssessmentUnits_OR_Rivers_Coast") %>%
  as.data.frame() %>%
  select(AU_ID, AU_Name, AU_Description, AU_LenMiles, AU_AreaAcr)

watershed_AU <- st_read("//deqHQ1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_Assessment/WQ_2018_20_IntegratedReport_FINAL/WQ_Assessment_2018_20.gdb",
                       layer = "AssessmentUnit_OR_Watershed_Area") %>%
  as.data.frame() %>%
  select(AU_ID, AU_Name, AU_Description, AU_LenMiles, AU_AreaAcr)

waterbody_AUs <-  st_read("//deqHQ1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_Assessment/WQ_2018_20_IntegratedReport_FINAL/WQ_Assessment_2018_20.gdb",
                                         layer = "AssessmentUnits_OR_Waterbodies") %>%
  as.data.frame() %>%
  select(AU_ID, AU_Name, AU_Description, AU_LenMile, AU_AreaAcre) %>%
  rename(AU_LenMiles = AU_LenMile,
         AU_AreaAcr = AU_AreaAcre)

assessment_units <- bind_rows(rivcoast_AU, watershed_AU, waterbody_AUs) %>%
  distinct()


test <-  assessment_units%>%
  group_by(AU_ID) %>%
  mutate(n = n())

total_assessment_units <- length(unique(assessment_units$AU_ID))
total_assessed_units <- length(unique(AU_all_rollup$AU_ID))
total_assessed_units_22 <- length(unique(total_2022_assessments$AU_ID))
total_assessed_units/total_assessment_units*100


assessed_units <- assessment_units %>%
  filter(AU_ID %in% unique(AU_all_rollup$AU_ID))

map_display <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup outputs/AU_all_rollup.xlsx",
                          sheet = "map_display")

impaired_AUs <- map_display %>%
  filter(AU_status == "Impaired")
  


# 2018/2020 -------------------------------------------------------------------------------------------------------

previous_assessment <- read.xlsx('Stats/Previous IR categories.xlsx',
                               sheet = "Previous AU categoires")

previous_impaired <- previous_assessment %>%
  mutate(Impaired = case_when(str_detect(IR_category, "5") ~ 1,
                              str_detect(IR_category, "4") ~ 1,
                              TRUE ~0)) %>%
  filter(Impaired == 1) %>%
  select(AU_ID) %>%
  distinct()
  
  

  
  

