#2020 assesssments
#From online database



# Lookup tables ----------------------------------------------------------------------------------------------------



con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")



LU_Pollutant <- DBI::dbGetQuery(con, glue::glue_sql("SELECT distinct RTRIM([Pollutant_DEQ WQS]) as Pollutant,
[Pollu_ID]
  FROM [IntegratedReport].[dbo].[LU_Pollutant]", .con = con)) 
   


database_download_2020 <- read.csv("C:/Users/tpritch/Downloads/Oregon 2020 Integrated Report Filtered Download (4).csv")

#add wqstd_code
database_download_2020_1 <- database_download_2020 %>%
  mutate(wqstd_code = case_when(str_detect(Assessment, " Aquatic Life Criteria") ~ 15,
                                str_detect(Assessment, " Human Health Criteria") ~ 16,
                                str_detect(Assessment, "Temperature") ~ 12,
                                Assessment == 'Total Dissolved gas' ~ 13,
                                Assessment == 'Chlorophyll-a' ~ 17,
                                str_detect(Assessment, "Dissolved Oxygen") ~ 3,
                                Assessment %in% c('E. coli', 'Fecal Coliform', 'Enterococci') ~ 1,
                                Assessment == 'pH' ~ 10,
                                Assessment == "Turbidity"  ~ 9,
                                Assessment == "BioCriteria"  ~ 5,
                                Assessment == "Shellfish Toxins"  ~ 8,
                                Assessment == "Ocean Biological Integrity"  ~ 5,
                                Assessment == "Methylmercury- Year Round"~ 16,
                                Assessment ==  "Chlorine- Year Round"~ 15,  
                                Assessment == "Endrin Aldehyde- Year Round" ~ 16,
                                Assessment == "Dieldrin- Category 4A" ~ 16,
                                ))


# add polluID -----------------------------------------------------------------------------------------------------

database_download_2020_2 <- database_download_2020_1 %>%
  mutate(Pollutant = case_when(str_detect(Assessment, " Aquatic Life Criteria") ~ str_remove(Assessment, "- Aquatic Life Criteria"),
                               str_detect(Assessment, " Human Health Criteria") ~ str_remove(Assessment, "- Human Health Criteria"),
                               str_detect(Assessment, "Temperature") ~ 'Temperature',
                               str_detect(Assessment, "Dissolved Oxygen") ~ 'Dissolved Oxygen',
                               str_detect(Assessment, "Methylmercury") ~ "Methylmercury",
                               str_detect(Assessment, "Chlorine") ~ "Chlorine",
                               str_detect(Assessment, "Endrin Aldehyde") ~ "Endrin Aldehyde",
                               str_detect(Assessment, "Dieldrin") ~ "Dieldrin",
                               str_detect(Assessment, "Harmful Algal Blooms") ~ "Harmful Algal Blooms",
                               TRUE ~ Assessment
         
                               )) %>%
  left_join(LU_Pollutant) %>%
  mutate(Pollu_ID = case_when(Pollutant == 'Dinitrotoluene 2,4' ~ 73,
                              Pollutant == 'Flow Modification' ~ 170,
                              Pollutant == 'Habitat Modification' ~ 171,
                              Pollutant == 'Nitrosodiethylamine, N' ~ 116,
                              TRUE ~ Pollu_ID)) %>%
  filter(!is.na(Pollu_ID))


# Add period ------------------------------------------------------------------------------------------------------
database_download_2020_3 <- database_download_2020_2 %>%
  mutate(period = case_when(Assessment == "Temperature- Year Round"  ~ 'year_round',
                            Assessment == "Temperature- Spawning"  ~ 'spawn',
                            Assessment == "Dissolved Oxygen- Spawning"  ~ 'spawn',
                            Assessment == "Dissolved Oxygen- Year Round"   ~ 'year_round'))

IR_20 <- database_download_2020_3

save(IR_20, file = 'IR_20.RData')




                                           