library(sf)
library(openxlsx)
library(tidyverse)

# # Previous listings
# 
# dsn <- 'C:/Users/tpritch/Documents/2018-2020 IR geodatabase/WQ_Assessment_2018_20_FINAL.gdb'
# 
# AU_prev_cat_riv <-  st_read(dsn,
#                         'By_Parameter_Rivers_Coast') %>%
#   as.data.frame() %>%
#   select(AU_ID, Pollu_ID, WQstd_code, Period,IR_category,Rationale, year_assessed, Year_listed, Assessed_in_2018)
# 
# AU_prev_cat_WS <-  st_read(dsn,
#                             'By_Parameter_Watershed') %>%
#   as.data.frame() %>%
#   select(AU_ID, Pollu_ID, WQstd_code, Period,IR_category,Rationale, year_assessed, Year_listed, Assessed_in_2018)
# 
# AU_prev_cat_waterbody <-  st_read(dsn,
#                             'By_Parameter_Waterbodies') %>%
#   as.data.frame() %>%
#   select(AU_ID, Pollu_ID, WQstd_code, Period,IR_category,Rationale, year_assessed, Year_listed, Assessed_in_2018)
# 
# 
# AU_prev_cat <- bind_rows(AU_prev_cat_riv, AU_prev_cat_WS,
#                          AU_prev_cat_waterbody) %>%
#   distinct()  %>%
#   rename(period = Period,
#                      wqstd_code  = WQstd_code) %>%
#   mutate(Rationale = case_when(year_assessed == '2018' & !is.na(Rationale) ~ paste("2018:", Rationale),
#                                TRUE ~ Rationale)) %>%
#   mutate(period = case_when(period == 'Spawning' ~ "spawn", 
#                           period == 'Year Round' ~ 'year_round',
#                           TRUE ~ period)) 

AU_prev_cat <- read.xlsx("C:/Users/tpritch/Desktop/Previous IR categories.xlsx",
          sheet = "Previous AU categoires") %>%
  select(AU_ID, Pollu_ID, wqstd_code, Period,IR_category,Rationale, year_assessed, Year_listed, Assessed_in_2018)%>%
  distinct()  %>%
  rename(period = Period) %>%
  mutate(Rationale = case_when(year_assessed == '2018' & !is.na(Rationale) ~ paste("2018:", Rationale),
                               TRUE ~ Rationale)) %>%
  mutate(period = case_when(period == 'Spawning' ~ "spawn", 
                            period == 'Year Round' ~ 'year_round',
                            TRUE ~ period)) 

save(AU_prev_cat, file='Rollups/rollup helper/AU_prev_cat.Rdata')
