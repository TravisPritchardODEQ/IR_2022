library(tidyverse)
library(sf)
library(arcgisbinding)
arc.check_product()
library(openxlsx)


# AU to AU_UseCode ------------------------------------------------------------------------------------------------


## Get AU use codes ------------------------------------------------------------------------------------------------

url <- 'https://services.arcgis.com/uUvqNMGPm7axC2dD/arcgis/rest/services/IR_2022_Draft_Final/FeatureServer/'
watershed_num <- 7
waterbody_num <- 6
river_num <- 5




riv_coast <- arc.data2sf(arc.select(arc.open(paste0(url, river_num))))%>%
  as.data.frame() %>%
  select(AU_ID,AU_Name,AU_Description, AU_UseCode, AU_LenMiles, AU_AreaAcr )



watershed <- arc.data2sf(arc.select(arc.open(paste0(url, watershed_num)))) %>% 
  as.data.frame() %>%
  select(AU_ID,AU_Name,AU_Description, AU_UseCode, AU_LenMiles, AU_AreaAcr )

waterbodies <- arc.data2sf(arc.select(arc.open(paste0(url, waterbody_num)))) %>%
  as.data.frame() %>%
  select(AU_ID,AU_Name,AU_Description, AU_UseCode, AU_LenMile, AU_AreaAcre ) %>%
  rename(AU_LenMiles = AU_LenMile,
         AU_AreaAcr = AU_AreaAcre)

AU_to_ben_use0 <- bind_rows(riv_coast, watershed, waterbodies) %>%
  group_by(AU_ID) %>%
  filter(row_number() == 1)


new_AUs <- read.xlsx('Rollups/rollup helper/New_AU_BUcode_LAM.xlsx') %>%
  mutate(AU_UseCode = as.character(paste0("0", AU_UseCode)))

AU_to_ben_use <- AU_to_ben_use0 %>%
  filter(!AU_ID %in% new_AUs$AU_ID) %>%
  bind_rows(new_AUs)

save(AU_to_ben_use, file = 'Rollups/rollup helper/AU_to_ben_use.Rdata')

