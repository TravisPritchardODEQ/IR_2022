library(tidyverse)
library(sf)



# AU to AU_UseCode ------------------------------------------------------------------------------------------------


## Get AU use codes ------------------------------------------------------------------------------------------------

dsn <- '//deqhq1/WQ-Share/2018 Integrated Report_Assessments/GIS/WebMap2020_V4_Final/OR_AUs.gdb'

riv_coast <- st_read(dsn,
                     'AssessmentUnits_OR_Rivers_Coast') %>%
  as.data.frame() %>%
  select(AU_ID,AU_Name,AU_Description, AU_UseCode, AU_LenMiles, AU_AreaAcr )

watershed <- st_read(dsn,
                     'AssessmentUnits_OR_Watershed_Lines') %>%
  as.data.frame() %>%
  select(AU_ID,AU_Name,AU_Description, AU_UseCode, AU_LenMiles, AU_AreaAcr )

waterbodies <- st_read(dsn,
                       'AssessmentUnits_OR_Waterbodies') %>%
  as.data.frame() %>%
  select(AU_ID,AU_Name,AU_Description, AU_UseCode, AU_LenMile, AU_AreaAcre ) %>%
  rename(AU_LenMiles = AU_LenMile,
         AU_AreaAcr = AU_AreaAcre)

AU_to_ben_use <- bind_rows(riv_coast, watershed, waterbodies) %>%
  group_by(AU_ID) %>%
  filter(row_number() == 1)

save(AU_to_ben_use, file = 'Rollups/rollup helper/AU_to_ben_use.Rdata')

