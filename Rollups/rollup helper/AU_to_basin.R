library(tidyverse)
library(sf)
library(openxlsx)



# Fetch data from Geodb -------------------------------------------------------------------------------------------

# I created a new geodatabase that spatially joined the AU tables with OWRD basin polygons


dsn <- 'E:/ArcGIS/Projects/AUs/AUs.gdb'

riv_coast <- st_read(dsn,
                     'AssessmentUnits_OR_Rivers_Co1')%>%
  as.data.frame() %>%
  select(AU_ID,AU_Name,AU_Description, basin_name,  HUC12,  AU_LenMiles, AU_AreaAcr )

watershed <- st_read(dsn,
                     'AssessmentUnit_OR_Watershed_')%>%
  as.data.frame() %>%
  select(AU_ID,AU_Name,AU_Description, basin_name,  HUC12,  AU_LenMiles, AU_AreaAcr )

waterbodies <- st_read(dsn,
                       'AU_waterbodies_basin')%>%
  as.data.frame() %>%
  select(AU_ID,AU_Name,AU_Description, basin_name,  HUC12,  AU_LenMile, AU_AreaAcre ) %>%
  rename(AU_LenMiles = AU_LenMile,
         AU_AreaAcr = AU_AreaAcre)

AUs <- bind_rows(riv_coast, watershed, waterbodies) %>%
  group_by(AU_ID) %>%
  filter(row_number() == 1)


# Write to IR db --------------------------------------------------------------------------------------------------
# Write to IR database, so we don't have to do that again
database <- 'IR_Dev'

con <- DBI::dbConnect(odbc::odbc(), database)

dbWriteTable(con, "AU", AUs)



# Write table -----------------------------------------------------------------------------------------------------

write.xlsx(AUs, file = 'AUs.xlsx')
