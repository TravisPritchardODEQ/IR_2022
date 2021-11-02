


temp_yr_station <- read.xlsx('Parameters/Outputs/temperature- data.xlsx',
                             sheet = "Temperature Data")

#tempertuare
stations <- temp_yr_station %>%
  filter(!grepl("WS", AU_ID)) %>%
  select(AU_ID, MLocID, Pollu_ID, wqstd_code, Spawn_type) %>%
  distinct()
  
temp_spawn <- stations %>%
  filter(Spawn_type == "Spawn") %>%
  mutate(period = 'spawn') %>%
  select(-Spawn_type)

temp_yr <- stations %>%
  mutate(period = 'year_round') %>%
  select(-Spawn_type)


AU_mloc_function <- function(df){
  stations <- df %>%
    filter(!grepl("WS", AU_ID)) %>%
    select(AU_ID, MLocID, Pollu_ID, wqstd_code) %>%
    distinct()
  
}

## Bacteria --------------------------------------------------------------------------------------------------------


### Freshwater ----------------------------------------------------------------------------------------------------


bacteria_fresh_mlocs <- read.xlsx('Rollups/Rollup Assessment/bacteria freshwater contact.xlsx',
                               sheet = "Fresh Bacteria Data") %>%
  AU_mloc_function()
  

### Coastal Contact -------------------------------------------------------------------------------------------------

bacteria_coast_AU <- read.xlsx('Rollups/Rollup Assessment/bacteria coast contact.xlsx',
                               sheet = "Coast Bacteria Data_Other")  %>%
  AU_mloc_function()


## chl -------------------------------------------------------------------------------------------------------------

chl_AU <- read.xlsx("Rollups/Rollup Assessment/chl-a.xlsx",
                    sheet = "Chl-a Raw Data")  %>%
  AU_mloc_function()



##  pH ------------------------------------------------------------------------------------------------------------

ph_AU <- read.xlsx('Rollups/Rollup Assessment/pH.xlsx',
                   sheet = "pH other AU data" )%>%
  AU_mloc_function()


## Tox_AL ----------------------------------------------------------------------------------------------------------


### others ------------------------------------------------------------------------------------------------------


tox_AL_other_AU <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                             sheet = "tox_AL_data" ) %>%
  AU_mloc_function()


### hardness metals -------------------------------------------------------------------------------------------------

tox_AL_hard_AU <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                            sheet = 'tox_AL_hard_data') %>%
  AU_mloc_function()



### penta -----------------------------------------------------------------------------------------------------------

tox_AL_penta_AU <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                             sheet = 'tox_AL_penta_data') %>%
  AU_mloc_function()



### Ammonia --------------------------------------------------------------------------------------------------------


tox_AL_ammonia_AU <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                               sheet = 'tox_AL_Ammonia_data') %>%
  AU_mloc_function()



### Aluminum --------------------------------------------------------------------------------------------------------

tox_AL_aluminum_AU <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                sheet = 'tox_AL_Aluminum_data')  %>%
  AU_mloc_function()



## Tox HH ----------------------------------------------------------------------------------------------------------

tox_HH_AU <- read.xlsx('Rollups/Rollup Assessment/Tox_HH.xlsx',
                       sheet = "HH Toxt Data"  ) %>%
  AU_mloc_function()



## Turbidity ---------------------------------------------------------------------------------------------------------
turbidity_AU <- read.xlsx('Rollups/Rollup Assessment/turbidity.xlsx',
                          sheet = "Turb Data")%>%
  AU_mloc_function()



## DO --------------------------------------------------------------------------------------------------------------

### year round ------------------------------------------------------------------------------------------------------


DO_yr_cont <- read.xlsx('Rollups/Rollup Assessment/DO Year Round.xlsx',
                        sheet = "Yr Rnd Cont Data" ) %>%
  filter(!grepl("WS", AU_ID)) %>%
  select(AU_ID, MLocID, Pollu_ID, wqstd_code, DO_Class) %>%
  distinct()


DO_yr_inst <- read.xlsx('Rollups/Rollup Assessment/DO Year Round.xlsx',
                        sheet = "Yr Rnd Instant Data" ) %>%
  filter(!grepl("WS", AU_ID)) %>%
  select(AU_ID, MLocID, Pollu_ID, wqstd_code, DO_Class) %>%
  distinct()


do_yr_stations <- bind_rows(DO_yr_cont, DO_yr_inst) %>%
  mutate(period = 'year_round') %>%
  distinct()

### Spawn -------------------------------------------------------------------------------------------------------




DO_sp_cont <- read.xlsx('Rollups/Rollup Assessment/DO Spawn.xlsx',
                        sheet = "Spawn Cont Data" ) %>%
  filter(!grepl("WS", AU_ID)) %>%
  select(AU_ID, MLocID, Pollu_ID, wqstd_code, DO_Class, in_spawn) %>%
  distinct()
DO_sp_inst <- read.xlsx('Rollups/Rollup Assessment/DO Spawn.xlsx',
                        sheet = "Spawn Instant Data")%>%
  filter(!grepl("WS", AU_ID)) %>%
  select(AU_ID, MLocID, Pollu_ID, wqstd_code, DO_Class, in_spawn) %>%
  distinct()

DO_spawn <- bind_rows(DO_sp_cont,DO_sp_inst )

stations <- DO_spawn %>%
  filter(!grepl("WS", AU_ID)) %>%
  select(AU_ID, MLocID, Pollu_ID, wqstd_code, DO_Class, in_spawn) %>%
  distinct()

DO_spawn_stations <- stations %>%
  filter(in_spawn == 1) %>%
  mutate(period = 'spawn') %>%
  select(-in_spawn)




# Put it together -------------------------------------------------------------------------------------------------

all__other_stations <- bind_rows(do_yr_stations,
                              DO_spawn_stations,
                              temp_spawn,
                              temp_yr,
                              bacteria_fresh_mlocs,
                              bacteria_coast_AU,
                              chl_AU,
                              ph_AU,
                              tox_AL_aluminum_AU,
                              tox_AL_ammonia_AU,
                              tox_AL_hard_AU,
                              tox_AL_other_AU,
                              tox_AL_penta_AU,
                              tox_HH_AU,
                              turbidity_AU
                              ) 

  
  
all_other_stations <- All_stations %>%
    group_by(AU_ID, Pollu_ID, wqstd_code,period, DO_Class ) %>%
    summarise(stations = str_c(MLocID, sep = "; ", collapse = "; "))


all_other_stations <- all_other_stations %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))

save(all_other_stations, file = 'Rollups/all_other_stations.Rdata')
