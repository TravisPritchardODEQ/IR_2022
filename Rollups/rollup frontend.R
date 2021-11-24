
#' This file will rollup individual assessments into various final products used for data display and visulalization
#' 
#' Mloc_rollup.R- Rolls us watershed unit monitoring locations. Produces excel document which contains 2 tabs.
#'      - WS_MLocID_param_rollup: an accouting of all the parameter assessments done at each monitoring location
#'      - WS_MlocID_rollup: Each monitoring location as one line with columns for IR categories base don 2022 assessments 
#'
#' GNIS rollup.R rolls up data into GNIS by parameter. This is what will be used to detrmine linework for the watershed 
#' units
#' 
#' Other AU rollup.R rolls up non-watershed unit to an AU level
#' together rollup.R combines watershed and other unit types. Tabs here also include beneficaial use information 
#' 



# Set options -----------------------------------------------------------------------------------------------------

options(scipen=999)
options(dplyr.summarise.inform = FALSE)


source('Rollups/rollup helper/AU_prev_cat.R') 


# Rollup MLOC -----------------------------------------------------------------------------------------------------

source('Rollups/Mloc_rollup.R')


# Rollup GNIS -----------------------------------------------------------------------------------------------------

source('Rollups/GNIS rollup.R')


# Rollup non WS AUs ------------------------------------------------------------------------------------------------------

source('Rollups/Other AU rollup.R')



# AU rollup- combined ---------------------------------------------------------------------------------------------

source('Rollups/together rollup.R')
