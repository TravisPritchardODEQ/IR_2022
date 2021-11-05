#Needs copper assessment
#needs penta to be slip into WS and other units




source("parameters/Tox_AL/fun_toxAL_data.R")
source("parameters/Tox_AL/fun_AL_Tox_Assessment.R")
source("parameters/Tox_AL/fun_ToxAL_Pentachlorophenol_data.R")
source("parameters/Tox_AL/fun_ToxAL_Pentachlorophenol_assessment.R")
source("parameters/Tox_AL/fun_ToxAL_copper_data.R")
source("parameters/Tox_AL/fun_ToxAL_hardnessmetals.R")
source("parameters/Tox_AL/fun_ToxAL_ammonia.R")
source('Parameters/Tox_AL/toxAL_aluminum.R')



# Non calculated standards ------------------------------------------------

# Fetch data for analysing the parameters with non-calculated standards
Tox_AL_Censored_data <- tox_AL_data("IR_Dev")

# Run the analysis
Tox_AL_categories <- TOX_AL_analysis(Tox_AL_Censored_data)




# Hardness based standards ----------------------------------------------------------------------------------------

Tox_AL_hardness_cat <- Hardness_based_metals("IR_Dev")


# Pentachlorophenol -----------------------------------------------------------------------------------------------------------
tox_AL_penta_data <- Pentachlorophenol_data("IR_Dev")
tox_AL_penta_cat <- TOX_AL_penta_analysis(tox_AL_penta_data)

# Copper data -----------------------------------------------------------------------------------------------------


Copper_data("IR_Dev")



# Ammonia ---------------------------------------------------------------------------------------------------------

tox_AL_Ammonia <- ToxAL_Ammonia("IR_Dev")


# Aluminum --------------------------------------------------------------------------------------------------------

tox_AL_aluminum  <- aluminum_assessment("IR_Dev") 

# Pull data together for export -----------------------------------------------------------------------------------


tox_AL_data                   <- Tox_AL_categories[["data"]]
tox_AL_other_cats             <- Tox_AL_categories[['AL_Tox_other']]
tox_AL_WS_cats                <- Tox_AL_categories[["AL_Tox_WS"]]
tox_AL_WS_rollup              <- Tox_AL_categories[["AL_Tox_WS_rollup"]]


tox_AL_hard_data             <- Tox_AL_hardness_cat[["data"]]
tox_AL_hard_other_cats       <- Tox_AL_hardness_cat[['AL_Tox_Hard_other']]
tox_AL_hard_WS_cats          <- Tox_AL_hardness_cat[["AL_Tox_Hard_WS"]]
tox_AL_hard_WS_rollup        <- Tox_AL_hardness_cat[["AL_Tox_Hard_WS_rollup"]]

tox_AL_penta_data             <- tox_AL_penta_cat[["data"]]
tox_AL_penta_other_cats       <- tox_AL_penta_cat[["AL_tox_Penta_other"]] 
tox_AL_penta_WS_cats          <- tox_AL_penta_cat[["AL_tox_Penta_WS"]]
tox_AL_penta_WS_rollup        <- tox_AL_penta_cat[["AL_tox_Penta_WS_rollup"]]

tox_AL_Ammonia_data          <- tox_AL_Ammonia[["data"]]
tox_AL_Ammonia_other_cats    <- tox_AL_Ammonia[["AL_tox_Ammonia_other"]]
tox_AL_Ammonia_WS_cats       <- tox_AL_Ammonia[["AL_tox_Ammonia_WS"]]
tox_AL_Ammonia_WS_rollup     <- tox_AL_Ammonia[["AL_tox_Ammonia_WS_rollup"]]


tox_AL_Aluminum_data       <-  tox_AL_aluminum[['data']]
tox_AL_Aluminum_other_cats <-  tox_AL_aluminum[['AL_Tox_AL_other']]
tox_AL_Aluminum_WS_cats    <-  tox_AL_aluminum[['AL_Tox_AL_WS']]
tox_AL_Aluminum_WS_rollup  <-  tox_AL_aluminum[['AL_Tox_AL_WS_rollup']]

# Write excel doc -------------------------------------------------------------------------------------------------
library(openxlsx)


wb <- createWorkbook()
addWorksheet(wb, sheetName = "tox_AL_data"            )   
addWorksheet(wb, sheetName = "tox_AL_other_cats"      )   
addWorksheet(wb, sheetName = "tox_AL_WS_cats"         )
addWorksheet(wb, sheetName = "tox_AL_WS_rollup"  )  

addWorksheet(wb, sheetName = "tox_AL_hard_data"       )   
addWorksheet(wb, sheetName = "tox_AL_hard_other_cats" )   
addWorksheet(wb, sheetName = "tox_AL_hard_WS_cats"    ) 
addWorksheet(wb, sheetName = "tox_AL_hard_WS_rollup"  )

addWorksheet(wb, sheetName = 'tox_AL_penta_data'       )
addWorksheet(wb, sheetName = 'tox_AL_penta_other_cats' )
addWorksheet(wb, sheetName = 'tox_AL_penta_WS_cats'    )
addWorksheet(wb, sheetName = 'tox_AL_penta_WS_rollup'  )


addWorksheet(wb, sheetName = "tox_AL_Ammonia_data"      )   
addWorksheet(wb, sheetName = "tox_AL_Ammonia_other_cats"      )   
addWorksheet(wb, sheetName = "tox_AL_Ammonia_WS_cats"      )   
addWorksheet(wb, sheetName = "tox_AL_Ammonia_WS_rollup"  )  

addWorksheet(wb, sheetName =  'tox_AL_Aluminum_data'           )   
addWorksheet(wb, sheetName =  'tox_AL_Aluminum_other_cats'      )   
addWorksheet(wb, sheetName =  'tox_AL_Aluminum_WS_cats'      )   
addWorksheet(wb, sheetName =  'tox_AL_Aluminum_WS_rollup'       )  

header_st <- createStyle(textDecoration = "Bold", border = "Bottom")

freezePane(wb, "tox_AL_data"                , firstRow = TRUE)
freezePane(wb, "tox_AL_other_cats"          , firstRow = TRUE)
freezePane(wb, "tox_AL_WS_cats"             , firstRow = TRUE)
freezePane(wb, "tox_AL_WS_rollup"      ,    firstRow = TRUE)

freezePane(wb, "tox_AL_hard_data"           , firstRow = TRUE)
freezePane(wb, "tox_AL_hard_other_cats"     , firstRow = TRUE)
freezePane(wb, "tox_AL_hard_WS_cats"        , firstRow = TRUE)
freezePane(wb, "tox_AL_hard_WS_rollup"      , firstRow = TRUE)

freezePane(wb,  'tox_AL_penta_data'       ,        firstRow = TRUE)
freezePane(wb,  'tox_AL_penta_other_cats' ,        firstRow = TRUE)
freezePane(wb,  'tox_AL_penta_WS_cats'    , firstRow = TRUE)
freezePane(wb,  'tox_AL_penta_WS_rollup'  , firstRow = TRUE)


freezePane(wb, "tox_AL_Ammonia_data"        , firstRow = TRUE)
freezePane(wb, "tox_AL_Ammonia_other_cats"  , firstRow = TRUE)
freezePane(wb, "tox_AL_Ammonia_WS_cats"     , firstRow = TRUE)
freezePane(wb, "tox_AL_Ammonia_WS_rollup"   , firstRow = TRUE) 

freezePane(wb,  'tox_AL_Aluminum_data'       , firstRow = TRUE)
freezePane(wb,  'tox_AL_Aluminum_other_cats' , firstRow = TRUE)
freezePane(wb,  'tox_AL_Aluminum_WS_cats'    , firstRow = TRUE)
freezePane(wb,  'tox_AL_Aluminum_WS_rollup'  , firstRow = TRUE) 

writeData(wb = wb, sheet = "tox_AL_data"                , x = tox_AL_data, headerStyle = header_st)
writeData(wb = wb, sheet = "tox_AL_other_cats"          , x = tox_AL_other_cats , headerStyle = header_st)
writeData(wb = wb, sheet = "tox_AL_WS_cats"             , x = tox_AL_WS_cats, headerStyle = header_st)
writeData(wb = wb, sheet = "tox_AL_WS_rollup"      , x = tox_AL_WS_rollup, headerStyle = header_st)

writeData(wb = wb, sheet = "tox_AL_hard_data"           , x = tox_AL_hard_data, headerStyle = header_st)
writeData(wb = wb, sheet = "tox_AL_hard_other_cats"     , x = tox_AL_hard_other_cats, headerStyle = header_st)
writeData(wb = wb, sheet = "tox_AL_hard_WS_cats"        , x = tox_AL_hard_WS_cats, headerStyle = header_st)
writeData(wb = wb, sheet = "tox_AL_hard_WS_rollup"      , x = tox_AL_hard_WS_rollup, headerStyle = header_st)

writeData(wb = wb, sheet =   'tox_AL_penta_data'      , x = tox_AL_penta_data      , headerStyle = header_st)
writeData(wb = wb, sheet =   'tox_AL_penta_other_cats', x = tox_AL_penta_other_cats, headerStyle = header_st)
writeData(wb = wb, sheet =   'tox_AL_penta_WS_cats'   , x = tox_AL_penta_WS_cats   , headerStyle = header_st)
writeData(wb = wb, sheet =   'tox_AL_penta_WS_rollup' , x = tox_AL_penta_WS_rollup , headerStyle = header_st)


writeData(wb = wb, sheet = "tox_AL_Ammonia_data"        , x = tox_AL_Ammonia_data, headerStyle = header_st)
writeData(wb = wb, sheet = "tox_AL_Ammonia_other_cats"  , x = tox_AL_Ammonia_other_cats, headerStyle = header_st) 
writeData(wb = wb, sheet = "tox_AL_Ammonia_WS_cats"     , x = tox_AL_Ammonia_WS_cats, headerStyle = header_st)
writeData(wb = wb, sheet = "tox_AL_Ammonia_WS_rollup"   , x = tox_AL_Ammonia_WS_rollup, headerStyle = header_st)

writeData(wb = wb, sheet =  'tox_AL_Aluminum_data'        , x = tox_AL_Aluminum_data, headerStyle = header_st)
writeData(wb = wb, sheet =  'tox_AL_Aluminum_other_cats'  , x = tox_AL_Aluminum_other_cats,    headerStyle = header_st) 
writeData(wb = wb, sheet =  'tox_AL_Aluminum_WS_cats'     , x = tox_AL_Aluminum_WS_cats,   headerStyle = header_st)
writeData(wb = wb, sheet =  'tox_AL_Aluminum_WS_rollup'   , x = tox_AL_Aluminum_WS_rollup,   headerStyle = header_st)

print("Writing excel doc")
saveWorkbook(wb, "Parameters/Outputs/Tox_AL.xlsx", overwrite = TRUE) 
