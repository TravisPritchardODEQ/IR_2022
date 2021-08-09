source("parameters/Tox_AL/fun_toxAL_data.R")
# source("parameters/Tox_AL/fun_ToxAL_analysis.R")
# source("parameters/Tox_AL/fun_ToxAL_pentachlorophenol_data.R")
# source("parameters/Tox_AL/fun_ToxAL_pentachlorophenol_analysis.R")
# source("parameters/Tox_AL/fun_ToxAL_copper_data.R")
# source("parameters/Tox_AL/fun_ToxAL_hardnessmetals_data.R")
# source("parameters/Tox_AL/fun_ToxAL_Ammonia.R")



# Non calculated standards ------------------------------------------------

# Fetch data for analysing the parameters with non-calculated standards
Tox_AL_Censored_data <- tox_AL_data("IR_Dev")

# Run the analysis
Tox_AL_IR_categories <- TOX_AL_analysis(Tox_AL_Censored_data)