source("Parameters/Bacteria/fun_bacteria_data.R")
source("Parameters/Bacteria/fun_fresh_contact.R")
source("Parameters/Bacteria/fun_coast_contact.R")
source("Parameters/Bacteria/fun_shell_harvest.R")


library(tidyverse)
library(odeqIRtools)

Bacteria_results <- Bacteria_data("IR_Dev")


freshwater_bacteria <- fresh_contact_rec(Bacteria_results, write_excel = TRUE)

coastal_contact <- coast_contact(Bacteria_results, write_excel = TRUE)

shellfish_harvesting <- Shell_Harvest(Bacteria_results, write_excel = TRUE)