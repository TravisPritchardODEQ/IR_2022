source("Parameters/Bacteria/fun_bacteria_data.R")
source("Parameters/Bacteria/fun_fresh_contact.R")


library(tidyverse)
library(odeqIRtools)

Bacteria_results <- Bacteria_data("IR_Dev")


freshwater_bacteria <- fresh_contact_rec(Bacteria_results, write_excel = TRUE)