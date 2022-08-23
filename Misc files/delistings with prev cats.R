### DAdd previous categories to delistings file
# Jill would like to view previous categories in the delisting file so this is an attempt to provide that data


library(tidyverse)
library(openxlsx)

# Get delistings file ---------------------------------------------------------------------------------------------

delistings <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Website/submittal docs/DEQ_IR2022_Delistings.xlsx",
                        sheet = 2)


# Load previous categories ----------------------------------------------------------------------------------------

load('Rollups/rollup helper/AU_prev_cat.Rdata')


AU_prev_cat <- AU_prev_cat %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code)) %>%
  rename(previous_IR_category = IR_category) %>%
  select(-Rationale, -year_assessed, -Year_listed, -Assessed_in_2018)

# Join ------------------------------------------------------------------------------------------------------------

delistings_join <- delistings %>%
  left_join(AU_prev_cat, by = c("AU_ID", "Pollu_ID", "wqstd_code", "period", "DO_Class"))


failed_joins <- delistings_join %>%
  filter(is.na(previous_IR_category))



# Write excel doc -------------------------------------------------------------------------------------------------
#there are a lot of missing ones. Needs manual additions

write.xlsx(delistings_join, file = "Misc files/delisting_with_prev_cats.xlsx")

