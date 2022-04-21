library(tidyverse)
library(openxlsx)

# to ryan

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

AU_all <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Draft List/Public Comment Edits/AU_all_rollup.xlsx",
                    sheet = "AU_all")


AU_actions <- read.xlsx('Rollups/Final rollup/TMDL_4A_LU_2022.04.07.xlsx') %>%
  select(AU_ID, Pollu_ID,wqstd_code, period, IR_Category_RM,  Status.Comment, action_ID,TMDL )

AU_HUC12 <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Draft List/Public Comment Edits/AU_ID_HUC12.xlsx") %>%
  select(AU_ID, HUC12) %>%
  filter(AU_ID != '99') %>%
  mutate(HUC12 =case_when(is.na(HUC12) ~ str_sub(AU_ID, start = 7, end = 19),
                          TRUE ~ as.character(HUC12)),
         HUC8 = str_sub(HUC12, start = 1, end = 8),
         UID =substrRight(AU_ID, 6) ) %>%
  distinct(AU_ID, .keep_all = TRUE) %>%
  select(-AU_ID)


AU_all_actions <- AU_all %>%
  mutate(UID =substrRight(AU_ID, 6) ) %>%
  left_join(AU_HUC12) %>%
  left_join(AU_actions) %>%
  select(-UID) %>%
  mutate(HUC8 = case_when(is.na(HUC8) ~ str_sub(AU_ID, start = 10, end = 17),
                          TRUE ~ HUC8))


TMDL_priorities <- read.xlsx('Rollups/Final rollup/IR2022TMDLPrioritiespubliccomment.xlsx') %>%
  select(AU_ID, Pollu_ID,wqstd_code, period, TMDL_Priority, TMDL_Project)


AU_all_actions_priority <- AU_all_actions %>%
  left_join(TMDL_priorities)
  
to_print <- AU_all_actions_priority[,c(1:3, 20, 21, 4:19, 22:27)]


dup_test <- to_print %>%
  group_by(AU_ID, Pollu_ID, wqstd_code, period, DO_Class) %>%
  filter(n() > 1)


write.xlsx(to_print, file = 'Rollups/Final rollup/2022_IR_assessments.xlsx')
