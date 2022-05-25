library(tidyverse)
library(AWQMSdata)

upstream <- '14158500'
downstream <- '14158740'

sites <- c(upstream, downstream)


AWQMS_temp <- AWQMS_Data(station = sites,
                          stat_base = '7DADM')


temperature <- AWQMS_temp %>%
  select(MLocID, SampleStartDate, SampleMedia, SampleSubmedia,Statistical_Base,  Char_Name, Result_Operator, Result_Numeric, 
         Result_Unit) %>%
  mutate(position = case_when(MLocID %in% upstream ~ "upstream",
                              MLocID %in% downstream ~ "downstream")) %>%
  select(-MLocID) %>%
  pivot_wider(names_from = position, values_from = Result_Numeric) %>%
  mutate(delta = upstream - downstream,
         excursion = case_when(upstream >= 9 & delta > 0.3 ~ TRUE,
                               upstream < 9 & delta > 1 ~ TRUE,
                               TRUE ~ FALSE))

colnames(temperature)[which(names(temperature) == "upstream")] <- upstream
colnames(temperature)[which(names(temperature) == "downstream")] <- downstream

openxlsx::write.xlsx(temperature, file = "C:/Users/tpritch/Desktop/delta T- 14158500-14158740.xlsx")

