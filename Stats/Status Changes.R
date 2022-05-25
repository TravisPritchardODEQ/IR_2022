load('IR_20.RData')


color_pal <- c("#001219",
               "#005f73",
               "#0a9396",
               "#94d2bd",
               "#e9d8a6",
               "#ee9b00",
               "#ca6702",
               "#bb3e03",
               "#ae2012",
               "#9b2226")
AU_all <- read.xlsx('C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Draft List/Final List/AU_all_rollup.xlsx')

AU_status_change <- AU_all %>%
  mutate(status_change = case_when(AU_final_status == IR_category20 |
                                     (str_detect(AU_final_status, '3') & str_detect(IR_category20, '3'))~ "No\nChange",
                                   AU_final_status %in% c('5', "4A", "4C", "4B") & IR_category20 %in% c('2') ~ "Attain\nto Impaired",
                                   AU_final_status %in% c("4A", "4C", "4B") & IR_category20 %in% c('5') ~ "TMDL\nDeveloped",
                                   AU_final_status %in% c('2') & IR_category20 %in% c('5', "4A", "4C", "4B") ~ "Impaired\nto Attain",
                                   AU_final_status %in% c('2') & IR_category20 %in% c("3C", "3B", "3D",  "3") ~ "Insufficient\nto Attain",
                                   AU_final_status %in%  c('5', "4A", "4C", "4B") & IR_category20 %in% c("3C", "3B", "3D",  "3") ~ "Insufficient\nto Impaired",
                                   is.na(IR_category20) ~ "New\nAssessment",
                                   TRUE ~ 'Other'
  ))



status_change_summary <- AU_status_change %>%
  group_by(status_change) %>%
  summarise(n = n())

ggplot(status_change_summary, aes(x = status_change, y = n)) +
  geom_col(fill = color_pal[[2]])+
  geom_text(aes(label = n), nudge_y = 600) +
  theme_bw()+
  ylim(0, 30000) +
 # scale_y_continuous(expand = c(0,500))+
  labs(title = "Status Change", x = NULL,
       y = "AU / Parameter Assessments",
       subtitle = "Assessment Unit/ Parameter Assessment Changes from 2018/2020 to 2022",
       x = element_blank()) + 
  theme(axis.text = element_text(size = 16), axis.text.y = element_text(size = 16), 
        axis.title.y = element_text(size = 12), legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))


# old -------------------------------------------------------------------------------------------------------------





IR_20_for_joining <- IR_20 %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code)) %>%
  mutate(DO_Class = case_when(str_detect(Assessment, "Cold Water") ~ "Cold Water",
                              str_detect(Assessment, "Cool Water") ~ "Cool Water",
                              TRUE ~ NA_character_)) %>%
  select(AU_ID, Pollu_ID, wqstd_code,period,DO_Class,IR_category) %>%
  rename(IR_category20 = IR_category)

IR_20_for_joining_no_class <- IR_20_for_joining %>%
  select(-DO_Class) %>%
  rename(IR_category20_noclass = IR_category20)


AU_all <- read.xlsx('C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Draft List/Final List/AU_all_rollup.xlsx')



AU_all_joined <- AU_all %>%
  left_join(IR_20_for_joining) %>%
  left_join(IR_20_for_joining_no_class) %>%
  mutate(IR_category20 = case_when(!is.na(IR_category20) ~ IR_category20,
                                             !is.na(IR_category20_noclass) ~ IR_category20_noclass)) %>%
  distinct(AU_ID, Pollu_ID, wqstd_code, period, IR_category20, .keep_all = TRUE) %>%
  select(-IR_category20_noclass) %>%
  mutate(IR_category20 = str_remove(IR_category20, "Category ")) %>%
  mutate(status_change = case_when(AU_final_status == IR_category20 |
                                     (str_detect(AU_final_status, '3') & str_detect(IR_category20, '3'))~ "No Change",
                                   AU_final_status %in% c('5', "4A", "4C", "4B") & IR_category20 %in% c('2') ~ "Attain to Impaired",
                                   AU_final_status %in% c("4A", "4C", "4B") & IR_category20 %in% c('5') ~ "TMDL Developed",
                                   AU_final_status %in% c('2') & IR_category20 %in% c('5', "4A", "4C", "4B") ~ "Impaired to Attain",
                                   AU_final_status %in% c('2') & IR_category20 %in% c("3C", "3B", "3D",  "3") ~ "Insufficient to Attain",
                                   AU_final_status %in%  c('5', "4A", "4C", "4B") & IR_category20 %in% c("3C", "3B", "3D",  "3") ~ "Insufficient to Impaired",
                                   is.na(IR_category20) ~ "New Assessment",
                                   TRUE ~ 'Other'
                                   ))

write.xlsx(AU_all_joined, file = "AU_All_w_previous.xlsx")


status_change_summary <- AU_all_joined %>%
  group_by(status_change) %>%
  summarise(n = n())

ggplot(status_change_summary, aes(x = status_change, y = n)) +
  geom_bar(stat="identity") +
  theme_bw()+
  scale_y_continuous(expand = c(0, 0))+
  labs(title = "Status Change", x = NULL,
       y = "AU / Parameter Assessments",
       subtitle = "Assessment Unit/ Parameter Assessment Changes from 2018/2020 to 2022") + 
  theme(axis.text = element_text(size = 12), axis.text.y = element_text(size = 12), 
        axis.title.y = element_text(size = 12), legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))