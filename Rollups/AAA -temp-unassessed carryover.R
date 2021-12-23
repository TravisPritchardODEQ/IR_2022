assessed_Pollu_ID <- c(unique(AU_all_parameter$Pollu_ID))



load('Rollups/rollup helper/AU_prev_cat.Rdata')





unassessed_prev_AU_cat <- AU_prev_cat %>%
  filter(!Pollu_ID %in%assessed_Pollu_ID ) 

save(unassessed_prev_AU_cat, file = 'Rollups/unassessed_prev_AU_cat.Rdata')

unassessed <- data.frame(
  stringsAsFactors = FALSE,
  Pollu_ID = as.character(unassessed_prev_AU_cat$Pollu_ID),
  wqstd_code = as.character(unassessed_prev_AU_cat$wqstd_code),
  AU_ID = NA_character_,
  AU_GNIS_Name = NA_character_,
  MLocID = NA_character_,
  IR_category= NA_character_,
  Rationale = NA_character_,
  GNIS_previous_IR_impairement = NA_character_,
  Delist = NA_character_
) %>%
  distinct() %>%
  GNIS_rollup() %>%
  filter(!is.na(AU_ID)) %>%
  filter( !(GNIS_IR_category == 'Unassessed' & GNIS_previous_IR_impairement == "Not previously listed")) %>%
  join_pollu_assess()

  
  
  AU_prev_cat %>%
  filter(!Pollu_ID %in%assessed_Pollu_ID ) %>%
  mutate(Delist = NA_character_) %>%
  GNIS_rollup()





Pollu_IDs <- unique(unassessed_prev_AU_cat$Pollu_ID)
wqstd_codes <- unique(unassessed_prev_AU_cat$wqstd_code)

subset_prev_cat <- odeqIRtools::WS_GNIS_previous_listings %>%
  filter(Pollu_ID %in% Pollu_IDs,
         wqstd_code %in% wqstd_codes) %>%
  select(-Char_Name, -period) %>%
  mutate(GNIS_IR_category = NA_character_)


subset_prev_AU_cat <- odeqIRtools::AU_previous_categories %>%
  filter(Pollu_ID %in% Pollu_IDs,
         wqstd_code %in% wqstd_codes,
         grepl("WS", AU_ID)) %>%
  select(-Char_Name, -period) %>%
  mutate(AU_previous_IR_category = str_remove(AU_previous_IR_category, 'Category '))

test <- subset_prev_cat %>%
  full_join(subset_prev_AU_cat) %>%
  mutate(GNIS_IR_category = case_when(is.na(GNIS_IR_category) ~ "Unassessed",
                                      TRUE ~as.character(GNIS_IR_category)),
         GNIS_previous_IR_impairement = case_when(is.na(GNIS_previous_IR_impairement) ~ "Not previously listed",
                                                  TRUE ~ GNIS_previous_IR_impairement)) %>%
  mutate(GNIS_IR_category = factor(GNIS_IR_category, levels=c("Unassessed",'3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE)) %>%
  separate(AU_GNIS, c(NA, "AU_GNIS_Name"),sep = ";") %>%
  #arrange(AU_ID, AU_GNIS_Name) %>%
  ungroup() %>%
  mutate(GNIS_final_IR_category = case_when(GNIS_IR_category == "Unassessed" ~ as.character(GNIS_previous_IR_impairement),
                                            GNIS_previous_IR_impairement == 'Not previously listed' ~ as.character(GNIS_IR_category),
                                            GNIS_IR_category == '2' ~ '2',
                                            GNIS_IR_category == '5' ~ '5',
                                            GNIS_IR_category == '4A' ~ '4A',
                                            grepl('3', GNIS_IR_category) &GNIS_previous_IR_impairement == 'Not previously listed' ~ as.character(GNIS_IR_category),
                                            grepl('3', GNIS_IR_category) &GNIS_previous_IR_impairement != 'Not previously listed' ~ as.character(GNIS_previous_IR_impairement),
  )) %>%
  mutate(GNIS_final_IR_category = str_remove(GNIS_final_IR_category, 'Category ')) %>%
  mutate(GNIS_final_IR_category = factor(GNIS_final_IR_category, levels=c("Unassessed", '3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C' ), ordered=TRUE)) %>%
  group_by(AU_ID, Pollu_ID, wqstd_code) %>%
  mutate(AU_final_status = case_when(#GNIS_IR_category == "Unassessed" & GNIS_previous_IR_impairement == 'Not previously listed' ~ AU_previous_IR_category,
    all(GNIS_final_IR_category == '2') ~ "2",
    any(GNIS_IR_category == "Unassessed") ~ as.character(AU_previous_IR_category),
    any(GNIS_IR_category == '5') & !any(AU_previous_IR_category == '4A') ~ '5',
    any(GNIS_IR_category %in% c('5', '4A')) & any(AU_previous_IR_category == '4A') ~ '4A',
    any(GNIS_IR_category == '5') & is.na(AU_previous_IR_category) ~ '5',
    all(GNIS_IR_category == '2') ~ '2',
    all(GNIS_IR_category %in% c('2', '3', '3B', '3D', "3C")) ~ as.character(max(GNIS_IR_category)),
    TRUE ~ "Error"
  )) %>%
  ungroup() %>%
  mutate(GNIS_remove_impairment = case_when(GNIS_final_IR_category == 2 & GNIS_previous_IR_impairement %in% c("Category 5", "Category 4A") ~ "Yes",
                                            TRUE ~ "No"),
         AU_delist = case_when(AU_final_status == 2 & AU_previous_IR_category %in% c('5', '4A') ~ "Yes",
                               TRUE ~ "No"
         ))%>%
  ungroup() %>%
  arrange(AU_ID, AU_GNIS_Name) 
