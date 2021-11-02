



# # testing
# df <- read.xlsx('Rollups/Rollup Assessment/chl-a.xlsx',
#                                 sheet = 'Other AU categorization') 
# periods =   FALSE






AU_rollup_other <- function(df, periods = FALSE, DO = FALSE){

Pollu_IDs <- unique(df$Pollu_ID)
wqstd_codes <- unique(df$wqstd_code)

if(DO == TRUE){
  
  selection <- c('AU_ID', 'Pollu_ID','DO_Class', 'wqstd_code', 'period', 'IR_category', 'Rationale', 'recordID')
   per <- unique(df$period)
  
  subset_prev_AU_cat <- odeqIRtools::AU_previous_categories %>%
    filter(!(AU_previous_IR_category %in% c("-","Unassessed")),
           !is.na(AU_previous_IR_category)) %>%
    mutate(period = ifelse(period == 'Spawn', "spawn", period )) %>%
    filter(Pollu_ID %in% Pollu_IDs,
           wqstd_code %in% wqstd_codes,
           period %in% per,
           !grepl("WS", AU_ID)) %>%
    separate(Char_Name, c("Char_Name", "DO_Class"), sep = " - ") %>%
    select(-Char_Name) %>%
    mutate(AU_previous_IR_category = str_remove(AU_previous_IR_category, 'Category ')) %>%
    rename(AU_previous_IR_category_class = AU_previous_IR_category)
  
  subset_prev_AU_cat_no_class <- subset_prev_AU_cat %>%
    select(-DO_Class) %>%
    rename(AU_previous_IR_category_no_class = AU_previous_IR_category_class)
  
  
  
  
  AU_rollup <- df%>%
    mutate(period = ifelse(period == 'Spawn', "spawn", period )) %>%
    select(selection) %>% 
    left_join(subset_prev_AU_cat) %>%
    full_join(subset_prev_AU_cat_no_class, by = c("AU_ID",  "Pollu_ID", "wqstd_code", "period")) %>%
    mutate(AU_previous_IR_category = case_when(!is.na(AU_previous_IR_category_class ) ~ AU_previous_IR_category_class ,
                                               !is.na(AU_previous_IR_category_no_class) & is.na(AU_previous_IR_category_class ) ~ AU_previous_IR_category_no_class,
                                               is.na(AU_previous_IR_category_class) & is.na(AU_previous_IR_category_no_class) ~ NA_character_,
                                               TRUE ~ "ERROR")) %>%
    select(-AU_previous_IR_category_no_class, -AU_previous_IR_category_class) %>%
    mutate(IR_category = factor(IR_category, levels=c("Unassessed", '3D',"3", "3B", "2", "5", '4B', '4A' ), ordered=TRUE),
           AU_previous_IR_category =  factor(AU_previous_IR_category, levels=c("Unassessed", '3D',"3", "3B", "2", "5", '4B', '4A' ), ordered=TRUE),
           AU_final_status = case_when(is.na(IR_category) ~ AU_previous_IR_category,
                                       IR_category %in% c("Unassessed", '3D',"3", "3B") ~ pmax(IR_category, AU_previous_IR_category, na.rm= TRUE),
                                       IR_category == 5 & AU_previous_IR_category == '4A' ~ AU_previous_IR_category,
                                       IR_category %in% c( "2", "5", '4A') ~ IR_category
           ) ) %>%
    mutate(assessed_2022 = case_when(is.na(IR_category) & !is.na(AU_previous_IR_category) ~ "No",
                                     !is.na(IR_category) ~ "Yes",
                                     TRUE ~ "ERROR"),
           AU_delist = case_when(AU_final_status == 2 & AU_previous_IR_category %in% c('5', '4A') ~ "Yes",
                                 TRUE ~ "No")) %>%
    rename(`2022_IR_category` = IR_category)
  
  return(AU_rollup)
  
} else if(periods == TRUE){
  
  per <- unique(df$period)
  
  selection <- c('AU_ID', 'Pollu_ID', 'wqstd_code', 'period', 'IR_category', 'Rationale', 'recordID')
  
  
  
  subset_prev_AU_cat <- odeqIRtools::AU_previous_categories %>%
    filter(!(AU_previous_IR_category %in% c("-","Unassessed")),
           !is.na(AU_previous_IR_category)) %>%
    filter(Pollu_ID %in% Pollu_IDs,
           wqstd_code %in% wqstd_codes,
           !grepl("WS", AU_ID),
           period %in% per) %>%
    select(-Char_Name) %>%
    mutate(AU_previous_IR_category = str_remove(AU_previous_IR_category, 'Category '))
  
  
} else if (periods == FALSE){
  
  selection <- c('AU_ID', 'Pollu_ID', 'wqstd_code', 'IR_category', 'Rationale', 'recordID')
  
  subset_prev_AU_cat <- odeqIRtools::AU_previous_categories %>%
    filter(!(AU_previous_IR_category %in% c("-","Unassessed")),
           !is.na(AU_previous_IR_category)) %>%
    filter(Pollu_ID %in% Pollu_IDs,
           wqstd_code %in% wqstd_codes,
           !grepl("WS", AU_ID)) %>%
    select(-Char_Name, -period) %>%
    mutate(AU_previous_IR_category = str_remove(AU_previous_IR_category, 'Category '))
}
  




AU_rollup <- df%>%
  select(selection) %>%
  full_join(subset_prev_AU_cat) %>%
  mutate(IR_category = factor(IR_category, levels=c("Unassessed", '3D',"3", "3B", "2", "5", '4B', '4A' ), ordered=TRUE),
         AU_previous_IR_category =  factor(AU_previous_IR_category, levels=c("Unassessed", '3D',"3", "3B", "2", "5", '4B', '4A' ), ordered=TRUE),
         AU_final_status = case_when(is.na(IR_category) ~ AU_previous_IR_category,
                                     IR_category %in% c("Unassessed", '3D',"3", "3B") ~ pmax(IR_category, AU_previous_IR_category, na.rm= TRUE),
                                     IR_category == 5 & AU_previous_IR_category == '4A' ~ AU_previous_IR_category,
                                     IR_category %in% c( "2", "5", '4A') ~ IR_category
                                     ) ) %>%
  mutate(assessed_2022 = case_when(is.na(IR_category) & !is.na(AU_previous_IR_category) ~ "No",
                                   !is.na(IR_category) ~ "Yes",
                                   TRUE ~ "ERROR"),
         AU_delist = case_when(AU_final_status == 2 & AU_previous_IR_category %in% c('5', '4A') ~ "Yes",
                               TRUE ~ "No")) %>%
  rename(`2022_IR_category` = IR_category)

return(AU_rollup)

}