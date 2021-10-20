### Assess biocriteria using % taxa loss from the PREDATOR model 
#df= BioCriteria

#BioCriteria_Assement <- function(df){

require(data.table)

# Marine Western Coastal Forest
##### old script ######  
# MWCF_AU_sum = Results_import %>%
#     filter(EcoRegion2 == "MARINE WEST COAST FOREST") %>%
#     group_by(MLocID) %>%
#     summarise(num_Samples = n(),
#               n_over20PTL = sum(Score >= 20),
#               n_15to20PTL = sum(Score >= 15 & Score <= 20),
#               n_9to14PTL = sum(Score >= 9 & Score <= 14),
#               n_less8PTL = sum(Score <= 8)) %>%
#     mutate(IR_Cat = ifelse(num_Samples == 1 & n_over20PTL >=1 | num_Samples >= 2 & n_over20PTL >=1 | num_Samples >= 2 & n_15to20PTL >=1,"Cat5",
#                            ifelse(num_Samples == 1 & n_15to20PTL >=1,"Cat3B",
#                                   ifelse(n_9to14PTL >= 1,"Cat3C",
#                                          ifelse(n_less8PTL >= 1,"Cat2","")))))

#### single Sample MWCF #####
MWCF_AU_SS_WS = Results_import %>%
  filter(EcoRegion2 == "MARINE WEST COAST FOREST" & str_detect(AU_ID, "WS", negate = FALSE))%>%
  group_by(AU_ID,MLocID,AU_GNIS_Name) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples == 1) %>%
  summarise(num_Samples = n(),
            n_over20PTL = sum(Score >= 20),
            n_15to20PTL = sum(Score >= 15 & Score <= 20),
            n_9to14PTL = sum(Score >= 9 & Score <= 14),
            n_less8PTL = sum(Score <= 8)) %>%
  mutate(IR_Cat = case_when(n_over20PTL >=1 ~"Cat5",
                            n_15to20PTL >=1 ~"Cat3B",
                            n_9to14PTL >= 1 ~ "Cat3C",
                            n_less8PTL >= 1 ~ "Cat2",
                            TRUE ~ "ERROR"))

MWCF_AU_SS_RS = Results_import %>%
  filter(EcoRegion2 == "MARINE WEST COAST FOREST" & str_detect(AU_ID, "SR", negate = FALSE)) %>%
  group_by(AU_ID) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples == 1) %>%
  summarise(num_Samples = n(),
            n_over20PTL = sum(Score >= 20),
            n_15to20PTL = sum(Score >= 15 & Score <= 20),
            n_9to14PTL = sum(Score >= 9 & Score <= 14),
            n_less8PTL = sum(Score <= 8)) %>%
  mutate(IR_Cat = case_when(n_over20PTL >=1 ~"Cat5",
                            n_15to20PTL >=1 ~"Cat3B",
                            n_9to14PTL >= 1 ~ "Cat3C",
                            n_less8PTL >= 1 ~ "Cat2",
                            TRUE ~ "ERROR"))

#### multiple Sample at the same station or AU MWCF #####
MWCF_AU_MS_WS = Results_import %>%
  filter(EcoRegion2 == "MARINE WEST COAST FOREST"& str_detect(AU_ID, "WS", negate = FALSE)) %>%
  group_by(AU_ID,MLocID,AU_GNIS_Name) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples >= 2) %>%
  summarise(num_Samples = n(),
            n_over15PTL = sum(Score >= 15),
            n_9to14PTL = sum(Score >= 9 & Score <= 14),
            n_less8PTL = sum(Score <= 8),
            mean_all_samples = mean(Score)) %>%
      mutate(IR_Cat_ave = case_when(mean_all_samples >= 15 ~"Cat5",
                                mean_all_samples >= 9 & mean_all_samples <= 14 ~ "Cat3C",
                                mean_all_samples <= 8 ~ "Cat2",
                                TRUE ~ "ERROR"))

MWCF_AU_MS_SR = Results_import %>%
  filter(EcoRegion2 == "MARINE WEST COAST FOREST"& str_detect(AU_ID, "SR", negate = FALSE)) %>%
  group_by(AU_ID,AU_GNIS_Name) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples >= 2) %>%
  summarise(num_Samples = n(),
            n_over15PTL = sum(Score >= 15),
            n_9to14PTL = sum(Score >= 9 & Score <= 14),
            n_less8PTL = sum(Score <= 8),
            mean_all_samples = mean(Score)) %>%
  mutate(IR_Cat_ave = case_when(mean_all_samples >= 15 ~"Cat5",
                                mean_all_samples >= 9 & mean_all_samples <= 14 ~ "Cat3C",
                                mean_all_samples <= 8 ~ "Cat2",
                                TRUE ~ "ERROR"))

# combine WS and river stream
MWCF_AU_SS_all <- rbind(MWCF_AU_SS_WS,MWCF_AU_SS_RS)

MWCF_AU_MS_all <- rbind(MWCF_AU_MS_WS,MWCF_AU_MS_SR)


# Western Cordillera and Columbia Plateau
#### single Sample WCCP#####
WCCP_AU_SS_WS = Results_import %>%
  filter(EcoRegion2 == "WESTERN CORDILLERA"|EcoRegion2 == "COLD DESERTS") %>% 
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  group_by(AU_ID,MLocID,AU_GNIS_Name) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples == 1) %>%
  summarise(num_Samples = n(),
            n_over27PTL = sum(Score >= 27),
            n_22to26PTL = sum(Score >= 22 & Score <= 26),
            n_8to21PTL = sum(Score >= 8 & Score <= 21),
            n_less8PTL = sum(Score <= 7)) %>%
  mutate(IR_Cat = case_when(n_over27PTL >=1 ~"Cat5",
                            n_22to26PTL >=1 ~"Cat3B",
                            n_8to21PTL >= 1 ~ "Cat3C",
                            n_less8PTL >= 1 ~ "Cat2",
                            TRUE ~ "ERROR"))

WCCP_AU_SS_SR = Results_import %>%
  filter(EcoRegion2 == "WESTERN CORDILLERA"|EcoRegion2 == "COLD DESERTS") %>%
  filter(str_detect(AU_ID, "SR", negate = FALSE)) %>%
  group_by(AU_ID,AU_GNIS_Name) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples == 1) %>%
  summarise(num_Samples = n(),
            n_over27PTL = sum(Score >= 27),
            n_22to26PTL = sum(Score >= 22 & Score <= 26),
            n_8to21PTL = sum(Score >= 8 & Score <= 21),
            n_less8PTL = sum(Score <= 7)) %>%
  mutate(IR_Cat = case_when(n_over27PTL >=1 ~"Cat5",
                            n_22to26PTL >=1 ~"Cat3B",
                            n_8to21PTL >= 1 ~ "Cat3C",
                            n_less8PTL >= 1 ~ "Cat2",
                            TRUE ~ "ERROR"))

#### multiple Sample at the same station WCCP #####

WCCP_AU_MS_WS = Results_import %>%
  filter(EcoRegion2 == "WESTERN CORDILLERA"|EcoRegion2 == "COLD DESERTS") %>%
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  group_by(AU_ID,MLocID,AU_GNIS_Name) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples >= 2) %>%
  summarise(num_Samples = n(),
            n_over22PTL = sum(Score >= 22),
            n_8to21PTL = sum(Score >= 8 & Score <= 21),
            n_less8PTL = sum(Score <= 7),
            mean_all_samples = mean(Score)) %>%
  mutate(IR_Cat_ave = case_when(mean_all_samples >= 22 ~"Cat5",
                                mean_all_samples >= 8 & mean_all_samples <= 21 ~ "Cat3C",
                                mean_all_samples <= 7 ~ "Cat2",
                                TRUE ~ "ERROR"))



WCCP_AU_MS_SR = Results_import %>%
  filter(EcoRegion2 == "WESTERN CORDILLERA"|EcoRegion2 == "COLD DESERTS") %>%
  filter(str_detect(AU_ID, "SR", negate = FALSE)) %>%
  group_by(AU_ID,AU_GNIS_Name) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples >= 2) %>%
  summarise(num_Samples = n(),
            n_over22PTL = sum(Score >= 22),
            n_8to21PTL = sum(Score >= 8 & Score <= 21),
            n_less8PTL = sum(Score <= 7),
            mean_all_samples = mean(Score)) %>%
  mutate(IR_Cat_ave = case_when(mean_all_samples >= 22 ~"Cat5",
                                mean_all_samples >= 8 & mean_all_samples <= 21 ~ "Cat3C",
                                mean_all_samples <= 7 ~ "Cat2",
                                TRUE ~ "ERROR"))

# combine WS and river stream
WCCP_AU_SS_all <- rbind(WCCP_AU_SS_WS,WCCP_AU_SS_SR)

WCCP_AU_MS_all <- rbind(WCCP_AU_MS_WS,WCCP_AU_MS_SR)


# export - for bpj analysis 
write.csv(MWCF_AU_SS_all,"MWCF_AU_SS_all.csv")
write.csv(MWCF_AU_MS_all,"MWCF_AU_MS_all.csv")
write.csv(WCCP_AU_SS_all,"WCCP_AU_SS_all.csv")
write.csv(WCCP_AU_MS_all,"WCCP_AU_MS_all.csv")
write.csv(Results_import,"Bug_raw_scores.csv")
  