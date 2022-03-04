library(tidyverse)
library(openxlsx)



# Setup -----------------------------------------------------------------------------------------------------------

color_pal <- c(#"#001219",
               "#005f73",
               "#0a9396",
               "#94d2bd",
               "#e9d8a6",
               "#ee9b00",
               "#ca6702",
               "#bb3e03",
               "#ae2012",
               "#9b2226")


# Benuse impairemensts graph --------------------------------------------------------------------------------------




AU_BU <- read.xlsx("//deqHQ1/WQASSESSMENT/2022IRFiles/Draft List/Rollup outputs/Actual Final Final Final/AU_all_rollup.xlsx",
                           sheet = "AU_BU")


BU_impaired_summary <- AU_BU %>%
  filter(str_detect( AU_final_status, "[54]")) %>%
  group_by(AU_ID, ben_use) %>%
  filter(row_number() == 1) %>%
  group_by( ben_use) %>%
  summarise(count = n())

ggplot(data = BU_impaired_summary, aes(x = ben_use, y = count)) +
  geom_col(fill = color_pal[[2]])+
  theme_bw()+
  scale_y_continuous(expand = c(0, 0))+
  labs(title = "Beneficial Use Impairments", x = NULL,
       y = "",
       subtitle = "Number of Assessment Units impaired for each beneficial use") + 
  scale_x_discrete(breaks=unique(BU_impaired_summary$ben_use), 
                   labels=c(BU_impaired_summary$ben_use[[1]], 
                            BU_impaired_summary$ben_use[[2]], 
                            BU_impaired_summary$ben_use[[3]], 
                            BU_impaired_summary$ben_use[[4]], 
                            BU_impaired_summary$ben_use[[5]], 
                            "Private Domestic\nWater Supply", 
                            "Public Domestic\nWater Supply", 
                            "Water Contact\nRecreation")) 

ggsave(filename = "Stats/public_comment_presentation/benuse_impairement.png", 
       width = 9.48, height = 6.19, units = c("in"))


# Impairement counts ----------------------------------------------------------------------------------------------

AU_all_rollup <- read.xlsx("//deqHQ1/WQASSESSMENT/2022IRFiles/Draft List/Rollup outputs/Actual Final Final Final/AU_all_rollup.xlsx",
                           sheet = "AU_all")

AU_impaired <- AU_all_rollup %>%
  filter(str_detect( AU_final_status, "[54]")) %>%
  group_by(Pollutant) %>%
  summarise(count = n_distinct(AU_ID)) %>%
  arrange(desc(count))

# reorder 
AU_impaired$Pollutant <- reorder(AU_impaired$Pollutant, -AU_impaired$count)
  
ggplot(data = head(AU_impaired, 4), aes(x = Pollutant, y = count)) +
  geom_col(fill = color_pal[[2]])+
  theme_bw()+
  scale_y_continuous(expand = c(0, 0))+
  labs(title = "Impairments", x = NULL,
       y = "Assessment Units (count)",
       subtitle = "Number of assessment units impaired for top 4 parameters")+
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12))

ggsave(filename = "Stats/public_comment_presentation/top4_impairements.png", 
       width = 9.48, height = 6.19, units = c("in"))


# Attain vs Impaired ----------------------------------------------------------------------------------------------

priority_pollutants <- c('Temperature',
                         'Dissolved Oxygen',
                         'E. coli',
                         'pH')

select_metals <- c("Copper  ")


AU_param_status <- AU_all_rollup %>%
  mutate(status = case_when(str_detect( AU_final_status, "[54]") ~ "Impaired",
                            AU_final_status == 2 ~ "Attaining",
                            str_detect( AU_final_status, "[3]") ~ "Insuffcient data",
                            TRUE ~ "ERROR")) %>%
  filter(status != "ERROR") %>%
  group_by(Pollutant, status) %>%
  summarise(count = n_distinct(AU_ID)) %>%
  arrange(desc(Pollutant))

prioritiy <- AU_param_status %>%
  filter(Pollutant %in% priority_pollutants)
  




ggplot(data = prioritiy, aes(x = Pollutant, y = count, fill = status)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values=color_pal) +
  theme_bw()+
  scale_y_continuous(expand = c(0, 0))+
  labs(title = "Assessment Unit Status", x = NULL,
       y = "Assessment Units",
       subtitle = "Count of assessment unit status by select parameter")


ggsave(filename = "Stats/public_comment_presentation/Assessment Units Status.png", 
       width = 9.48, height = 6.19, units = c("in"))


metals <-  AU_param_status %>%
  filter(Pollutant %in% select_metals)



# Attains Group ---------------------------------------------------------------------------------------------------


color_pal <- c(#"#001219",
  "#005f73",
  "#0a9396",
  "#94d2bd",
  "#e9d8a6",
  "#ee9b00",
  "#ca6702",
  "#bb3e03",
  "#ae2012",
  "#9b2226")

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")



Pollutants <- DBI::dbReadTable(con, 'LU_Pollutant') %>%
  mutate(Pollu_ID = as.character(Pollu_ID)) %>%
  select(-SSMA_TimeStamp) %>%
  mutate(Pollutant_DEQ.WQS = trimws(Pollutant_DEQ.WQS, which = "right")) %>%
  select(Pollu_ID,Pollutant_DEQ.WQS, Attains_PolluName,Attains_Group ) %>%
  mutate(Attains_Group = case_when(Attains_Group == "TOXIC ORGANICS\r\n" ~ "TOXIC ORGANICS",
                                   Attains_Group == "TOXIC INORGANICS\r\n" ~ "TOXIC INORGANICS",
                                   Attains_Group %in% c("HARDNESS BASED METALS","MERCURY", "METALS") ~ "METALS",
                                   Attains_Group == "PESTICIDES\r\n" ~ "PESTICIDES",
                                   Attains_Group == 'CAUSE UNKNOWN - IMPAIRED BIOTA' ~ 'BIOCRITERIA',
                                   TRUE ~ Attains_Group)) %>%
  select(Pollu_ID, Attains_Group)


DBI::dbDisconnect(con)

AU_all_rollup <- read.xlsx("//deqHQ1/WQASSESSMENT/2022IRFiles/Draft List/Rollup outputs/Actual Final Final Final/AU_all_rollup.xlsx",
                           sheet = "AU_all")

AU_param_status_Attains <- AU_all_rollup %>%
  left_join(Pollutants)%>%
  mutate(Attains_Group = str_to_title(Attains_Group, locale = "en"),
         Attains_Group = case_when(Attains_Group == "Ph" ~ 'pH',
                                   TRUE ~ Attains_Group)) %>%
  mutate(status = case_when(str_detect( AU_final_status, "[54]") ~ "Impaired",
                            AU_final_status == 2 ~ "Attaining",
                            str_detect( AU_final_status, "[3]") ~ "Insuffcient data",
                            TRUE ~ "ERROR")) %>%
  filter(status != "ERROR") %>%
  group_by(Attains_Group, status) %>%
  summarise(count = n_distinct(AU_ID)) %>%
  arrange(desc(Attains_Group)) %>%
  filter(Attains_Group %in% str_to_title(c('TEMPERATURE',
                                           'DISSOLVED OXYGEN',
                                           "PH",
                                           "PATHOGENS",
                                           "BIOCRITERIA",
                                           "METALS",
                                           "PESTICIDES",
                                           "TOXIC ORGANICS",
                                           "TOXIC INORGANICS"), locale = "en")) %>%
  filter(status!= 'Insuffcient data') %>%
  mutate(Attains_Group = str_replace(Attains_Group, ' ', '\n'))






ggplot(data = AU_param_status_Attains, aes(x = factor(Attains_Group,
                                                      levels = str_to_title(c('TEMPERATURE',
                                                                              'DISSOLVED\nOXYGEN',
                                                                              "PH",
                                                                              "PATHOGENS",
                                                                              "BIOCRITERIA",
                                                                              "METALS",
                                                                              "PESTICIDES",
                                                                              "TOXIC\nORGANICS",
                                                                              "TOXIC\nINORGANICS"), locale = "en")), 
                                           y = count, fill = status)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values=color_pal) +
  theme_bw()+
  scale_y_continuous(expand = c(0, 0))+
  labs(title = "Parameter Category Status", x = NULL,
       y = "Parameter Assessments",
       subtitle = "Count of Parameter Assessment statuts by parameter group")+
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12))



ggsave(filename = "Stats/public_comment_presentation/Attains group status.png", 
       width = 10.4, height = 5.2, units = c("in"))

