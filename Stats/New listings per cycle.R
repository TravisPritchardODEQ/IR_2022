library(tidyverse)
library(flextable)
library(openxlsx)

AU_all_rollup <- read.xlsx("//deqHQ1/WQASSESSMENT/2022IRFiles/Draft List/Rollup outputs/Actual Final Final Final/AU_all_rollup.xlsx",
                           sheet = "AU_all")

list_by_year <- AU_all_rollup %>%
  group_by(Year_listed) %>%
  summarise(count = n())

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

ggplot(data = list_by_year, aes(x = Year_listed, y = count)) +
  geom_col(fill = color_pal[[2]])+
  scale_x_continuous(breaks = seq(from = 1998, to = 2022, by = 2))+
  theme_bw()+
  scale_y_continuous(expand = c(0, 0))+
  labs(title = "New impairments by IR cycle", x = NULL,
       y = "Number impairments",
       subtitle = "Number of Impairments (AU/parameter combination) added in each IR cycle") + 
  theme(axis.text = element_text(size = 12), axis.text.y = element_text(size = 12), 
        axis.title.y = element_text(size = 12), legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))


ggsave(filename = "New impairments by IR cycle.png", width = 9.48, height = 6.19, units = c("in"))



Attaining <- AU_all_rollup %>%
   filter(AU_final_status == 2)
  # distinct(AU_ID, Pollutant, Assessment, Pollu_ID,wqstd_code, period, DO_Class )


Impaired <- AU_all_rollup %>%
  filter(grepl( 5, AU_final_status, fixed = TRUE) | grepl( 4, AU_final_status, fixed = TRUE)) %>%
  distinct(AU_ID, Pollutant, Assessment, Pollu_ID,wqstd_code, period, DO_Class )
    
total_units <- 7273

impaired_units <- AU_all_rollup %>%
  filter(grepl( 5, AU_final_status, fixed = TRUE) | grepl( 4, AU_final_status, fixed = TRUE)) %>%
  distinct(AU_ID, Pollutant, Assessment, Pollu_ID,wqstd_code, period, DO_Class ) %>%
  pull(AU_ID) %>%
  unique()

assessed_22_units <- AU_all_rollup %>%
  filter(assessed_2022 == "Yes") %>%
  pull(AU_ID) %>%
  unique()

total_assessed <- AU_all_rollup %>%
  pull(AU_ID) %>%
  unique()


