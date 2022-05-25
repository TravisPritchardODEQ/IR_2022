

AU_all_rollup <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Draft List/Final List/AU_all_rollup.xlsx",
                           sheet = "AU_all")

assessments_2022 <- AU_all_rollup %>%
  filter(assessed_2022 == 'Yes')

impaired <- AU_all_rollup %>%
  filter(str_detect( AU_final_status, "[54]") )

impaired_parameter_summary <- impaired %>%
  group_by(Pollutant) %>%
  summarise(num_impairements = sum(str_detect( AU_final_status, "[54]"))) %>%
  arrange(desc(num_impairements))


impaired_parameter_summary2 <- impaired %>%
  group_by(Pollutant, assessed_2022) %>%
  summarise(num_impairements = sum(str_detect( AU_final_status, "[54]"))) %>%
  arrange(desc(num_impairements))

top_5_vector <- impaired_parameter_summary %>%
  head(n = 5) %>%
  pull(Pollutant)


palette <- c("#15647A", 
             '#93BDC6', 
             '#CAF4F9', 
             '#DA9E3E', 
             '#CC928D')

# palette <- c("#264653", 
#              '#2A9D8F', 
#              '#E9C46A', 
#              '#F4A261', 
#              '#E76F51')

ggplot(data = filter(impaired_parameter_summary2, Pollutant %in% top_5_vector), 
       aes(x = Pollutant, y = num_impairements,  fill=assessed_2022))  +
  geom_bar(stat="identity") +
  scale_fill_manual(values=palette) +
  theme_bw()+
  scale_y_continuous(expand = c(0, 0),limits = c(0, 2500))+
  labs(title = "2022 IR Impairements", x = NULL,
       y = "Assessment Unit/Parameter Impairements ", fill = "Assessed in 2022",
       subtitle = "Overall number of Assessment Unit/Parameter impairments\nfor the top 5 impaired parameters") + 
  theme(axis.text = element_text(size = 12), axis.text.y = element_text(size = 12), 
        axis.title.y = element_text(size = 12), legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))