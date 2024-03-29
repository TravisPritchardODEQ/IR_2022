library(tidyverse)
library(lubridate)
library(wql)
library(zoo)
library(readxl)
library(AWQMSdata)

#ml/L to mg/L conversion bases on https://github.com/GLEON/LakeMetabolizer/issues/101


ooi_DO_data <- AWQMS_Data_Cont(org = "OOI_(NOSTORETID)",
                          char = "Dissolved oxygen (DO)")

sdadm <- ooi_DO_data %>%
  
  mutate(result_conv = Result_Numeric* (1.4/63.9)*1.42905, #convert to mg/L
         result_unit = 'mg/l',
         date = ymd(Result_Date),
         month = month(date),  
         yrmon = as.yearmon(date), 
         year = year(date),
         loc_depth = paste0(MLocID, "-", Depth,Depth_Unit )) %>%
  group_by(MLocID,loc_depth, year, month, Depth) %>%
  summarise(sdadm = mean(result_conv),
            num_days = n_distinct(date),
            count = n()) %>%
  ungroup() %>%
  complete(loc_depth, year, month) 


sdadm_count <- sdadm %>%
  mutate(mo_days = days_in_month(month),
         diff = mo_days - num_days,
         exclude = ifelse(diff > 3 | is.na(sdadm), "yes", "no")) %>%
    select(MLocID, loc_depth, year, month, exclude)


sdadm_assess <- sdadm %>%
  left_join(sdadm_count, by = c("loc_depth", "year", "month", "MLocID"))%>%
  filter(exclude == "no") %>%
  select(MLocID, loc_depth,  Depth, year, month, sdadm) %>%
  mutate(date = paste0(year, "-", month, "-01")) %>%
  mutate(date = ymd(date)) %>%
  mutate(variable = as.factor("DO")) %>%
  select(date, MLocID,loc_depth,  Depth, variable, sdadm) %>%
  rename(value = sdadm,
         site = MLocID) 




#create empty list to acept test results
kendall_list <- list()




for(i in 1:length(unique(sdadm_assess$loc_depth))){
  
  #select unique site ID and arrange in chonological order
  #fill in NAs for missing months

  sdadm_seamannkenn <- sdadm_assess %>%
    filter(loc_depth == unique(sdadm_assess$loc_depth)[i]) %>%
    mutate(Depth = as.numeric(Depth))
  
  #create wqdata format table
  wqdataframe <- wqData(sdadm_seamannkenn, c(1, 2, 4), 5:6, site.order = TRUE, type = "long",
                        time.format = "%y-%m-%d")
  
  #create time series
  timeseries <- tsMake(wqdataframe, focus= wqdataframe$site[1])
  
  res <- seaKen(timeseries)
  
  #save results as a dataframe and add MLocID and n
  df <- data.frame(as.list(unclass(res))) %>%
    mutate(loc_depth = unique(sdadm_assess$loc_depth)[i],
           n = length(timeseries))
  
  


  #bind results list
  kendall_list[[i]] <- df
  
  
  
} #end of for loop

#bind results of seasonal mann kendall test to single dataframe
kendall_results <- bind_rows(kendall_list) 


#trend is detected when the tau > critical value and p. value (sl) is <0.10
#reorder and rename rows for better exporting
kendall_results <- kendall_results %>%
  mutate(significance = ifelse(p.value < 0.10 & sen.slope < 0, "Signifigant (-)", 
                               ifelse(p.value < 0.10 & sen.slope > 0, "Signifigant (+)",
                                      "No Trend"))) %>%
  select(loc_depth, significance, p.value, sen.slope)




# graphs ----------------------------------------------------------------------------------------------------------
# for loop to process each MLocID


#join trend analysis to data
sdadm_trend <- sdadm %>%
  left_join(kendall_results, by = "loc_depth") %>%
  left_join(sdadm_count) %>%
  filter(exclude == "no") %>%
  select(-exclude)

# #set up some data for graphing
# sdadm_raw_trend <-ooi_DO_data %>%
#   mutate(result_conv = Result_Numeric* (1.4/63.9),
#          result_unit = 'ml/l',
#          date = ymd(Result_Date),
#          month = month(date),  
#          yrmon = as.yearmon(date), 
#          year = year(date),
#          moname = month.name[month],
#          StationDes = case_when(StationDes == "North Myrtle Creek at Evergreen Park" ~ "North Myrtle at mouth", 
#                                 StationDes == "Windy Creek at Glendale" ~ "Windy Creek near Glendale",
#                                 TRUE ~ StationDes )) %>%
#   left_join(kendall_results, by = "loc_depth") %>%
#   left_join(sdadm_count, by = c("loc_depth", "year", "month")) %>%
#   filter(exclude == "no") %>%
#   select(-exclude)


  # 
  # #box plots
  # sdadm_box_plot <- sdadm_raw_trend
  # 

  

# Station info ----------------------------------------------------------------------------------------------------

station <- ooi_DO_data %>%
  select(OrganizationID, MLocID, StationDes,Lat_DD, Long_DD, AU_ID ) %>%
  distinct()

  
  #point graph
  sdadm_month_average <- sdadm_trend %>%
    mutate(yearmon = as.Date(paste0(year,"-",month,"-1"))) %>%
    mutate(moname = month.name[month]) %>%
    filter(!is.na(sdadm)) %>%
    left_join(station)
  
  
  p <- ggplot(data = sdadm_month_average)+
    geom_point(aes(x = yearmon, y = sdadm, color = Depth), size = 2, position = position_dodge(0.15)) +
    geom_line(aes(x = yearmon, y = sdadm, color = Depth)) +
    #geom_smooth(aes(x = yearmon, y = sdadm, color = Depth), method = "lm") +
    #scale_x_continuous(breaks = seq(min(sdadm_month_average$year), max(sdadm_month_average$year),1),
                      # minor_breaks = 1) +
    #scale_color_brewer(palette="Set1") +
    #coord_cartesian(ylim = c(12,30)) +
    labs(title = "Average Monthly Dissolved Oxygen",
         subtitle = paste0(sdadm_month_average$MLocID[1], ": ",sdadm_month_average$StationDes[1] ),
         caption = paste0("Lat/Long: ",
                          sdadm_month_average$Lat_DD[1], ", ",sdadm_month_average$Long_DD[1] ),
         x = element_blank(),
         y = "mg/L")+
    theme_bw()
    # theme(plot.title = element_text(hjust = 0.5, size = 12),
    #       plot.subtitle =  element_text(hjust = 0.5),
    #       axis.text.x = element_text(angle = 50, hjust = 1)) 
    #guides(color = guide_legend(title = "Month"), order = 1)
  
  #graph no trend values
  if(sdadm_month_average$significance[1] == "No Trend"){
    p = p + annotate("text", label = "No Trend", 
                     x =  as.Date('2020-06-01'),
                     y = 1.75,
                     colour = "black", size = 3.5) 
    
  } #end of no trend if statement
  
  if(sdadm_month_average$significance[1] != "No Trend"){
    p = p +  annotate("text", label = "Significant Trend (p-value < 0.10)", 
                      x = ymd('2020-06-01'),
                      y = 1.7,
                      colour = "black", size = 3.5) 
    
    
    
    
    #plot trend line. This is taken from the coho trends process
    slope <- kendall_results[kendall_results$MLocID == unique(sdadm_raw_trend$MLocID)[1], "sen.slope"]
    x.delta <- as.numeric((max(sdadm_month_average$year) - min(sdadm_month_average$year)))/2
    SK.min <- mean(sdadm_month_average$sdadm, na.rm = TRUE) - x.delta*slope
    SK.max <- mean(sdadm_month_average$sdadm, na.rm = TRUE) + x.delta*slope
    
    p <- p + geom_segment(aes(x = ymd('2018-01-01'), y = SK.min,
                              xend =ymd('2021-02-01'), yend = SK.max, linetype = "SeasMannKendall Trend"),
                          size = 1.05, color = "gray49") +
      scale_linetype_manual(values=c("dotted")) +
      guides(linetype=guide_legend(title = element_blank(), order = 2))
    
    
  }
  
  
  
  #Add hypoxia lines
  
  water_status <- data.frame(
  stringsAsFactors = TRUE,
                                Status = c("Hypoxic","Hypoxic",
                                         "Severe Hypoxia","Severe Hypoxia","Suboxic",
                                         "Suboxic","Anoxic","Anoxic"),
                                Date = c("1/1/2018","2/1/2021","1/1/2018",
                                         "2/1/2021","1/1/2018","2/1/2021",
                                         "1/1/2018","2/1/2021"),
             Value = c(1.4, 1.4, 0.5, 0.5, 0.1, 0.1, 0, 0)
                  ) %>%
    mutate(Date = mdy(Date),
           Value = Value*1.4276) #convert to mg/L

  water_status$Status <- factor(water_status$Status, levels = c("Hypoxic", "Severe Hypoxia", "Suboxic", "Anoxic"))    
  
  p2 <- p +
    geom_line(data = water_status, 
              aes(x = Date, y = Value, linetype = Status)) +
    scale_color_brewer(palette="Paired")
  
  
  ggsave(p2, file=paste0("Marine/Graphs/",sdadm_month_average$MLocID[1],"- ", Sys.Date(), "- average.png"),
         width = 8, height = 5, units = c("in"))

  


# Raw data --------------------------------------------------------------------------------------------------------



ooi_DO_data_graph <- ooi_DO_data %>%
  mutate(result_conv = Result_Numeric* (1.4/63.9)*1.42905,
         result_unit = 'ml/l',
         date = ymd(Result_Date),
         month = month(date),  
         yrmon = as.yearmon(date), 
         year = year(date))




q <- 
  ggplot(data = ooi_DO_data_graph) +
    geom_point(aes(x = date, y = result_conv, color = Depth)) + 
    #geom_smooth(aes(x = date, y = result_conv, color = Depth), method = "lm") +
    labs(title = "Dissolved Oxygen",
         subtitle = paste0(ooi_DO_data_graph$MLocID[1], ": ", ooi_DO_data_graph$StationDes[1]),
         caption = paste0("Lat/Long: ",
                          ooi_DO_data_graph$Lat_DD[1], ", ",ooi_DO_data_graph$Long_DD[1] ),
         x = element_blank(),
         y = "mg/L")+
    theme_bw()+
    # theme(#plot.title = element_text(hjust = 0.5, size = 12),
    #       #plot.subtitle =  element_text(hjust = 0.5),
    #       axis.text.x = element_text(angle = 50, hjust = 1))+
  geom_line(data = water_status, 
            aes(x = Date, y = Value, linetype = Status)) +
  scale_color_brewer(palette="Paired")
    
ggsave(q, file=paste0("Marine/Graphs/",ooi_DO_data_graph$MLocID[1],"- ", Sys.Date(), "-raw.png"),
       width = 8, height = 5, units = c("in"))



# Newport Line data -----------------------------------------------------------------------------------------------



newport_line_awqms <- AWQMS_Data(org = "NOAANEWPORTLINE_(NOSTORETID)",
                                 char = "Dissolved oxygen (DO)")


#Deoths range from 0-50 m. Sort into three bins, upper, mid, lower
newport_line_EDA <- newport_line_awqms %>%
  mutate(act_depth_height = as.numeric(act_depth_height)) %>%
  mutate(depth_position = case_when(act_depth_height <= 16.6667 ~ "Upper",
                                    act_depth_height <= 16.6667*2 ~ "Mid",
                                    act_depth_height <= 16.6667*3 ~ "Bottom")) %>%
  # group_by(MLocID, SampleStartDate, depth_position, Result_Unit) %>%
  # summarise(avg_do = mean(Result_Numeric) ) %>%
  mutate(period = case_when(SampleStartDate < lubridate::ymd("2016-01-01") ~ "Historical",
                            SampleStartDate >= lubridate::ymd("2016-01-01") ~ "IR Period"))

ggplot(newport_line_EDA, aes(x=avg_do, color = depth_position)) + 
  geom_density()


upper_historic <- newport_line_EDA %>%
  filter(depth_position == "Upper",
         period == 'Historical') %>%
  pull(avg_do)

upper_IR_Period <- newport_line_EDA %>%
  filter(depth_position == "Upper",
         period == 'IR Period') %>%
  pull(avg_do)


wilcox.test(upper_historic, upper_IR_Period, alternative = "two.sided")


unique(newport_line_EDA$MLocID)


for(i in 1:length(unique(newport_line_EDA$MLocID))){
  mloc <- unique(newport_line_EDA$MLocID)[i]
  
  newport_graph <- newport_line_EDA %>%
    filter(MLocID == mloc)
  
  
  p <- ggplot(newport_graph, aes(x = depth_position,  y=Result_Numeric, fill = period)) + 
    geom_boxplot() +
    labs(title = "Newport Line Disoslved Oxygen",
         subtitle =NH01$MLocID[1],
         x = "Depth",
         y = "ml/l") +
    theme_bw() +
    scale_fill_brewer(palette="Dark2")
  
  
  ggsave(p, file=paste0("Marine/Graphs/newport_line-",mloc,"-", Sys.Date(), ".png"),
         width = 8, height = 5, units = c("in"))
  
  
  
  
  
}



# Newport monthly average -----------------------------------------------------------------------------------------
library(lubridate)

newport_line_month_avg <- newport_line_awqms %>%
  mutate(act_depth_height = as.numeric(act_depth_height),
         month = month(SampleStartDate, label = TRUE),
         year = year(SampleStartDate)) %>%
  mutate(depth_position = case_when(act_depth_height <= 16.6667 ~ "Upper",
                                    act_depth_height <= 16.6667*2 ~ "Mid",
                                    act_depth_height <= 16.6667*3 ~ "Bottom")) %>%
  #group_by(MLocID, month, year, depth_position, Result_Unit) %>%
  #summarise(avg_do = mean(Result_Numeric) ) %>%
  mutate(period = case_when(year < 2016 ~ "Historical",
                            (year >= 2016 ~ "IR Period")))

for(i in 1:length(unique(newport_line_month_avg$MLocID))){
  mloc <- unique(newport_line_month_avg$MLocID)[i]
  
  newport_graph <- newport_line_month_avg %>%
    filter(MLocID == mloc)
  
  
  p <- ggplot(newport_graph, aes(x = depth_position,  y=Result_Numeric, fill = period)) + 
    geom_boxplot() +
    labs(title = "Newport Line Disoslved Oxygen",
         subtitle =newport_graph$MLocID[1],
         x = "Depth",
         y = "ml/l") +
    theme_bw() +
    scale_fill_brewer(palette="Dark2") + 
    facet_wrap(~month)
  
  
  ggsave(p, file=paste0("Marine/Graphs/newport_line-",mloc,"-", Sys.Date(), "-month.png"),
         width = 8, height = 5, units = c("in"))
  
  
  
  
  
}


NHo1_mid_aug_historical <- newport_line_month_avg %>%
  filter(MLocID == "NH01",
         month == "Aug",
         period == 'Historical',
         depth_position == "Upper") %>%
  pull(Result_Numeric)

NHo1_mid_aug_IR <- newport_line_month_avg %>%
  filter(MLocID == "NH01",
         month == "Aug",
         period == 'IR Period',
         depth_position == "Upper") %>%
  pull(Result_Numeric)


wilcox.test(NHo1_mid_aug_historical, NHo1_mid_aug_IR, alternative = "two.sided")

NHo3_mid_aug_historical <- newport_line_month_avg %>%
  filter(MLocID == "NH03",
         month == "Aug",
         period == 'Historical',
         depth_position == "Upper") %>%
  pull(Result_Numeric)

NHo3_mid_aug_IR <- newport_line_month_avg %>%
  filter(MLocID == "NH03",
         month == "Aug",
         period == 'IR Period',
         depth_position == "Upper") %>%
  pull(Result_Numeric)


 wilcox.test(NHo3_mid_aug_historical, NHo3_mid_aug_IR, alternative = "two.sided")[['p.value']]



# Mid -------------------------------------------------------------------------------------------------------------



NHo1_mid_aug_historical <- newport_line_month_avg %>%
  filter(MLocID == "NH01",
         month == "Aug",
         period == 'Historical',
         depth_position == "Mid") %>%
  pull(Result_Numeric)

NHo1_mid_aug_IR <- newport_line_month_avg %>%
  filter(MLocID == "NH01",
         month == "Aug",
         period == 'IR Period',
         depth_position == "Mid") %>%
  pull(Result_Numeric)


wilcox.test(NHo1_mid_aug_historical, NHo1_mid_aug_IR, alternative = "two.sided")

NHo3_mid_aug_historical <- newport_line_month_avg %>%
  filter(MLocID == "NH03",
         month == "Aug",
         period == 'Historical',
         depth_position == "Mid") %>%
  pull(Result_Numeric)

NHo3_mid_aug_IR <- newport_line_month_avg %>%
  filter(MLocID == "NH03",
         month == "Aug",
         period == 'IR Period',
         depth_position == "Mid") %>%
  pull(Result_Numeric)


wilcox.test(NHo3_mid_aug_historical, NHo3_mid_aug_IR, alternative = "two.sided")

