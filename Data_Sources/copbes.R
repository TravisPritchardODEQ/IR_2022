library(tidyverse)



source("Data_Sources/copbes_data.R")



copBES_AWQMS <- function(station, 
                         startdate = "2016-01-01", 
                         enddate = "2020-12-31", 
                         char = c('Dissolved oxygen.Primary',
                                  'Dissolved oxygen saturation.Primary',
                                  'pH.Primary',
                                  'Temperature.7DADM' ),
                         project,
                         save_location = "Data_Sources/copbes/") {
  # station = "158"
  # startdate = "2016-01-01"
  # enddate = "2020-12-31"
  # project = "call for data 2022"
  # 
  # char <- c('Dissolved oxygen.Primary',
  #            'Dissolved oxygen saturation.Primary',
  #            'pH.Primary',
  #            'Temperature.7DADM')
  # save_location = "Data_Sources/copbes/"

  

# Error checking --------------------------------------------------------------------------------------------------

  if(!any(char %in% c('Dissolved oxygen.Field Visits', 'Dissolved oxygen.Primary',
                      'Dissolved oxygen saturation.Field Visits', 'Dissolved oxygen saturation.Primary',
                      'pH.Field Visits', 'pH.Primary',
                      'Specific conductance.Field Visits', 'Specific conductance.Primary',
                      'Temperature.Field Visits', 'Temperature.Primary', 'Temperature.7DADM'))) {
    stop("non valid value in 'char'")
  }  
  
  if(missing(project)){
    stop("'project' cannot be null.")
    
  }
  

# fetch data ------------------------------------------------------------------------------------------------------

  

data_fetch <- copbes_data(station = station, startdate = startdate, enddate = "2020-12-31", char = char)

#Filter out lower quality data
 # grade codes
   # -3  = Gap
   # -2  = UNUSABLE
   # -1  = UNSP
   # 0   = UNDEF
   # 20  = Poor
   # 50  = Suspect
   # 100 = Good

data_fetch_hq <- data_fetch %>%
  filter(Grade.Code == 100)

# Temperature -----------------------------------------------------------------------------------------------------

if(nrow(dplyr::filter(data_fetch_hq, Characteristic.Name == 'Temperature.7DADM')) > 0 ){


data_fetch_temp <- data_fetch_hq %>%
  dplyr::filter(Characteristic.Name == 'Temperature.7DADM') %>%
  dplyr::transmute(charID = "Temperature, water",
                   Result = Result.Value,
                   Result.Unit = Result.Unit,
                   Result.Analytical.Method.ID = "THM01",
                   RsltType = "Calculated",
                   ResultStatusID = as.character(Approval.Level),
                   StatisticalBasis = '7DMADMax',
                   RsltTimeBasis = ifelse(StatisticalBasis == "7DMADMax", "7 day", "1 Day" ),
                   cmnt = Comment,
                   ActivityType = "FMC",
                   Monitorining.Location.ID = Monitorining.Location.ID,
                   SmplColMthd = "ContinuousPrb",
                   SmplColEquip = "Probe/Sensor",
                   SmplDepth = "",
                   SmplDepthUnit = "",
                   SmplColEquipComment = "",
                   Samplers = "",
                   Equipment = "Continuous Probe",
                   Project = project,
                   ActStartDate = datetime,
                   ActStartTime = "0:00",
                   ActStartTimeZone = "PST",
                   ActEndDate = datetime,
                   ActEndTime = "0:00",
                   ActEndTimeZone = "PST",
                   AnaStartDate = "",
                   AnaStartTime = "",
                   AnaStartTimeZone = "",
                   AnaEndDate = "",
                   AnaEndTime = "",
                   AnaEndTimeZone = "",
                   ActivityID = paste0(Monitorining.Location.ID, ":", gsub("-","",ActStartDate), ":", ActivityType)
  ) 



}

# DO --------------------------------------------------------------------------------------------------------------

if(nrow(dplyr::filter(data_fetch_hq, Characteristic.Name == 'Dissolved oxygen.Primary')) > 0 ){

data_fetch_DO <-   data_fetch_hq %>%
  dplyr::filter(Characteristic.Name == 'Dissolved oxygen.Primary') %>%
  dplyr::mutate(hr =  format(datetime, "%Y-%j-%H"))

#Simplify to hourly values and Stats
hrsum <- data_fetch_DO %>%
  dplyr::group_by(Monitorining.Location.ID, hr, Result.Unit) %>%
  dplyr::summarise(date = as.Date(dplyr::first(datetime)),
            hrDTmin = min(datetime),
            hrDTmax = max(datetime),
            hrN = sum(!is.na(Result.Value)),
            hrMean = mean(Result.Value, na.rm=TRUE),
            hrMin = min(Result.Value, na.rm=TRUE),
            hrMax = max(Result.Value, na.rm=TRUE))


# For each date, how many hours have hrN > 0
# remove rows with zero records in an hour. 
hrdat<- hrsum[which(hrsum$hrN >0),]

# Summarise to daily statistics
daydat <- hrdat %>%
  dplyr::group_by(Monitorining.Location.ID, date) %>%
  dplyr::summarise(  dDTmin = min(hrDTmin),
              dDTmax = max(hrDTmax),
              hrNday = length(hrN), 
              dyN = sum(hrN),
              dyMean = mean(hrMean, na.rm=TRUE),
              dyMin = min(hrMin, na.rm=TRUE),
              dyMax = max(hrMax, na.rm=TRUE))

daydat <- daydat %>%
  dplyr::rowwise() %>%
  dplyr::mutate(ResultStatusID = ifelse(hrNday >= 22, 'Final', "Rejected")) %>%
  dplyr::mutate(cmnt =ifelse(hrNday >= 22, "Generated by ORDEQ", ifelse(hrNday <= 22 & hrNday >= 20, 
                                                                 paste0("Generated by ORDEQ; Estimated - ", as.character(hrNday), ' hrs with valid data in day' ), 
                                                                 paste0("Generated by ORDEQ; Rejected - ", as.character(hrNday), ' hrs with valid data in day' )) )) %>%
  dplyr::mutate(ma.mean7 = as.numeric(""),
         ma.min7 = as.numeric(""),
         ma.mean30 = as.numeric(""),
         ma.max7 = as.numeric(""))

#create list for getting data out of loop
monloc_do_list <- list()

#monitoring location loop
for(j in 1:length(unique(daydat$Monitorining.Location.ID))){
  print(paste("Station", j, "of", length(unique(daydat$Monitorining.Location.ID))))
  
  station <- unique(daydat$Monitorining.Location.ID)[j]
  
  #Filter dataset to only look at 1 monitoring location at a time
  daydat_station <- daydat %>%
    dplyr::filter(Monitorining.Location.ID == station) %>%
    dplyr::mutate(startdate7 = as.Date(date) - 6,
           startdate30 = as.Date(date) -30)
  
  # 7 day loop
  # Loops throough each row in the monitoring location dataset
  # And pulls out records that are within the preceding 7 day window
  # If there are at least 6 values, then calculate 7 day min and mean
  # Assigns data back to daydat_station
  print("Begin 7 day moving averages")
  pb <- txtProgressBar(min = 0, max = nrow(daydat_station), style = 3)
  for(k in 1:nrow(daydat_station)){
    
    start7 <- daydat_station$startdate7[k]
    end7 <- daydat_station$date[k] 
    
    station_7day <- daydat_station %>%
      dplyr::filter(date <= end7 & date >= start7) %>%
      dplyr::filter(hrNday >= 22) 
    
    ma.mean7 <- ifelse(length(unique(station_7day$date)) >= 6, mean(station_7day$dyMean), NA )
    ma.min7 <- ifelse(length(unique(station_7day$date)) >= 6, mean(station_7day$dyMin), NA )
    
    daydat_station[k,"ma.mean7"] <- ifelse(k >=7, ma.mean7, NA)
    daydat_station[k, "ma.min7"] <- ifelse(k >=7, ma.min7, NA)
    
    
    setTxtProgressBar(pb, k)
    
  } #end of 7day loop
  close(pb)
  # 30 day loop
  # Loops throough each row in the monitoring location dataset
  # And pulls out records that are within the preceding 30 day window
  # If there are at least 29 values, then calculate 30 day mean
  # Assigns data back to daydat_station
  print("Begin 30 day moving averages" )
  pb <- txtProgressBar(min = 0, max = nrow(daydat_station), style = 3)
  for(l in 1:nrow(daydat_station)){
    
    
    start30 <- daydat_station$startdate30[l]
    end30 <- daydat_station$date[l] 
    
    station_30day <- daydat_station %>%
      dplyr::filter(date <= end30 & date >= start30) %>%
      dplyr::filter(hrNday >= 22) 
    
    ma.mean30 <- ifelse(length(unique(station_30day$date)) >= 29, mean(station_30day$dyMean), NA )
    
    
    daydat_station[l,"ma.mean30"] <- ifelse(l >= 30, ma.mean30, NA)
    setTxtProgressBar(pb, l)
  } #end of 30day loop
  
  close(pb)
  # Assign dataset filtered to 1 monitoring location to a list for combining outside of for loop
  monloc_do_list[[j]] <- daydat_station
  
  
  
} # end of monitoring location for loop

# Combine list to single dataframe
sum_stats_DO <-dplyr::bind_rows(monloc_do_list)    


#Gather summary statistics from wide format into long format
#rename summary statistcs to match AWQMS Import COnfiguration
sumstat_long <- sum_stats_DO %>%
  dplyr::rename("Daily Maximum" = dyMax,
         "Daily Minimum" = dyMin,
         "Daily Mean"    = dyMean,
         "7DMADMin"      = ma.min7,
         "7DMADMean"     = ma.mean7,
         "7DMADMax"      = ma.max7,
         "30DMADMean"    = ma.mean30) %>%
  tidyr::gather(
    "Daily Maximum",
    "Daily Minimum",
    "Daily Mean",
    "7DMADMin",
    "7DMADMean",
    "7DMADMax",
    "30DMADMean",
    key = "StatisticalBasis",
    value = "Result",
    na.rm = TRUE
  ) %>% 
  dplyr::arrange(Monitorining.Location.ID, date) 

#   left_join(Audits_unique, by = c("Monitoring.Location.ID", "charID" = "Characteristic.Name") )
DO_AWQMS <- sumstat_long %>%
  dplyr::mutate(charID = 'Dissolved oxygen (DO)',
         RsltTimeBasis = ifelse(StatisticalBasis == "7DMADMin" |
                                  StatisticalBasis == "7DMADMean" |
                                  StatisticalBasis == "7DMADMax", "7 Day", 
                                ifelse(StatisticalBasis == "30DMADMean", "30 Day", "1 Day" )),
         ActivityType = "FMC",
         Result.Analytical.Method.ID = "NFM 6.2.1-LUM", 
         SmplColMthd = "ContinuousPrb",
         SmplColEquip = "Probe/Sensor",
         SmplDepth = "",
         SmplDepthUnit = "",
         SmplColEquipComment = "",
         Samplers = "",
         Project = project,
         AnaStartDate = "",
         AnaStartTime = "",
         AnaEndDate = "",
         AnaEndTime = "",
         ActStartDate = as.Date(format(dDTmax, "%Y-%m-%d")), 
         ActStartTime = format(dDTmin, "%H:%M"),
         ActEndDate = as.Date(format(dDTmax, "%Y-%m-%d")),
         ActEndTime = format(dDTmax, "%H:%M"),
         RsltType = "Calculated",
         ActStartTimeZone = "",
         ActEndTimeZone = "",
         AnaStartTimeZone = "",
         AnaEndTimeZone = "",
         Result = round(Result, digits = 2),
         Result.Unit = "mg/l",
         Equipment = Monitorining.Location.ID,
         ActivityID = paste0(Monitorining.Location.ID, ":", gsub("-","",ActStartDate), ":", ActivityType),
         ResultStatusID = as.character(ResultStatusID)
  ) %>%
  dplyr::select(charID,
         Result,
         Result.Unit,
         Result.Analytical.Method.ID,
         RsltType,
         ResultStatusID,
         StatisticalBasis,
         RsltTimeBasis,
         cmnt,
         ActivityType,
         Monitorining.Location.ID,
         SmplColMthd,
         SmplColEquip,
         SmplDepth,
         SmplDepthUnit,
         SmplColEquipComment,
         Samplers,
         Equipment,
         Project,
         ActStartDate,
         ActStartTime,
         ActStartTimeZone,
         ActEndDate,
         ActEndTime,
         ActEndTimeZone,
         AnaStartDate,
         AnaStartTime,
         AnaStartTimeZone,
         AnaEndDate,
         AnaEndTime,
         AnaEndTimeZone,
         ActivityID)

}


#Write summary stat tables. CHeck to see if parameter exists first.

if(exists("DO_AWQMS") & exists("data_fetch_temp") ){

AWQMS_sum_stats <- dplyr::bind_rows(DO_AWQMS, data_fetch_temp) %>%
  dplyr::arrange(Monitorining.Location.ID, ActStartDate)

openxlsx::write.xlsx(AWQMS_sum_stats, file = paste0(save_location, station, "-sumstats.xlsx" ))

} else if(exists("DO_AWQMS") & !exists("data_fetch_temp") ){
  openxlsx::write.xlsx(DO_AWQMS, file = paste0(save_location, station, "-sumstats.xlsx" ))
} else if(!exists("DO_AWQMS") & exists("data_fetch_temp") ){
  openxlsx::write.xlsx(data_fetch_temp, file = paste0(save_location, station, "-sumstats.xlsx" ))
} 

# pH --------------------------------------------------------------------------------------------------------------

if(nrow(dplyr::filter(data_fetch_hq, Characteristic.Name == 'pH.Primary')) > 0 ){

data_fetch_pH <- data_fetch_hq %>%
  dplyr::filter(Characteristic.Name == 'pH.Primary') %>%
  dplyr::transmute('Monitoring_Location_ID' = Monitorining.Location.ID,
                   "Activity_start_date" = format(datetime, "%Y/%m/%d"),
                   'Activity_Start_Time' =format(datetime, "%H:%M:%S"),
                   'Activity_Time_Zone' = "",
                   'Equipment_ID' = Monitorining.Location.ID,
                   'Characteristic_Name' = 'pH',
                   "Result_Value" = Result.Value,
                   "Result_Unit" = Result.Unit,
                   "Result_Status_ID" = Approval.Level)





pH_deployments <-   data_fetch_pH %>%
  dplyr::mutate(datetime = lubridate::ymd_hms(paste(Activity_start_date,Activity_Start_Time ))) %>%
  dplyr::group_by(Monitoring_Location_ID, Equipment_ID) %>%
  dplyr::summarise(Activity_start_date_time = min(datetime),
                   Activity_end_date_time  = max(datetime + lubridate::seconds(1)),
                   Activity_start_end_time_Zone = dplyr::first(Activity_Time_Zone)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Media = "Water",
                Media_subdivision = "Surface Water",
                Project_ID = project,
                Alternate_Project_ID = "",
                Alternate_Project_ID2 = "",
                Frequency_on_minutes = "",
                Depth_in_m = "")

openxlsx::write.xlsx(data_fetch_pH, file = paste0(save_location, "cont_pH-", station, ".xlsx" ))
openxlsx::write.xlsx(pH_deployments, file = paste0(save_location, "cont_pH-", station, "-deployments.xlsx" ))




}

}


# 
# copBES_AWQMS("158", startdate = '2016-01-01', enddate = "2020-12-31", char = c('Dissolved oxygen.Primary',
#                                                                                'Dissolved oxygen saturation.Primary',
#                                                                                'pH.Primary',
#                                                                                'Temperature.7DADM' ),
#              project = "Call for data 2022")
