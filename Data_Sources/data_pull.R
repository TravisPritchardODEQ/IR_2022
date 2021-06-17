library(magrittr)


# This script will generate AWQMS import files from publicly available databases. 
# 1. USGS NWIS
# 2. NERRS


# NWIS ------------------------------------------------------------------------------------------------------------


source("Data_Sources/NWIS_cont_data_pull.R")

#2015-12-02 represents 30 days from 2016/01/01. This date is used so we can generate the 30 day averages for the period
#ending in 1/1/2016. 
NWIS_cont_data_pull(start.date = "2015-12-02", 
                    end.date = "2020-12-31",
                    project = "Call For Data 2022",
                    save_location = "//deqhq1/WQASSESSMENT/2022IRFiles/DataSources/NWIS/")



# NERRS -----------------------------------------------------------------------------------------------------------

#This function pulls together downloaded NERRS data files
# Need to make sure project is sorted out. 
# Data should be downloaded from http://cdmo.baruch.sc.edu/get/landing.cfm

source("Data_Sources/National_Estuary_sum_stat.R")
NERRS_sum_stats()



# City of Portland ------------------------------------------------------------------------------------------------
source("Data_Sources/copbes_data.R")

stations <- c("158",
              "21B",
              "92B",
              "CRD",
              "CRU",
              "FC8",
              "P0006",
              "P0012",
              "P0016",
              "P0017",
              "P0058",
              "P0060",
              "P0080",
              "P0122",
              "P0124",
              "P0129",
              "P0144",
              "P0170",
              "P0188",
              "P0208",
              "P0234",
              "P0250",
              "P0272",
              "P0273",
              "P0298",
              "P0314",
              "P0316",
              "P0329",
              "P0337",
              "P0352",
              "P0444",
              "P0464",
              "P0498",
              "P0513",
              "P0524",
              "P0526",
              "P0529",
              "P0544",
              "P0554",
              "P0592",
              "P0633",
              "P0641",
              "P0705",
              "P0720",
              "P0746",
              "P0754",
              "P0762",
              "P0769",
              "P0800",
              "P0825",
              "P0828",
              "P0892",
              "P0961",
              "P1002",
              "P1020",
              "P1089",
              "P1104",
              "P1130",
              "P1153",
              "P1184",
              "P1194",
              "P1212",
              "P1292",
              "P1312",
              "P1360",
              "P1376",
              "P1404",
              "P1473",
              "P1593",
              "P1612",
              "P1642",
              "P1744",
              "P1769",
              "P1778",
              "P1781",
              "P1793",
              "P1809",
              "P1834",
              "P1857",
              "P1865",
              "P1872",
              "P1916",
              "P1936",
              "P1985",
              "P2000",
              "P2113",
              "P2154",
              "P2177",
              "P2185",
              "P2208",
              "P2290",
              "P2318",
              "P2320",
              "P2377",
              "P2384",
              "P2400",
              "TC4",
              "VNB")

for(i in 1:length(stations)) {
  
  location  <-  stations[[i]]
  
}
