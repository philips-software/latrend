library(usethis)
library(latrend)
source('osa.R')

# Test data ####
testLongData = generateLongData() %>%
  .[, .(Id, Time, Value, Cluster)]

use_data(testLongData, overwrite=TRUE)

# OSA case study data ####
osaData = generate_osa_data()
OSA1y = osaData[, .(Patient=Id, Day=Time, HoursOfUse=Usage, Profile=Group)]

OSA1y14 = transformToAverage(osaData, binSize=14) %>%
  .[, .(Patient=Id, Day=Time, HoursOfUse=Usage, Profile=Group)]

OSA1y30 = transformToAverage(osaData, binSize=30) %>%
  .[, .(Patient=Id, Day=Time, HoursOfUse=Usage, Profile=Group)]

use_data(OSA1y, OSA1y14, OSA1y30, overwrite=TRUE)
