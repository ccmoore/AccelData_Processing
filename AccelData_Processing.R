library(readr)
library(dplyr)
library(RSQLite)
library(PhysicalActivity)

# 1) Take 1-sec data given by accelerometer and add a time stamp, reintegrate to 60-sec, 
# add a day variable, and classify wear time to create PrepedData
source("./Functions/acceldata_prep.R")
acceldata_prep(input_dir = "./Data", file_pattern = "*1secAGdata.csv", output_dir = "./Outputs",
               newname_start = 1, newname_stop = 2)

### Also created custom function for reintegrating 1-sec epoch files to 60-sec epoch
source("./Functions/reintegrate_accel.R")

# 2) Take PrepedData and use sleep logs to fitler out data collected during sleep
source("./Functions/sleep_filter.R")
sleep_filter(PAdata_dir = "./Outputs", PAfile_pattern = "^PrepedData", sleeplog_dir = "./Data",
             sleeplog_pattern= "*SleepLog", newname_start = 12, newname_stop = 13)