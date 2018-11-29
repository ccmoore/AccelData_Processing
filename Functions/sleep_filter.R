sleep_filter <- function(PAdata_dir, PAfile_pattern = "^PrepedData", sleeplog_dir, sleeplog_pattern, newname_start = 12, newname_stop = 13){
  
  sleeplog_files <- list.files(sleeplog_dir, pattern = sleeplog_pattern)
  pa_files <- list.files(PAdata_dir, pattern = PAfile_pattern)
  
  for(i in 1:length(pa_files)) {
    current_pa_file <- paste0(PAdata_dir,"/", pa_files[i])
    pa_data <- read.csv(current_pa_file, stringsAsFactors = FALSE)
    current_sleeplog_file <- paste0(sleeplog_dir,"/", sleeplog_files[i])
    sleeplog_data <- read.csv(current_sleeplog_file, stringsAsFactors = FALSE)
   
    for(j in 1:length(sleeplog_data$Day)) {
      wakeup_time <- as.POSIXlt(paste(sleeplog_data$Date[j], sleeplog_data$Wakeup_Time[j]),
                                format="%m/%d/%Y %I:%M:%S %p")
      bed_time <-  as.POSIXlt(paste(sleeplog_data$Date[j], sleeplog_data$Bed_Time[j]),
                              format="%m/%d/%Y %I:%M:%S %p")
      daywake_data <- pa_data %>% 
        filter(days == j) %>% 
        filter(TimeStamp >= wakeup_time & TimeStamp <= bed_time)
      
      if(j==1){
        wake_data <- daywake_data
      } else {
        wake_data <- rbind(wake_data, daywake_data)
      }
    }
     write.csv(wake_data, 
               paste0(PAdata_dir,"/FilteredWakeData_", substr(pa_files[i], start = newname_start, stop = newname_stop), ".csv"),
               row.names = FALSE)
  }
}
