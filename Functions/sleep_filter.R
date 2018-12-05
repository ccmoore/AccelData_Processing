sleep_filter <- function(PAdata_dir, sleeplog_dir, subjectID_length, PAfile_pattern = "^PrepedData",
                         sleeplog_pattern = "*SleepLog", sleeplog_IDstart = 1, output_dir = PAdata_dir){

  sleeplog_files <- list.files(sleeplog_dir, pattern = sleeplog_pattern)
  pa_files <- list.files(PAdata_dir, pattern = PAfile_pattern)

  # Error if subject with a PA file does not have a sleep log
  subjects_pa_files <- substr(pa_files, start = 12, stop = (11+subjectID_length))
  subjects_sleep_files <- substr(sleeplog_files, start = sleeplog_IDstart,
                                 stop = (sleeplog_IDstart+subjectID_length-1))
  if(any(!subjects_pa_files %in% subjects_sleep_files)) {
    stop(paste("No sleep log for Subject",
               subjects_pa_files[which(!subjects_pa_files %in% subjects_sleep_files)]),
         call. = FALSE)
  }

  # Continuing with code for filtering out sleep time
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
               paste0(output_dir,"/FilteredWakeData_", subjects_pa_files[i], ".csv"),
               row.names = FALSE)
  }
}