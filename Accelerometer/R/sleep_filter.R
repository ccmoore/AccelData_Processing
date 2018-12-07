
#'Use a sleep log to remove accelerometer data collected during sleep
#'
#'Imports the sleep log and accelerometer physical activity file for each
#'subject, removes all epochs occuring between the bed and wake time of each
#'day (as indicated in the sleep log), and exports new files to a directory
#'with the names 'FilteredWakeData_ID' where 'ID' is the unique subject
#'identifier. Each accelerometer physical activity file must contain a
#''TimeStamp' column with POSIXlt object of date and time for each epoch)
#'
#'@param acceldata_dir character sting of path to directory with accelerometer
#' files
#'
#'@param output_dir charcter sting of path to directory where all the new
#' 'FilteredWakeData' files will be exported
#'
#'@param sleeplog_dir character sting of path to directory with sleep log files
#'
#'@param subjectID_length numeric value of number of characters comprising the
#' unique subject identifier in the names of the accelerometer and sleep log
#' files (e.g. if naming convention is "PrepedData_01" then set to 2, if naming
#' convention is "XXXX_data" then set to 4)
#'
#'@param accelfile_pattern charcter sting of naming convention for identifying
#' all accelerometer files in acceldata_dir (equivalent to 'pattern' argument
#' of list.files() function)
#'
#'@param sleeplog_pattern charcter sting of naming convention for identifying
#' all sleep log files in sleeplog_dir (equivalent to 'pattern' argument
#' of list.files() function)
#
#'@param accelfile_IDstart numeric value of the character in the names of the
#' accelerometer files in acceldata_dir where the unique subject identifier
#'  starts (e.g., if naming convention is "PrepedData_01" then set to 12, if
#'  naming convention is"XXXX_data" then set to 1)
#'
#'@param sleeplog_IDstart numeric value of the character in the names of the
#' sleep log files in sleeplog_dir where the unique subject identifier starts
#' (e.g., if naming convention is "01_sleeplog_01" then set to 1, if naming
#' convention is "sleeplog_XXXX" then set to 1)
#'
#'@return Writes a CSV file ('FilteredWakeData_ID.csv') in output_dir for each
#' accelerometer file in the acceldata_dir. Each new file is the same as the
#' input accelerometer file except all epochs occuring between the bed and
#' wake time of each day (as indicated in the sleep log) are removed
#'@export
#'
#' @examples
#'  sleep_filter(acceldata_dir = "./Outputs", sleeplog_dir = "./Data",
#'              output_dir = "./Outputs", subjectID_length = 2,
#'              accelfile_pattern = "^PrepedData", sleeplog_pattern = "*SleepLog",
#'              accelfile_IDstart = 12, sleeplog_IDstart = 1)

sleep_filter <- function(acceldata_dir, sleeplog_dir, output_dir, subjectID_length,
                         accelfile_pattern = "^PrepedData", sleeplog_pattern = "*SleepLog",
                         accelfile_IDstart = 12, sleeplog_IDstart = 1){

  sleeplog_files <- list.files(sleeplog_dir, pattern = sleeplog_pattern)
  accel_files <- list.files(acceldata_dir, pattern = accelfile_pattern)

  # Error if subject with an accel file does not have a sleep log
  subjects_accel_files <- substr(accel_files, start = accelfile_IDstart, stop = (accelfile_IDstart+subjectID_length-1))
  subjects_sleep_files <- substr(sleeplog_files, start = sleeplog_IDstart,
                                 stop = (sleeplog_IDstart+subjectID_length-1))
  if(any(!subjects_accel_files %in% subjects_sleep_files)) {
    stop(paste("No sleep log for Subject",
               subjects_accel_files[which(!subjects_accel_files %in% subjects_sleep_files)]),
         call. = FALSE)
  }

  # Continuing with code for filtering out sleep time
  for(i in 1:length(accel_files)) {
    current_accel_file <- paste0(acceldata_dir,"/", accel_files[i])
    accel_data <- read.csv(current_accel_file, stringsAsFactors = FALSE)
    current_sleeplog_file <- paste0(sleeplog_dir,"/", sleeplog_files[i])
    sleeplog_data <- read.csv(current_sleeplog_file, stringsAsFactors = FALSE)

    for(j in 1:length(sleeplog_data$Day)) {
      wakeup_time <- as.POSIXlt(paste(sleeplog_data$Date[j], sleeplog_data$Wakeup_Time[j]),
                                format="%m/%d/%Y %I:%M:%S %p")
      bed_time <-  as.POSIXlt(paste(sleeplog_data$Date[j], sleeplog_data$Bed_Time[j]),
                              format="%m/%d/%Y %I:%M:%S %p")
      daywake_data <- accel_data %>%
        filter(days == j) %>%
        filter(TimeStamp >= wakeup_time & TimeStamp <= bed_time)

      if(j==1){
        wake_data <- daywake_data
      } else {
        wake_data <- rbind(wake_data, daywake_data)
      }
    }
     write.csv(wake_data,
               paste0(output_dir,"/FilteredWakeData_", subjects_accel_files[i], ".csv"),
               row.names = FALSE)
  }
}
