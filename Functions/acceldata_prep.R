acceldata_prep <- function(input_dir, file_pattern, output_dir,
                           subjectID_length = 2, subjectID_start = 1) {
  files_1sec <- list.files(input_dir, pattern = file_pattern)

  for(i in 1:length(files_1sec)) {
    current_file <- paste0("./Data/", files_1sec[i])
    acceldata_1sec <- read.csv(current_file, stringsAsFactors = FALSE, skip = 1)

    # Create timestamp
    acceldata_1sec$TimeStamp <- as.POSIXlt(paste(acceldata_1sec$date, acceldata_1sec$epoch),
                                           format="%m/%d/%Y %I:%M:%S %p")

    # Reintegrate 1-sec to 60-sec epoch
    acceldata_reint <- dataCollapser(dataset = acceldata_1sec , TS = "TimeStamp", by = 60)

    # Add day variable
    acceldata_days <- markingTime(dataset = acceldata_reint, timestamp = "TimeStamp",
                                  startTime = "00:00:00", tz = "UTC")
    # Classify weartime
    acceldata_final <- wearingMarking(acceldata_days, frame = 90, perMinuteCts = 1,
                                      TS = "TimeStamp", cts = "axis1",
                                      streamFrame = NULL, allowanceFrame = 2, newcolname = "wearing",
                                      getMinuteMarking = FALSE, dayStart = "00:00:00", tz = "UTC")
    write.csv(acceldata_final,
              paste0(output_dir,"/PrepedData_",
                     substr(files_1sec[i], start = subjectID_start, stop = (subjectID_start+subjectID_length-1)),
                     ".csv"),
              row.names = FALSE)
  }
}


