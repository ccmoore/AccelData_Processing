
#'Prepare 1-sec epoch accelerometer files  for processing
#'
#'Imports each 1-sec epoch accelerometer file (direct ActiLife export file) in
#'a directory and adds a time stamp column (combination of date and time),
#'reintegrates it to 60-sec epochs, add a 'day' indicator as a new column,
#'classifies wear time (using Choi 2011 algorithm) and exports new files to a
#'directory with the names 'PrepedData_ID' where 'ID' is the unique subject
#'identifier
#'
#'@param input_dir character sting of path to directory with original 1-sec
#' epoch accelerometer files
#'
#'@param file_pattern charcter sting of naming convention for identifying all
#' original accelerometer files (equivalent to 'pattern' argument of
#' list.files() function)
#'
#'@param output_dir charcter sting of path to directory where all the new
#' 60-sec epoch 'PrepedData' files will be exported
#'
#'@param subjectID_length numeric value of number of characters comprising the
#' unique subject identifier in the names of the original 1-sec accelerometer
#' files (e.g., if naming convention is "data_01" then set to 2, if naming
#' convention is "XXXX_data" then set to 4)
#'
#'@param subjectID_start numeric value of the character in the names of the
#' original 1-sec accelerometer files where the unique subject identifier
#' starts (e.g., if naming convention is "data_01" then set to 6, if naming
#' convention is "XXXX_data" then set to 1)
#'
#'@return writes a CSV file ('PrepedData_ID.csv') in output_dir for each
#' original 1-sec epoch accelerometer file in the input_dir. Each new file is
#' in 60-sec epchs and contains the following new columns: TimeStamp (POSIXlt
#' object combining date and time), days (numeric value indicating which day of
#' monitoring each epoch is in, changing at each occurance of midnight),
#' wearing (character string of "w" when epoch is considered wear time and
#' "nw" for nonwear), and weekday (character string indicating day of the week)

#'@export

#'
#' @examples
#'  acceldata_prep(input_dir = "./Data", file_pattern = "*1secAGdata.csv",
#'                 output_dir = "./Outputs", subjectID_length = 2,
#'                 subjectID_start = 1)

acceldata_prep <- function(input_dir, file_pattern, output_dir,
                           subjectID_length = 2, subjectID_start = 1) {
  files_1sec <- list.files(input_dir, pattern = file_pattern)

  for(i in 1:length(files_1sec)) {
    current_file <- paste0("./Data/", files_1sec[i])
    acceldata_1sec <- read.csv(current_file, stringsAsFactors = FALSE, skip = 1)

    # Create timestamp
    acceldata_1sec$TimeStamp <- as.POSIXlt(paste(acceldata_1sec$date, 
                                                 acceldata_1sec$epoch),
                                           format="%m/%d/%Y %I:%M:%S %p")

    # Reintegrate 1-sec to 60-sec epoch
    acceldata_reint <- dataCollapser(dataset = acceldata_1sec ,
                                     TS = "TimeStamp", by = 60)

    # Add day variable
    acceldata_days <- markingTime(dataset = acceldata_reint,
                                  timestamp = "TimeStamp",
                                  startTime = "00:00:00", tz = "UTC")
    # Classify weartime
    acceldata_final <- wearingMarking(acceldata_days, frame = 90, 
                                      perMinuteCts = 1, TS = "TimeStamp", 
                                      cts = "axis1", streamFrame = NULL,
                                      allowanceFrame = 2, newcolname = "wearing",
                                      getMinuteMarking = FALSE, dayStart = "00:00:00",
                                      tz = "UTC")
    write.csv(acceldata_final,
              paste0(output_dir,"/PrepedData_",
                     substr(files_1sec[i], start = subjectID_start,
                            stop = (subjectID_start+subjectID_length-1)),
                     ".csv"),
              row.names = FALSE)
    print(paste("PrepedData file written in", output_dir, "for subject",
                substr(files_1sec[i], start = subjectID_start,
                       stop = (subjectID_start+subjectID_length-1))))
  }
}



