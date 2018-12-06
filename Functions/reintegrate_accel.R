reintegrate_accel <- function(data_dir, output_dir, file_pattern,
                              subjectID_length = 2, subjectID_start = 1) {
  files_1sec <- list.files(data_dir, pattern = file_pattern, recursive = T)

  for(j in 1:length(files_1sec)) {
    data_1sec <- read.csv(paste0(data_dir,"/", files_1sec[j]), stringsAsFactors = FALSE, skip = 1)

    date.format <- "%m/%d/%Y %I:%M:%S %p"
    times_1sec <- as.POSIXlt(paste(data_1sec$date, data_1sec$epoch), format=date.format)
    t0s <- which(times_1sec$sec==0)
    t0s <- t0s[-length(t0s)]

    new_rows <- length(t0s)
    new_rows_index <- t0s

    date <- data_1sec$date[new_rows_index]
    epoch <- data_1sec$epoch[new_rows_index]

    newepoch_data <- data.frame(date = date, epoch = epoch,
                                axis1 = rep(NA,new_rows), axis2 = rep(NA,new_rows),
                                axis3 = rep(NA,new_rows), vm = rep(NA,new_rows),
                                steps = rep(NA,new_rows))
    for (i in 1:new_rows){
      if(i+1 <= new_rows){
        newepoch_data$axis1[i] = sum(data_1sec$axis1[new_rows_index[i]:(new_rows_index[i+1]-1)])
        newepoch_data$axis2[i] = sum(data_1sec$axis2[new_rows_index[i]:(new_rows_index[i+1]-1)])
        newepoch_data$axis3[i] = sum(data_1sec$axis3[new_rows_index[i]:(new_rows_index[i+1]-1)])
        newepoch_data$vm[i] = sum(data_1sec$vm[new_rows_index[i]:(new_rows_index[i+1]-1)])
        newepoch_data$steps[i] = sum(data_1sec$steps[new_rows_index[i]:(new_rows_index[i+1]-1)])
      } else {
        newepoch_data$axis1[i] = sum(data_1sec$axis1[new_rows_index[i]:(new_rows_index[i]+epoch-1)])
        newepoch_data$axis2[i] = sum(data_1sec$axis2[new_rows_index[i]:(new_rows_index[i]+epoch-1)])
        newepoch_data$axis3[i] = sum(data_1sec$axis3[new_rows_index[i]:(new_rows_index[i]+epoch-1)])
        newepoch_data$steps[i] = sum(data_1sec$steps[new_rows_index[i]:(new_rows_index[i]+epoch-1)])
      }

    }
    subject <- substr(files_1sec[j], start = subjectID_start, stop = (subjectID_start+subjectID_length-1))
    write.csv(newepoch_data, paste0(output_dir, "/", subject, "_60sec_data.csv"), row.names = FALSE)

    print(paste("File for", subnum, "is written"))
  }
}
