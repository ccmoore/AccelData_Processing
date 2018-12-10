
#'Classify physical activity intensity from data frames in output folder
#'
#'@param data pull in data frame from output folder
#'
#'@param acceldata_PAI function created within 'pai_week_changes' to assign physical activity intensity
#'
#'@param steps_week_summary master function used to create functions 
#' "steps_byday", "steps_byweekday_end", and "steps_weekavg"
#'
#'@param steps_byday function used to create data frame classifying average step counts
#' for each day of the week
#'
#'@param pai_byweekday_end function used to create data frame classifying step counts
#' for the average of all weekdays compared to the average of all weekend days
#'
#'@param pai_weekavg function used to create data frame classifying step counts
#' for the average of the entire week as a whole
#'
#'@return function "steps_week_summary" computes values and associated data frames for variables 
#'  "steps_byday", "steps_byweekday_end", and "steps_weekavg"

#'@export

#'
#' @examples
#' data <- read.csv("./Outputs/FilteredWakeData_CM.csv")
#' 
#'  acceldata_PAI <- markPAI(data = data, cts = "axis1", markingString = "w",
#'  breaks = c(-Inf, 100, 760, 2020, Inf),
#'  labels = c("sedentary", "light", "moderate", "vigorous"))
#'  
#'  steps_week_summary <- function(acceldata_PAI)

# Summarize weekly step count #
steps_week_summary <- function(acceldata_PAI) {
  marked_data <- markPAI(
    data = acceldata_PAI, cts = "axis1", markingString = "w",
    breaks = c(-Inf, 100, 760, 2020, Inf),
    labels = c("sedentary", "light", "moderate", "vigorous")
  )

  # Step counts for each day of the week #
  steps_byday <<- marked_data %>%
    filter(wearing == "w") %>%
    group_by(weekday) %>%
    summarise(steps = sum(steps))

  # Step counts averaged over the weekend and weekdays #
  steps_byweekday_end <<- marked_data %>%
    filter(wearing == "w") %>%
    mutate(week_day_end = ifelse(weekday %in% c("Saturday", "Sunday"),
                                 "weekend", "weekday")) %>%
    group_by(week_day_end) %>%
    summarise(steps = sum(steps) / n_distinct(weekday))
  
  # Step counts averaged over the entire week #
  steps_weekavg <<- steps_byday %>%
    summarise(steps = sum(steps) / n())
}
