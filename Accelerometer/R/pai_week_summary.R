
#'Classify physical activity intensity from data frames in output folder
#'
#'@param data pull in data frame from output folder
#'
#'@param acceldata_PAI use markPAI function from 'PhysicalActivity' package to assign character string
#'    of physical activity intensities within "data"
#'
#'@param intensity_week_summary master function used to create functions "pai_byday", "pai_byweekday_end",
#'    and
#'    "pai_weekavg"
#'
#'@param pai_byday function used to create data frame classifying average physical activity intensity levels
#'    within "sedentary", "light", "moderate", and "vigorous" categories for each day of the week
#'
#'@param pai_byweekday_end function used to create data frame classifying physical activity intensity
#'   as "sedentary, light, moderate, or vigorous" for the average of all weekdays compared to the
#'   average of all weekend days
#'
#'@param pai_weekavg function used to create data frame classifying physical activity intensity
#'   as "sedentary, light, moderate, or vigorous" for the average of the entire week as a whole
#'
#'@return function "intensity_week_summary" computes values and associated data frames for variables
#'  "pai_byday", "pai_byweekday_end", and "pai_weekavg"

#'@export

#'
#' @examples
#' data <- read.csv("./Outputs/FilteredWakeData_CM.csv")
#'
#'  acceldata_PAI <- markPAI(data = data, cts = "axis1", markingString = "w",
#'  breaks = c(-Inf, 100, 760, 2020, Inf),
#'  labels = c("sedentary", "light", "moderate", "vigorous"))
#'
#'  intensity_week_summary <- function(acceldata_PAI)

pai_week_summary <- function(acceldata_PAI) {
  temp_data <- markPAI(data = acceldata_PAI, cts = "axis1", markingString = "w",
                       breaks = c(-Inf, 100, 760, 2020, Inf),
                       labels = c("sedentary", "light", "moderate", "vigorous"))
  pai_byday <<- temp_data %>%
    filter(wearing == "w") %>%
    group_by(weekday, pai) %>%
    summarise(minutes = n())

  pai_byweekday_end <<- temp_data %>%
    filter(wearing == "w") %>%
    mutate(week_day_end = ifelse(weekday %in% c("Saturday", "Sunday"), "weekend", "weekday")) %>%
    group_by(week_day_end, pai) %>%
    summarise(minutes= n()/n_distinct(weekday))

  pai_weekavg <<- pai_byday %>%
    group_by(pai) %>%
    summarise(minutes = sum(minutes)/n_distinct(weekday))
}
