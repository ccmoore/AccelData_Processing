
#' Classify physical activity intensity from data frames in output folder
#'
#' @param acceldata_PAI 1-minute epoch accelerometer data with sleep time
#' filtered out, a time stamp, and wear time classified (i.e., output from
#' sleep_filter function)
#'
#' @return returns three objects, each consisting of a dataframe: "pai_byday"
#' (minutes at each physical activity intensity for each day),
#' "pai_byweekday_end" (average minutes per day at each intensity for weekdays
#' and for weekends), and "pai_weekavg" (average minutes per day at each
#' intensity for the entire week)
#' @export
#'
#' @examples
#' pai_week_summary(myPAdata)

pai_week_summary <- function(acceldata_PAI) {
  temp_data <- markPAI(
    data = acceldata_PAI, cts = "axis1", markingString = "w",
    breaks = c(-Inf, 100, 760, 2020, Inf),
    labels = c("sedentary", "light", "moderate", "vigorous")
  )
  pai_byday <<- temp_data %>%
    filter(wearing == "w") %>%
    group_by(weekday, pai) %>%
    summarise(minutes = n())

  pai_byweekday_end <<- temp_data %>%
    filter(wearing == "w") %>%
    mutate(week_day_end = ifelse(weekday %in% c("Saturday", "Sunday"), "weekend", "weekday")) %>%
    group_by(week_day_end, pai) %>%
    summarise(minutes = n() / n_distinct(weekday))

  pai_weekavg <<- pai_byday %>%
    group_by(pai) %>%
    summarise(minutes = sum(minutes) / n_distinct(weekday))
}
