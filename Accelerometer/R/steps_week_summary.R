
#' Classify physical activity intensity from data frames in output folder
#'
#' @param acceldata_PAI 1-minute epoch accelerometer data with sleep time
#' filtered out, a time stamp, and wear time classified (i.e., output from
#' sleep_filter function)
#'
#' @return returns three objects, each consisting of a dataframe: "steps_byday"
#' (steps/day for each day), "steps_byweekday_end" (average steps/day for
#' weekdays and for weekends), and "steps_weekavg" (average steps/day for the
#' entire week)
#' @export
#'
#' @examples
#' steps_week_summary(myPAdata)

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
      "weekend", "weekday"
    )) %>%
    group_by(week_day_end) %>%
    summarise(steps = sum(steps) / n_distinct(weekday))

  # Step counts averaged over the entire week #
  steps_weekavg <<- steps_byday %>%
    summarise(steps = sum(steps) / n())
}
