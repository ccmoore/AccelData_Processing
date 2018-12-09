steps_week_summary <- function(acceldata_PAI) {
  marked_data <- markPAI(
    data = acceldata_PAI, cts = "axis1", markingString = "w",
    breaks = c(-Inf, 100, 760, 2020, Inf),
    labels = c("sedentary", "light", "moderate", "vigorous")
  )

  steps_byday <<- marked_data %>%
    filter(wearing == "w") %>%
    group_by(weekday) %>%
    summarise(steps = sum(steps))

  steps_byweekday_end <<- marked_data %>%
    filter(wearing == "w") %>%
    mutate(week_day_end = ifelse(weekday %in% c("Saturday", "Sunday"),
                                 "weekend", "weekday")) %>%
    group_by(week_day_end) %>%
    summarise(steps = sum(steps) / n_distinct(weekday))

  steps_weekavg <<- steps_byday %>%
    summarise(steps = sum(steps) / n())
  return(c(steps_byday, steps_byweekday_end, steps_weekavg))
}
