
#' Plot average physical activity minutes/day and steps/day for weekday vs weekend
#'
#'@param pai_weekdayend dataframe or tible with summary of average minutes/day
#' at physical activity intensities for weekdays vs the weekend
#'
#'@param steps_weekdayend dataframe or tible with summary of average
#' steps/day physical activity intensities for weekdays vs the weekend
#'
#'@param subject unique subject identifier
#'
#'@return Two bar plot, one with weekday vs weekend average minutes/day at
#' physical activity intensites and the other with weekday vs weekend average
#' minutes/day
#'@export
#'
#' @examples plot_weekday_vs_end(pai_summary, steps_summary, "S1")

plot_weekday_vs_end <- function(pai_weekdayend, steps_weekdayend, subject) {
  plot_pai_dayend <<- ggplot(pai_weekdayend, aes(x = week_day_end, y = minutes, fill = pai)) +
    geom_bar(stat="identity", position = position_dodge()) +
    xlab("Weekday Classification") +
    ylab("Average Minutes/Day") +
    ggtitle(subject) +
    theme_bw()
  
  plot_steps_dayend <<- ggplot(steps_weekdayend, aes(x = week_day_end, y = steps)) +
    geom_bar(stat="identity", position = position_dodge()) +
    xlab("Weekday Classification") +
    ylab("Average Steps/Day") +
    ggtitle(subject) +
    theme_bw()
}
