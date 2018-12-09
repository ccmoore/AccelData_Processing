
#' Plot Weekday vs Weekend MVPA
#'
#' @param pai_weekdayend dataframe or tible with summary of average minutes/day
#'  at physical activity intensities for weekdays vs the weekend
#'
#'  @param steps_weekdayend dataframe or tible with summary of average
#'  steps/day physical activity intensities for weekdays vs the weekend
#'
#'  @param subject unique subject identifier
#'
#' @return Two bar plot, one with weekday vs weekend average minutes/day at
#' physical activity intensites and the other with weekday vs weekend average
#' minutes/day
#' @export
#'
#' @examples plot_weekDay_vs_End(pai_summary, steps_summary, "S1")

plot_weekDay_vs_End <- function(pai_weekdayend, steps_weekdayend, subject) {
  ggplot(pai_weekdayend, aes(x = wee_day_end, y = minutes, fill = pai)) +
    geom_bar() +
    xlab("Weekday Classification") +
    ylab("Average Minutes/Day") +
    ggtitle(subject) +
    theme_bw()

  ggplot(steps_weekdayend, aes(x = week_day_end, y = steps)) +
    geom_bar() +
    xlab("Weekday Classification") +
    ylab("Average Steps/Day") +
    ggtitle(subject) +
    theme_bw()
}
