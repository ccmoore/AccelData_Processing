#' Data arrangement for weekday vs weekend MVPA
#'
#' @param data frame
#'
#' @return summary data frame of weekend/weeday MVPA including N, Average_MVPA, sd, se
#' @export
#'
#' @examples weekday_weekend_dat_arrange(my_data)
mvpa_plot_helper <- function(dat){
  mutate(dat, weekday_classification = ifelse(Weekday == "Monday" | Weekday == "Tuesday" | Weekday == "Wednesday"| Weekday == "Thursday"| Weekday == "Friday", "Weekday", "Weekend")) %>%  
    group_by(weekday_classification) %>% 
    summarize(N = length(MVPA_min),
              Average_MVPA = mean(MVPA_min),
              sd = sd(MVPA_min),
              se = sd / sqrt(N))
}

