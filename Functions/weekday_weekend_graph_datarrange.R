#' Data arrangement for weekday vs weekend MVPA
#'
#' @param dat 
#'
#' @return a data frame
#' @export
#'
#' @examples
weeday_weekend_dat_arrange <- function(dat){
  mutate(dat, weekday_classification = ifelse(Weekday == "Monday" | Weekday == "Tuesday" | Weekday == "Wednesday"| Weekday == "Thursday"| Weekday == "Friday", "Weekday", "Weekend")) %>%  
    group_by(weekday_classification) %>% 
    summarize(N = length(MVPA_min),
              Average_MVPA = mean(MVPA_min),
              sd = sd(MVPA_min),
              se = sd / sqrt(N))
}