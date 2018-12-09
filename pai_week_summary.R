### Classify physical activity intensity ###
data <- read.csv("./Outputs/FilteredWakeData_CM.csv")
acceldata_PAI <- markPAI(data = data, cts = "axis1", markingString = "w",
                         breaks = c(-Inf, 100, 760, 2020, Inf),
                         labels = c("sedentary", "light", "moderate", "vigorous"))

### Summarize physical activity intensity by week ###
intensity_week_summary <- function(acceldata_PAI) return(pai_byday, pai_byweekday_end, pai_weekavg) {
  pai_byday <- acceldata_PAI %>%
    filter(wearing == "w") %>% 
    group_by(weekday, pai) %>% 
    summarise(minutes = n())
  
  pai_byweekday_end <- acceldata_PAI %>%
    filter(wearing == "w") %>% 
    mutate(week_day_end = ifelse(weekday %in% c("Saturday", "Sunday"), "weekend", "weekday")) %>% 
    group_by(week_day_end, pai) %>% 
    summarise(minutes= n()/n_distinct(weekday))
  
  pai_weekavg <- pai_byday %>%
    group_by(pai) %>% 
    summarise(minutes = sum(minutes)/n_distinct(weekday))
}


