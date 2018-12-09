### Sort data by physical activity intensity ###
data <- read.csv("./Outputs/FilteredWakeData_CM.csv")
acceldata_PAI <- markPAI(data = data, cts = "axis1", markingString = "w",
                         breaks = c(-Inf, 100, 760, 2020, Inf),
                         labels = c("sedentary", "light", "moderate", "vigorous"))

### Sort data by physical activity intensity ###
steps_week_summary <- function(acceldata_PAI) return(steps_byday, steps_byweekday_end, steps_weekavg){
  steps_byday <- acceldata_PAI %>%
    filter(wearing == "w") %>% 
    group_by(weekday) %>% 
    summarise(steps = sum(steps))
  
  steps_byweekday_end <- acceldata_PAI %>%
    filter(wearing == "w") %>%
    mutate(week_day_end = ifelse(weekday %in% c("Saturday", "Sunday"), "weekend", "weekday")) %>% 
    group_by(week_day_end) %>%
    summarise(steps = sum(steps)/n_distinct(weekday))
  
  steps_weekavg <- steps_byday %>% 
    summarise(steps = sum(steps)/n())
}


            