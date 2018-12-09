
#' Plot Weekday vs Weekend MVPA
#'
#' @param dat dataframe with weekday indicatior and accelerometer daya
#' @param subject unique subject identifier
#'
#' @return a ggplot of weekday vs weekend MVP
#' @export
#'
#' @examples weekend_day_graph(subject1_data, "S1")
mvpa_plot <- function(dat, subject){
  temp_dat <- mutate(dat, weekday_classification = ifelse(weekday %in% c("Saturday", "Sunday"), "Weekday", "Weekend")) %>%  
    group_by(weekday_classification) %>% 
    summarize(N = length(MVPA_min),
              Average_MVPA = mean(MVPA_min),
              sd = sd(MVPA_min),
              se = sd / sqrt(N))
  
  x <- ggplot(temp_dat)+
  geom_col(aes(x = weekday_classification, y = Average_MVPA, fill = weekday_classification))+
  scale_fill_manual(values = c("black", "gray"))+
  xlab("Weekday Classification")+
  ylab("Average MVPA")+
  ggtitle(subject)+
  theme_bw()



x2 <- x + geom_errorbar(data = temp_dat, aes(x = weekday_classification, ymin = Average_MVPA- se, ymax = Average_MVPA + se))
x3 <- x2 + guides(fill = guide_legend(title = "Weekday Classification"))
x3
}



