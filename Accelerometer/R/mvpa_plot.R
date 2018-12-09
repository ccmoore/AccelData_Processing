
#' Plot Weekday vs Weekend MVPA
#'
#' @param a dataframe. x= weekday_classification, y = Average_MVPA
#' @param subject id
#'
#' @return a ggplot
#' @export
#'
#' @examples weekend_day_graph(subject1_data, S1)
mvpa_plot <- function(dat, subject){
  ggplot(dat)+
    geom_col(aes(x = weekday_classification, y = Average_MVPA, fill = weekday_classification))+
    scale_fill_manual(values = c("black", "gray"))+
    geom_errorbar(aes(x = weekday_classification, ymin = Average_MVPA - se, ymax = Average_MVPA + se))+
    guides(fill = guide_legend(title = "Weekday Classification"))+
    xlab("Weekday Classification")+
    ylab("Average MVPA")+
    ggtitle(subject)+
    theme_bw()
}



