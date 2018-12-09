
#' Plot steps vs time (minute)
#'
#' @param pa_data dataframe with one minute epoch dat
#'
#' @return a line graph of steps for each minute over the study period, with blue horizontal lines for each 24hrs
#' @export
#'
#' @examples plot_steps(my_data)
plot_steps <- function(pa_data){

  pa_data$study_min <- as.numeric(row.names(pa_data))

  ggplot(data = pa_data)+
    geom_line(aes(x = study_min, y = steps))+
    geom_vline(aes(xintercept=1440), linetype="dashed",colour="blue",size=0.7)+
    geom_vline(aes(xintercept=2880), linetype="dashed",colour="blue",size=0.7)+
    geom_vline(aes(xintercept=4320), linetype="dashed",colour="blue",size=0.7)+
    ylab("Steps")+
    xlab("Time (minutes)")+
    theme_bw()+
    theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())
}




