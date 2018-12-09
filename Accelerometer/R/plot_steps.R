
#' Plot steps per minute
#'
#' @param a dataframe. x = study_min, y = steps
#'
#' @returng a ggplot
#' @export
#'
#' @examples plot_steps(my_data)
plot_steps <- function(pa.dat){

  pa.dat$study_min <- as.numeric(row.names(pa.dat))


  ggplot(data = pa.dat)+
    geom_point(aes(x = study_min, y = steps))+
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




