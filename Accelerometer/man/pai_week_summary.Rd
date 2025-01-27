% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pai_week_summary.R
\name{pai_week_summary}
\alias{pai_week_summary}
\title{Classify physical activity intensity from data frames in output folder}
\usage{
pai_week_summary(acceldata_PAI)
}
\arguments{
\item{acceldata_PAI}{1-minute epoch accelerometer data with sleep time
filtered out, a time stamp, and wear time classified (i.e., output from
sleep_filter function)}
}
\value{
returns three objects, each consisting of a dataframe: "pai_byday"
(minutes at each physical activity intensity for each day),
"pai_byweekday_end" (average minutes per day at each intensity for weekdays
and for weekends), and "pai_weekavg" (average minutes per day at each
intensity for the entire week)
}
\description{
Classify physical activity intensity from data frames in output folder
}
\examples{
pai_week_summary(myPAdata)
}
