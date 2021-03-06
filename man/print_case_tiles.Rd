% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_case_tiles.R
\name{print_case_tiles}
\alias{print_case_tiles}
\title{A function to visualize tiles of how severe is the growth of confirmed corona virus cases for each county from day first 200 cases is confirmed.}
\usage{
print_case_tiles(n = 0)
}
\arguments{
\item{n}{Number of days in the plot will start from the day nth case is reported.}
}
\description{
This function downloads worldwide case and death data as per the data format of European Centre for Disease Prevention and Control. For all countries with population more than 10 million ( as of 2018) and 200 cases reported, it shows tiles for each day with color saturation of tile graded as per the rnage of cases worldwide for any day. It also draws a blue vertical line for 30th daty after 200 cases are reported.
}
\examples{
print_case_tiles(200)
}
\keyword{print_case_tiles}
