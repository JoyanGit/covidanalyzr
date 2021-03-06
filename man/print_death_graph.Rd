% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_death_graph.R
\name{print_death_graph}
\alias{print_death_graph}
\title{A function to visualize a line plot of growth of confirmed corona virus deaths after first 20 deaths are confirmed, grouped by country.}
\usage{
print_death_graph(countries, m = 0)
}
\arguments{
\item{countries}{A vector of all country names that the corona case count need to be plotted.}

\item{m}{number of confirmed covid cases beyond which the number of days for the plot will begin.}
}
\description{
This function downloads worldwide case and death data as per the data format of European Centre for Disease Prevention and Control. Based on countries given as input parameter it plots a line graph of how the confirmed deaths are progressing by day.
}
\examples{
print_death_grap(c("India", "United"States"of_Ameria"), 200)
}
\keyword{print_death_graph}
