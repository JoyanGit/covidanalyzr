#' A function to visualize a line plot of growth of confirmed corona virus deaths after first 20 deaths are confirmed, grouped by country.
#'
#' This function downloads worldwide case and death data as per the data format of European Centre for Disease Prevention and Control. Based on countries given as input parameter it plots a line graph of how the confirmed deaths are progressing by day.
#'
#' @param countries A vector of all country names that the corona case count need to be plotted.
#' @param m number of confirmed covid cases beyond which the number of days for the plot will begin.
#'
#' @keywords print_death_graph
#' @import dplyr
#' @import ggplot2
#' @import tidyverse
#' @import gtools
#' @import ggthemes
#' @export
#' @examples
#' print_death_grap(c("India", "United"States"of_Ameria"), 200)
print_death_graph <- function(countries, m=0) {
    covid_world_data_M <- add_case_death_counts(0,m)
    covid_world_data_M %>% filter(country %in% countries & day_counter > 0) %>% ggplot(aes(death_day_m_counter,
        cumulative_deaths, group = country, color = country)) + geom_line(size = 2) + theme_economist() +
        xlab(paste( "Days Since ", m, " Death(s) Reported")) + ylab("Deaths")
}
