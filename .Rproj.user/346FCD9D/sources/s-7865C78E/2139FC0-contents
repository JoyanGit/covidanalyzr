#' A function to visualize a line plot of growth of confirmed corona virus infections after first n cases are confirmed, grouped by country.
#'
#' This function downloads ordwide case and death data as per the data format of European Centre for Disease Prevention and Control. Based on countries given as input parameter it plots a line graph of how the confirmed cases are progressing by day.
#' @param countries A vector of all country names that the corona case count need to be plotted.
#' @param n number of confirmed covid cases beyond which the number of days for the plot will begin.
#'
#' @keywords print_case_graph
#'
#' @import dplyr
#' @import ggplot2
#' @import tidyverse
#' @import gtools
#' @import ggthemes
#' @export
#' @examples
#' print_case_graph(c("India", "United"States"of_Ameria"), 200)

print_case_graph <- function(countries, n = 0 ) {
    covid_world_data_M <- add_case_death_counts(n,0)
    covid_world_data_M %>% filter(country %in% countries & day_counter > 0) %>% ggplot(aes(day_n_counter,
        log10(cumulative_cases), group = country, color = country)) + geom_line(size = 2) + theme_economist() +
        xlab(paste( "Days Since ", n, " Case(s) Reported")) + ylab("Number of Cases - Power of 10")
}
