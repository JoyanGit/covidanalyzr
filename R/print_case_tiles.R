#' A function to visualize tiles of how severe is the growth of confirmed corona virus cases for each county from day first 200 cases is confirmed.
#'
#' This function downloads worldwide case and death data as per the data format of European Centre for Disease Prevention and Control. For all countries with population more than 10 million ( as of 2018) and 200 cases reported, it shows tiles for each day with color saturation of tile graded as per the rnage of cases worldwide for any day. It also draws a blue vertical line for 30th daty after 200 cases are reported.
#'
#' @param n Number of days in the plot will start from the day nth case is reported.
#'
#' @keywords print_case_tiles
#' @import dplyr
#' @import ggplot2
#' @import tidyverse
#' @import gtools
#' @import ggthemes
#' @import RColorBrewer

#' @export
#' @examples
#' print_case_tiles(200)
print_case_tiles <- function(n=0) {

    covid_world_data_M <- add_case_death_counts(n,0)
    covid_world_data_M %>% filter(day_n_counter > 0 & population_2018 > 1e+07) %>% ggplot(aes(day_n_counter,
        country, fill = cases+1)) + geom_tile(color = "grey50") + scale_x_continuous(expand = c(0, 0)) +
        scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "log10") + geom_vline(xintercept = 30,
        color = "blue") + xlab(paste( "Days Since ", n, " Case(s) Reported")) + ylab("Country with > 10 M population (2018) ")
}
