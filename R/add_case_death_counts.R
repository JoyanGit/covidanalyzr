#' A function to calculate the death case counts and number of days since a
#' specific number of cases or deaths are reported.
#'
#' This function downloads the latest corona wordwide case and death data as per
#' the data format of European Centre for Disease Prevention and Control. It
#' calculates the number of cumulative cases, cumulative deaths and number of
#' days since the first case, nth case, first death and mth death are reported
#' for each country and add them as additional columns in data frame.
#' @keywords add_case_death_counts
#' @param case_count_n Number of cases from which it starts a counter of number
#'   of days passed.
#' @param death_count_m Number of deaths from which it starts a counter of
#'   number of days passed.
#' @return Returns the data frame with daily data of cases and deaths for each
#'   country.
#' @export
#' @import dplyr
#' @import ggplot2
#' @import tidyverse
#' @import gtools
#' @import ggthemes

#' @examples
#' covid_world_data_today <- add_case_death_counts(200,20)

add_case_death_counts <- function(case_count_n=0, death_count_m=0) {

    covid_world_data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "",
        fileEncoding = "UTF-8-BOM")
    colnames(covid_world_data) [1:10] <- c("date", "day", "month", "year", "cases", "deaths", "country", "geoid",
        "counttry_code", "population_2018")
    covid_world_data$date <- as.Date(covid_world_data$date, "%d/%m/%Y")
    covid_world_data$cases <- as.numeric(covid_world_data$cases)
    covid_world_data$deaths <- as.numeric(covid_world_data$deaths)
    covid_world_data$country <- as.character(covid_world_data$country)
    covid_world_data <- covid_world_data %>% arrange(country, date)

    prev_country <- covid_world_data[1, "country"]

    cumulative_cases <- 0
    cumulative_deaths <- 0

    day_counter <- 0
    day_n_counter <- 0

    death_day_counter <- 0
    death_day_m_counter <- 0

    first_day <- FALSE
    first_death_day <- FALSE

    for (i in 1:nrow(covid_world_data)) {
        if (prev_country != covid_world_data[i, "country"]) {

            cumulative_cases <- 0
            cumulative_deaths <- 0

            day_counter <- 0
            day_n_counter <- 0

            death_day_counter <- 0
            death_day_m_counter <- 0


            first_day <- FALSE
            first_death_day <- FALSE


        }

        if (covid_world_data[i, "cases"] > 0 & !first_day) {
            first_day <- TRUE
        }

        if (first_day == TRUE) {
            day_counter <- day_counter + 1
            cumulative_cases <- cumulative_cases + covid_world_data[i, "cases"]
            if (cumulative_cases >= case_count_n) {
                day_n_counter <- day_n_counter + 1
            }
        }

        if (covid_world_data[i, "deaths"] > 0 & !first_death_day) {
            first_death_day <- TRUE
        }

        if (first_death_day == TRUE) {
            death_day_counter <- death_day_counter + 1
            cumulative_deaths <- cumulative_deaths + covid_world_data[i, "deaths"]
            if (cumulative_deaths >= death_count_m) {
                death_day_m_counter <- death_day_m_counter + 1
            }
        }

        covid_world_data[i, "day_counter"] <- day_counter
        covid_world_data[i, "day_n_counter"] <- day_n_counter
        covid_world_data[i, "cumulative_cases"] <- cumulative_cases
        covid_world_data[i, "death_day_counter"] <- death_day_counter
        covid_world_data[i, "death_day_m_counter"] <- death_day_m_counter
        covid_world_data[i, "cumulative_deaths"] <- cumulative_deaths

        prev_country <- covid_world_data[i, "country"]
    }

    return(covid_world_data)
}
