library(tidyverse)
library(dplyr)
library(ggplot2)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>

incar_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

summary_data_max_county <- list()
summary_data_max_county$max_jail_pop <- incar_data %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  select(county_name)

summary_data_max_region<- list()
summary_data_max_region$max_jail_pop <- incar_data %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  select(region)

summary_data_max_juv<- list()
summary_data_max_juv$female_juvenile_jail_pop <- incar_data %>%
  filter(female_juvenile_jail_pop == max(female_juvenile_jail_pop, na.rm = TRUE)) %>%
  select(county_name)

summary_data_max_juv_male<- list()
summary_data_max_juv_male$male_juvenile_jail_pop <- incar_data %>%
  filter(male_juvenile_jail_pop == max(male_juvenile_jail_pop, na.rm = TRUE)) %>%
  select(county_name)


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>

get_year_jail_pop <- function() {
  jail_pop_yearly <- incar_data %>% 
    group_by(year) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(jail_pop_yearly)   
}

# This function ... <todo:  update comment>

plot_jail_pop_for_us <- function()  {
  testing_plot <- ggplot(get_year_jail_pop()) +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)") +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks()) +
    labs(x = "Year", y = "Total Jail Population")
  return(testing_plot)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>

get_jail_pop_by_states <- function(states) {
  jail_pop_states <- incar_data %>% 
    group_by(year, state) %>%
    filter(state %in% states) %>% 
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(jail_pop_states)   
}

plot_jail_pop_by_states <- function(states) {
  state_plot <- ggplot(get_jail_pop_by_states(states)) +
    ggtitle("Growth of Prison Population by States") +
    geom_line(mapping = aes(x = year, y = total_jail_pop, color = state)) +
    labs(x = "Year", y = "Total Jail Population")
  return(state_plot)
}

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>

get_female_juv_jail_pop <- function() {
  female_juvenile_jail_population <- incar_data %>% 
    group_by(state) %>% 
    summarise(female_juvenile_jail_pop = sum(female_juvenile_jail_pop, na.rm = TRUE))
}

get_male_juv_jail_pop <- function() {
  male_juvenile_jail_population <- incar_data %>% 
    group_by(state) %>% 
    summarise(male_juvenile_jail_pop = sum(male_juvenile_jail_pop, na.rm = TRUE))
}

plot_juv_jail <- function() {
  plot_jail_juv_plot <- ggplot(get_male_juv_jail_pop()) +
    ggtitle("Juvenile Jail Population") +
    geom_line(mapping = aes(x = year, y = male_juvenile_jail_pop)) +
    return(plot_jail_juv_plot)   
}

plot_juv_jail()
# This function ... <todo:  update comment>

plot_jail_pop_for_us <- function()  {
  testing_plot <- ggplot(get_year_jail_pop()) +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)") +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks()) +
    labs(x = "Year", y = "Total Jail Population")
  return(testing_plot)   
} 


#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


