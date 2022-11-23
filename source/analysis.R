library(tidyverse)
library(dplyr)
library(ggplot2)
library(cartography)
library(sp)

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

testing_get_juv <- function() {
  juv_jail_pop_states <- incar_data %>% 
    group_by(year, state) %>%
    summarise(juv_total_jail_pop = sum(female_juvenile_jail_pop, 
                                         male_juvenile_jail_pop, na.rm = TRUE))
  return(juv_jail_pop_states)   
}

plot_testing_get_juv <- function() {
  juv_plot <- ggplot(testing_get_juv()) +
    ggtitle("Growth of Juvenile Prison Population by States") +
    geom_line(mapping = aes(x = year, y = juv_total_jail_pop, color = state)) +
    labs(x = "Year", y = "Total Juvenile Jail Population")
  return(juv_plot)
}

# Testing with California!

cali_df <- incar_data %>% 
  filter(state == "CA") %>% 
  select(year, county_name, male_juvenile_jail_pop)


cali_get_juv <- function() {
  cali_juv_jail_pop_states <- incar_data %>% 
  return(cali_juv_jail_pop_states)   
}

plot_cali_get_juv <- function() {
  cali_plot <- ggplot(cali_get_juv()) +
    ggtitle("Growth of Juvenile Prison Population by States") +
    geom_line(mapping = aes(x = year, y = male_juvenile_jail_pop, color = county_name)) +
    labs(x = "Year", y = "Total Juvenile Jail Population")
  return(cali_plot)
}

View(cali_df)
plot_cali_get_juv()

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>

ice_incar <- incar_data %>%
  filter(year == 2016) %>% 
  select(state, total_jail_from_ice)

state_shape <- map_data("state") %>%
  rename(state = region) %>% 
  full_join(ice_incar, by="state", na.rm = TRUE) 
  
ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_jail_from_ice),
    color = "white", 
    size = .1) +
  coord_map() + 
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Total Jail Population from ICE") +
  blank_theme

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )

#----------------------------------------------------------------------------#

## Load data frame ---- 


