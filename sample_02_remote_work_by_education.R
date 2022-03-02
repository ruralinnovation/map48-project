library(dplyr)
library(ggplot2)

# Goal: compare how the percent of population working from home has changed
# over time by education level

# Read in sample data
map48_sample_data <- readr::read_csv("data/time_series_sample_data.csv")

