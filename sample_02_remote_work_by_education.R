library(dplyr)
library(ggplot2)

# Goal: compare how the percent of population working from home has changed
# over time by education level

# Read in sample data
map48_sample_data <- readr::read_csv("data/time_series_sample_data.csv", col_types = list(readr::col_date(format = "%m/%d/%y")))

# Format for chart consumption
remote_work_by_ed_level <- map48_sample_data %>%
  tidyr::pivot_longer(
    cols = !date,
    names_to = "education_level",
    values_to = "pct_working_remotely"
  )

# ggplot with default styling
fig <- remote_work_by_ed_level %>%
  ggplot(aes(date, pct_working_remotely, group = education_level, color = education_level)) +
  geom_line() +
  xlab("")

fig
