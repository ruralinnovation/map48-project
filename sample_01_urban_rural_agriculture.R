library(dplyr)
library(ggplot2)

# Goal: compare the pct of the rural and urban population employed
# in agriculture, manufacturing, and professional services

# Read in sample data
map48_sample_data <- readr::read_csv("data/population_sample_data.csv")

# Select the columns we want:
#
# tract-level geoid,
# Core-based statistical areas rural definition,
# population employed in agriculture, forestry, fishing, and hunting,
# population employed in manufacturing, and
# population employed in professional and technical services
# total civilian employed population, 16 years and over
employment_by_industry <- map48_sample_data %>%
  dplyr::select(
    geoid,
    rural_cbsa_2020,
    estimate_agriculture_2019,
    estimate_manufacturing_2019,
    estimate_professional_services_2019,
    estimate_pop_civilian_employed_16_years_over_2019
  ) %>%
  # Remove unclassified tracts
  dplyr::filter(!is.na(rural_cbsa_2020))

# Calculate percentages for urban and rural
rural_urban_employment_by_industry <- employment_by_industry %>%
  dplyr::group_by(rural_cbsa_2020) %>%
  dplyr::summarise(
    tot_pop_civilian_employed_16_years_over_2019 = sum(estimate_pop_civilian_employed_16_years_over_2019, na.rm = TRUE),
    pct_agriculture_2019 = sum(estimate_agriculture_2019, na.rm = TRUE)/tot_pop_civilian_employed_16_years_over_2019,
    pct_manufacturing_2019 = sum(estimate_manufacturing_2019, na.rm = TRUE)/tot_pop_civilian_employed_16_years_over_2019,
    pct_professional_services_2019 = sum(estimate_professional_services_2019, na.rm = TRUE)/tot_pop_civilian_employed_16_years_over_2019
  ) %>%
  dplyr::rename(
    `Agriculture, forestry, fishing and hunting` = pct_agriculture_2019,
    Manufacturing = pct_manufacturing_2019,
    `Professional and technical services` = pct_professional_services_2019,
  )

# Format the data to be plot-ready

# Rename categories
rural_urban_employment_by_industry$rural_cbsa_2020[rural_urban_employment_by_industry$rural_cbsa_2020 == 1] <- "Rural"
rural_urban_employment_by_industry$rural_cbsa_2020[rural_urban_employment_by_industry$rural_cbsa_2020 == 0] <- "Urban"

# Modify into tidy format
rural_urban_employment_by_industry_tidy <- rural_urban_employment_by_industry %>%
  tidyr::pivot_longer(
    cols = tidyr::all_of(c("Agriculture, forestry, fishing and hunting", "Manufacturing", "Professional and technical services")),
    names_to = "industry",
    values_to = "pct_employment"
  ) %>%
  dplyr::select(rural_cbsa_2020, industry, pct_employment)

# Plot with default ggplot styling
fig <- rural_urban_employment_by_industry_tidy %>%
  ggplot(mapping = aes(industry, y = pct_employment, fill = rural_cbsa_2020)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::percent(pct_employment)), position = position_dodge(width = 1), hjust = -.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)), labels = scales::percent) +
  labs(x = "Industry",
       y = NULL) +
  coord_flip()

fig

