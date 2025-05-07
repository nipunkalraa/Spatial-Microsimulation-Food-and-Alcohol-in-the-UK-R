# Spatial-Microsimulation-Food-and-Alcohol-in-the-UK-R
# please check the 'Code_Microsimulation.R' file for full reproducible code. The code has extensive notes about what each line of the ipfp function does.
![R_screenshot](https://github.com/user-attachments/assets/72d90b91-83a6-4a3c-ad57-192f438bbfa6)


This project uses spatial microsimulation techniques to estimate the average daily consumption of various food and drink items, most notably alcohol, across Oxford's Lower Super Output Areas (LSOAs). It generates high-resolution, synthetic population data that is statistically aligned with official demographics, enabling localised public health insights.

# 🔧 Methods & Workflow
Microsimulation Engine: An Iterative Proportional Fitting Procedure (IPFP) matches synthetic individuals to area-level demographic constraints.

# Constraint Variables:
Age-Sex combinations
Ethnicity
Health status
Employment status

# Consumption Outputs:

Estimated daily intake of 13 items, including meats (e.g., pork, beef), plant-based foods (fruits, vegetables), and alcohol.

# 📊 Datasets Used

individuals.csv – Synthetic individuals with consumption attributes

Constraint datasets (age_sex.csv, Ethnicity.csv, Health.csv, Work.csv) – UK 2011 Census-based marginal totals

Geographic Focus: Final simulation and visualization are filtered for Oxford, UK (70+ LSOAs)

# 📦 Packages Used

tidyverse – Data wrangling and transformation

ipfp – Iterative proportional fitting algorithm

sf, tmap – Spatial data manipulation and visualisation

# 📍 Map
Estimated Alcohol Consumption in Oxford (ml/day)

# 🖼️ Map Interpretation
The choropleth map shows spatial variation in average alcohol consumption (ml/day) at the LSOA level in Oxford. Darker regions indicate higher per capita intake.

# ✅ Conclusion
This project demonstrates how spatial microsimulation can turn limited survey data into rich, small-area estimates.
