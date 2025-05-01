
#Presentation (12 pts): highlight your ability to describe and visualize data for a lay audience. The slide deck must include a topical motivation, description of the data, and two slides of analysis. You must create the slides in R markdown. The presentation must be 7-10 minutes in length.

# 1) Title slide with topic and author info]

# - 
# 2) Overview: outline the motivation and central findings
# 3) Data: describe the data you chose for the project
# 4) Analysis 1: Show and describe the data on a key measure
# 5) Analysis 2: Dig deeper. Show and describe a sub-group comparison or assessment of an empirical        relationship.

#Analysis slides should include a visual representation of the data (e.g., bar chart, box plot, whatever) and basic information. The second analysis slide should feature a at least one visual of some empirical relationship or group comparison. Elements of appropriate significance tests should also appear here. 

# Was the central topic/motivation clear?
# Was the description of the data thorough and understandable?
# How effective are the visuals? Think about titles, use of color, sharpness of image, text size, etc
# What do you think of the overall theme of the slides and visuals?

# -------------------------------------

library(readr)
library(dplyr)
library(tidyverse)
library(scales)
library(lfe)
library(modelsummary)
library(gt)
library(data.table)
library(scales)
greenspace_data_share <- read_csv("dataverse_files/greenspace_data_share.csv")
View(greenspace_data_share)

# Cleaning the Dataset
# - make new dataset: Isolate the variables: Country, HDI_level, peak_NDVI_2021. Then group by country, get rid of NA's 

library(dplyr)
library(ggplot2)

# Create summarized dataset by country

ndvi_summary <- greenspace_data_share %>%
  drop_na(peak_NDVI_2021) %>%
  group_by(Country) %>%
  summarise(avg_peak_ndvi = mean(peak_NDVI_2021, na.rm = TRUE)) %>%
  arrange(desc(avg_peak_ndvi))

# Plot top 10 countries by peak NDVI
top10 <- ndvi_summary %>% slice_max(avg_peak_ndvi, n = 10)

ggplot(top10, aes(x = reorder(Country, avg_peak_ndvi), y = avg_peak_ndvi)) +
  geom_col(fill = "#2ca25f") +
  coord_flip() +
  labs(title = "Top 10 Countries by Urban Greenspace (Peak NDVI, 2021)",
       x = "Country",
       y = "Average Peak NDVI") +
  theme_minimal()

#Show Data on a Key Measure
# - scatter plot of countries and NDVI

# Digging Deeper
# Show the relationship bwteen NDVI rate and HDI level (Categorical) visually and with a significnace test

# Group by HDI level
ndvi_by_hdi <- greenspace_data_share %>%
  drop_na(peak_NDVI_2021, HDI_level) %>%
  group_by(HDI_level) %>%
  summarise(avg_peak_ndvi = mean(peak_NDVI_2021))

# Boxplot to show distribution
ggplot(greenspace_data_share, aes(x = HDI_level, y = peak_NDVI_2021, fill = HDI_level)) +
  geom_boxplot() +
  labs(title = "Urban Greenspace by HDI Level",
       x = "HDI Level",
       y = "Peak NDVI (2021)") +
  theme_minimal()

# Kruskal-Wallis test (non-parametric)
kruskal.test(peak_NDVI_2021 ~ HDI_level, data = greenspace_data_share)

# Checking

unique(greenspace_data_share$Major_Geo_Region)

