# Ethnicity Category
# Load required libraries
library(tidyverse)
library(plotly)

# Combine NA and "Unknown" as separate categories, but color them the same
rt_target_2018_pub_clinical_data <- rt_target_2018_pub_clinical_data %>%
  mutate(Ethnicity.Category = case_when(
    is.na(Ethnicity.Category) ~ "NA",
    Ethnicity.Category == "Unknown" ~ "Unknown",
    TRUE ~ Ethnicity.Category
  ))

# Count frequency of each ethnicity category
ethnicity_count <- rt_target_2018_pub_clinical_data %>%
  count(Ethnicity.Category) %>%
  arrange(desc(n))

# Define custom color palette
color_palette <- ethnicity_count$Ethnicity.Category %>%
  map_chr(~ case_when(
    . == "Not Hispanic or Latino" ~ "#4682B4",  # Blue
    . == "Hispanic or Latino" ~ "#B22222",      # Red
    . == "Unknown" ~ "#808080",                 # Gray (Unknown)
    . == "NA" ~ "#808080",                      # Gray (NA)
    TRUE ~ "#808080"                            # Default: Gray
  ))

# Update legend labels for clarity
legend_labels <- ethnicity_count$Ethnicity.Category %>%
  map_chr(~ case_when(
    . == "NA" ~ "NA",               
    . == "Unknown" ~ "Unknown",    
    TRUE ~ .
  ))

# Create interactive pie chart
p_ethnicity_interactive <- plot_ly(
  ethnicity_count,
  labels = legend_labels,  
  values = ~n,
  type = "pie",
  textinfo = "label+percent",
  marker = list(colors = color_palette),
  hoverinfo = "label+value"
) %>%
  layout(title = "Ethnicity Category Distribution")

# Display the chart
p_ethnicity_interactive
