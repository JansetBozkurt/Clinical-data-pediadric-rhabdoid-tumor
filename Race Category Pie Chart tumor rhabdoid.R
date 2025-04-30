# Race Category Pie Chart
library(tidyverse)
library(ggplot2)
library(plotly)

# Replace NA and preserve 'Unknown' values in Race.Category
rt_target_2018_pub_clinical_data <- rt_target_2018_pub_clinical_data %>%
  mutate(Race.Category = case_when(
    is.na(Race.Category) ~ "NA",
    Race.Category == "Unknown" ~ "Unknown",
    TRUE ~ Race.Category
  ))

# Count the distribution of Race.Category
race_count <- rt_target_2018_pub_clinical_data %>%
  count(Race.Category) %>%
  arrange(desc(n))

# Define custom colors:
# Blue for White, Red for Black or African American,
# Dark Gray for Other, Gray for NA and Unknown
color_palette <- race_count$Race.Category %>%
  map_chr(~ case_when(
    . == "White" ~ "#4682B4",                     # Blue
    . == "Black or African American" ~ "#B22222", # Red
    . == "Other" ~ "#383E42",                     # Dark gray
    . == "Unknown" ~ "#808080",                   # Gray (Unknown)
    . == "NA" ~ "#808080",                        # Gray (NA)
    TRUE ~ "#808080"                              # Default Gray for others
  ))

# Update legend labels
legend_labels <- race_count$Race.Category %>%
  map_chr(~ case_when(
    . == "NA" ~ "NA",               
    . == "Unknown" ~ "Unknown",     
    TRUE ~ .
  ))

# Create interactive pie chart
p_race_interactive <- plot_ly(
  race_count,
  labels = legend_labels,  
  values = ~n,
  type = "pie",
  textinfo = "label+percent",
  marker = list(colors = color_palette),
  hoverinfo = "label+value"
) %>%
  layout(title = "Race Category")

# Display the plot
p_race_interactive
