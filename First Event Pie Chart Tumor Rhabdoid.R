##First Event Pie Chart 
library(tidyverse)
library(ggplot2)
library(plotly)

# Check the unique values in the 'First.Event' column
unique(rt_target_2018_pub_clinical_data$First.Event)
# Clean the 'First.Event' column by replacing NA values with "NA" and keeping other categories as is
rt_target_2018_pub_clinical_data <- rt_target_2018_pub_clinical_data %>%
  mutate(First.Event = case_when(
    is.na(First.Event) ~ "NA",    # Replace NA values with "NA"
    First.Event == "Death Without Remission" ~ "Death Without Remission",    # Keep "Death Without Remission"
    TRUE ~ First.Event     # Keep other categories as is
  ))

# Count the distribution of the 'First.Event' categories and sort by count in descending order
first_count <- rt_target_2018_pub_clinical_data %>%
  count(First.Event) %>%
  arrange(desc(n))

# Define custom colors for the pie chart categories:
# Blue for "Relapse", Red for "None", Orange for "Death Without Remission", Gray for "NA"
color_palette <- first_count$First.Event %>%
  map_chr(~ case_when(
    . == "Relapse" ~ "#4682B4",                   # Blue for Relapse
    . == "None" ~ "#B22222",                      # Red for None      
    . == "Death Without Remission" ~ "#FF7F00",   # Orange for Death Without Remission
    . == "NA" ~ "#808080",                        # Gray for NA
    TRUE ~ "#808080"                              # Default gray for others
  ))

# Update legend labels for clarity
legend_labels <- first_count$First.Event %>%
  map_chr(~ case_when(
    . == "NA" ~ "NA",               # Label "NA" as "NA"
    . == "Death Without Remission" ~ "Death Without Remission",     # Label "Death Without Remission" as is
    TRUE ~ .    # Keep other labels as is
  ))

# Create an interactive pie chart using plotly
p_first_interactive <- plot_ly(
  first_count,
  labels = legend_labels,  # Use updated legend labels
  values = ~n,
  type = "pie",
  textinfo = "label+percent",     # Show both label and percentage on the chart
  marker = list(colors = color_palette),    # Apply custom colors
  hoverinfo = "label+value"      # Show label and value on hover
) %>%
  layout(title = "First Event")    # Set the title of the chart

# Display the interactive pie chart
p_first_interactive


