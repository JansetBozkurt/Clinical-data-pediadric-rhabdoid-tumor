library(tidyverse)
library(ggplot2)
library(plotly)

# Count the number of NA values in the 'Mutation.Count' column
na_count_mut <- sum(is.na(rt_target_2018_pub_clinical_data$Mutation.Count))

# Round down Mutation.Count values to ensure whole numbers (remove decimal fractions)
rt_target_2018_pub_clinical_data <- rt_target_2018_pub_clinical_data %>%
  mutate(Mutation.Count = floor(Mutation.Count))

# Create a bar chart for Mutation.Count
p_mut <- ggplot(rt_target_2018_pub_clinical_data, aes(x = Mutation.Count)) +
  geom_bar(
    width = 1,  # Set bar width to 1 to remove spacing between bars
    fill = "#69b3a2", 
    color = "black", 
    size = 0.4, 
    alpha = 0.9,
    aes(text = paste0("Mutation Count: ", after_stat(x), "\nNumber of Samples: ", after_stat(count)))
  ) +
  scale_x_continuous(
    breaks = seq(0, max(rt_target_2018_pub_clinical_data$Mutation.Count, na.rm = TRUE), by = 1)
  ) +
  labs(
    title = "Mutation Count",
    x = "Mutation Count",
    y = "Number of Samples"
  ) +
  theme_minimal(base_size = 12) +
  annotate(
    "text",
    x = max(rt_target_2018_pub_clinical_data$Mutation.Count, na.rm = TRUE) - 1,
    y = max(table(rt_target_2018_pub_clinical_data$Mutation.Count), na.rm = TRUE) * 0.9,
    label = paste("NA:", na_count_mut),
    color = "black",
    size = 5,
    hjust = 0
  )

# Convert to interactive plot using plotly
p_mut_interactive <- ggplotly(p_mut, tooltip = "text")

# Display the interactive plot
p_mut_interactive
