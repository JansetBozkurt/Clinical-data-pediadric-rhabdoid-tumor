
library(tidyverse)
library(ggplot2)
library(plotly)

# Count the number of NA values in the 'Diagnosis.Age..days.' column
na_count_days <- rt_target_2018_pub_clinical_data %>%
  summarize(na_count = sum(is.na(Diagnosis.Age..days.))) %>%
  pull(na_count)

# Create a histogram for diagnosis age (in days), excluding NA values
p2 <- rt_target_2018_pub_clinical_data %>%
  drop_na(Diagnosis.Age..days.) %>%
  ggplot(aes(x = Diagnosis.Age..days.)) +
  geom_histogram(binwidth = 150,        # Bin width set to 150 days
                 fill = "#99CCFF",      # Light blue fill
                 color = "#6699CC",     # Blue border
                 alpha = 0.7,           # Transparency
                 size = 0.6             # Border thickness
                 ) +
  labs(
    title = "Diagnosis Age Histogram (Days)", 
    x = "Age (Days)", 
    y = "Count"
    ) +
  theme_minimal() +
  annotate(
    "text",
           x = max(rt_target_2018_pub_clinical_data$Diagnosis.Age..days., na.rm = TRUE) - 300,     # Position text near max value
           y = 15,      # Vertical position
           label = paste("NA count:", na_count_days),    # Show number of NA values
           color = "black",
           size = 5)

# Convert to interactive plot using plotly, showing tooltips for x (age) and count
p2_interactive <- ggplotly(p2, tooltip = c("x", "count"))

# Display the interactive histogram
p2_interactive 
