
library(tidyverse)
library(ggplot2)
library(plotly)

# Extract the diagnosis age (in days) from the clinical dataset
ages.hist.days <- rt_target_2018_pub_clinical_data$Diagnosis.Age..days.

# Count the number of missing (NA) values
na_count_days <- sum(is.na(ages.hist.days))

# Check if the extracted data is a data frame (for debugging)
is.data.frame(ages.hist.days)

# Create a histogram of diagnosis ages (in days)
p2 <- ggplot(data.frame(Age = ages.hist.days), aes(x = Age)) +
  geom_histogram(binwidth = bin_width, fill = "skyblue",
                 color = "blue",
                 alpha = 0.7,
                 aes(text = paste0("Range: ",
                                   floor(..xmin..),
                                   " - ", floor(..xmax..), 
                                   "\nCount: ", ..count..))) +
  labs(title = "Diagnosis Age Histogram (Days)",
       x = "Age (Days)",
       y = "Count") +
  theme_minimal() +
  annotate("text", x = max(ages.hist.days, na.rm = TRUE) - 300, y = 15, 
           label = paste("NA count:", na_count_days), color = "red", size = 5)

# Convert the static ggplot2 histogram into an interactive Plotly graph
p2_interactive <- ggplotly(p2, tooltip = "text")

# Display the interactive plot
p2_interactive

