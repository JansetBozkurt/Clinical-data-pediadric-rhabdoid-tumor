
#First, let's install and load the required packages.
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("plotly")

library(tidyverse)  # for data manipulation and visualization
library(ggplot2)    # for creating static plots
library(plotly)     # for making interactive plots

#This is a basic, non-interactive histogram that can be customized as needed.

# The 'Diagnosis.Age' column is selected
ages.hist <- rt_target_2018_pub_clinical_data$Diagnosis.Age
# Missing (NA) values are counted
na_count_years <- sum(is.na(ages.hist))

p1 <- ggplot(data.frame(Age = ages.hist), aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "blue") +
  labs(title = "Diagnosis Age Histogram", x = "Age (Years)", y = "Count") +
  theme_minimal() +
  annotate("text", x = max(ages.hist, na.rm = TRUE) - 2, y = 10, 
           label = paste("NA count:", na_count_years), color = "red", size = 5)
p1
# This code generates a basic histogram of diagnosis ages and annotates the number of missing values (NA) 

# If you need convert the static ggplot to an interactive plotly object just run this code 
interactive_p1 <- ggplotly(p1)
interactive_p1
