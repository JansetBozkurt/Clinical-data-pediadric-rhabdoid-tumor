library(tidyverse)
library(plotly)

# Count the number of cases for each sex including NA as "NA"
sex_counts <- rt_target_2018_pub_clinical_data %>%
  mutate(Sex = ifelse(is.na(Sex), "NA", Sex)) %>%
  count(Sex) %>%
  mutate(percentage = n / sum(n) * 100)

# Define custom colors
sex_colors <- c("Male" = "lightblue",       #Male
                "Female" = "lightpink",     #Female
                "NA" = "lightgray"          #NA
                )

# Create interactive pie chart with plotly
interactive_pie_chart <- plot_ly(
  data = sex_counts,
  labels = ~Sex,
  values = ~n,
  type = "pie",
  textinfo = "label+percent",
  hoverinfo = "label+value+percent",
  marker = list(colors = sex_colors[sex_counts$Sex])
) %>%
  layout(title = "Sex Distribution", legend = list(title = list(text = "Sex")))

# Display the chart
interactive_pie_chart




#or if you need non-interactive.
library(ggplot2)

# Count the number of cases for each sex and calculate percentages
sex_counts <- rt_target_2018_pub_clinical_data %>%
  count(Sex) %>%
  mutate(percentage = n / sum(n) * 100)

# Count number of NA values (optional, for reference)
na_count_Sex <- rt_target_2018_pub_clinical_data %>%
  summarize(na_count = sum(is.na(Sex))) %>%
  pull(na_count)

# Create the non-interactive pie chart
pie_chart <- ggplot(sex_counts, aes(x = "", y = percentage, fill = Sex)) +
  geom_col(width = 1) +  # Use geom_col for stacked bar format (will be converted to pie)
  coord_polar(theta = "y") +     # Convert bar chart to pie chart
  theme_void() +          # Remove all axes and grid elements for a clean look
  scale_fill_manual(values = c("Male" = "lightblue",
                               "Female" = "lightpink",
                               "NA" = "lightgray")      # Custom colors
                    ) +
  labs(title = "Sex Distribution", fill = "Sex") +
  theme(legend.position = "right") +
  geom_text(aes(label = paste(Sex, ": ",
                              n, " (",
                              round(percentage, 1),
                              "%)",
                              sep = "")
                ),
            position = position_stack(vjust = 0.5),
            color = "white")

# Display the chart
print(pie_chart)
