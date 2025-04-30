#Overall Survival Status Distribution 

library(tidyverse) 
library(plotly)

# Assign dataset
rt_data >- rt_target_2018_pub_clinical_data
str(rt_data)

# Prepare the survival status counts
os_count <- rt_data %>%
  mutate(Overall.Survival.Status = case_when(
    is.na(Overall.Survival.Status) ~ "NA",
    Overall.Survival.Status == "0" ~ "LIVING",
    Overall.Survival.Status == "1" ~ "DECEASED",
    TRUE ~ as.character(Overall.Survival.Status)
  )) %>%
  count(Overall.Survival.Status)

# Define color palette (Green: Living, Red: Deceased, Gray: NA)
os_colors <- c("LIVING" = "#009E73",
               "DECEASED" = "#D55E00",
               "NA" = "#808080")

# Create interactive pie chart
p_os_interactive <- plot_ly(
  os_count,
  labels = ~Overall.Survival.Status,
  values = ~n,
  type = "pie",
  textinfo = "label+percent",
  marker = list(colors = os_colors),
  hoverinfo = "label+value"
) %>%
  layout(title = "Overall Survival Status Distribution")

# Display the plot
p_os_interactive
