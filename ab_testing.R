library(readr)
library(dplyr)
library(plotly)
ds <- read_csv("E:/PYTHON PROJECTS/A_B TESTING/DATASET1/ab_testing.csv")
summary_stats <- ds %>%
  group_by(Group) %>%
  summarise(
    Avg_Page_Views = mean(`Page Views`, na.rm = TRUE),
    Avg_Time_Spent = mean(`Time Spent`, na.rm = TRUE),
    Conversion_Rate = mean(ifelse(Conversion == "Yes", 1, 0))
  )
print("Summary Statistics:")
print(summary_stats)

conversion_table <- table(ds$Group, ds$Conversion)
prop_test <- prop.test(conversion_table, correct = FALSE)

print("Hypothesis Test for Conversion Rates:")
print(prop_test)

conversion_summary <- ds %>%
  group_by(Group) %>%
  summarise(Conversion_Rate = mean(ifelse(Conversion == "Yes", 1, 0)))

conversion_plot <- plot_ly(
  data = conversion_summary,
  x = ~Group,
  y = ~Conversion_Rate,
  type = "bar",
  text = ~paste0(round(Conversion_Rate * 100, 2), "%"),
  textposition = "outside",
  marker = list(color = c('#1f77b4', '#ff7f0e'))
) %>%
  layout(
    title = "Conversion Rate by Group",
    xaxis = list(title = "Group"),
    yaxis = list(title = "Conversion Rate", tickformat = ".0%"),
    showlegend = FALSE
  )

time_spent_plot <- plot_ly(
  data = ds,
  y = ~`Time Spent`,
  x = ~Group,
  type = "box",
  color = ~Group,
  boxmean = TRUE
) %>%
  layout(
    title = "Time Spent Distribution by Group",
    xaxis = list(title = "Group"),
    yaxis = list(title = "Time Spent (Seconds)")
  )
device_summary <- ds %>%
  group_by(Group, Device) %>%
  summarise(Count = n())

device_plot <- plot_ly(
  data = device_summary,
  x = ~Group,
  y = ~Count,
  color = ~Device,
  type = "bar",
  barmode = "stack"
) %>%
  layout(
    title = "Device Usage by Group",
    xaxis = list(title = "Group"),
    yaxis = list(title = "Count"),
    legend = list(title = list(text = "Device"))
  )
pageview_summary <- ds %>%
  group_by(Group) %>%
  summarise(Average_Page_Views = mean(`Page Views`, na.rm = TRUE))

pageview_plot <- plot_ly(
  data = pageview_summary,
  x = ~Group,
  y = ~Average_Page_Views,
  type = "bar",
  text = ~round(Average_Page_Views, 2),
  textposition = "outside",
  marker = list(color = c('#636efa', '#ef553b'))
) %>%
  layout(
    title = "Average Page Views by Group",
    xaxis = list(title = "Group"),
    yaxis = list(title = "Average Page Views"),
    showlegend = FALSE
  )
location_summary <- ds %>%
  group_by(Location, Group) %>%
  summarise(Conversion_Rate = mean(ifelse(Conversion == "Yes", 1, 0)))

location_plot <- plot_ly(
  data = location_summary,
  x = ~Location,
  y = ~Conversion_Rate,
  color = ~Group,
  type = "bar",
  barmode = "group",
  text = ~paste0(round(Conversion_Rate * 100, 2), "%"),
  textposition = "outside"
) %>%
  layout(
    title = "Conversion Rate by Location",
    xaxis = list(title = "Location", tickangle = 45),
    yaxis = list(title = "Conversion Rate", tickformat = ".0%"),
    legend = list(title = list(text = "Group"))
  )

#print(conversion_plot)
#print(time_spent_plot)
#print(device_plot)
#print(pageview_plot)
#print(location_plot)
