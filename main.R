# 1. Load Data
industrial_production = read.csv("fredgraph_production")
industrial_production$observation_date <- as.Date(industrial_production$observation_date, format = "%Y-%m-%d") #converting date into math object

library(ggplot2)
library(dplyr)
#time series graph for production volumes
ggplot(industrial_production, aes(x = observation_date, y = IPG326S)) +
  geom_line() +
  scale_x_date(
    date_breaks = '3 years',
    date_labels = '%Y'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#time series graph for production volumes growth rates
ggplot(industrial_production, aes(x = observation_date, y = IPG326S_CH1)) +
  geom_line() +
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#median, mean, max, min for growth rates
print(median(industrial_production$IPG326S_CH1, na.rm = TRUE))
print(mean(industrial_production$IPG326S_CH1, na.rm = TRUE))
max_growth <- industrial_production[which.max(industrial_production$IPG326S_CH1), ]
min_growth <- industrial_production[which.min(industrial_production$IPG326S_CH1), ]
print(max_growth)
print(min_growth)

#standard deviation
sd_industrialproduction_volumes <- sd(industrial_production$IPG326S, na.rm = TRUE)
print(sd_industrialproduction_volumes)
sd_industrialproduction_growth <- sd(industrial_production$IPG326S_CH1, na.rm = TRUE)
print(sd_industrialproduction_growth)

#boxplot for growth rates
ggplot(data = industrial_production, aes(x = IPG326S_CH1)) +
  geom_boxplot()

#finding outliers using zscore
industrial_production$growth_zscore <- (industrial_production$IPG326S_CH1 - mean(industrial_production$IPG326S_CH1, na.rm = TRUE))/sd(industrial_production$IPG326S_CH1, na.rm=TRUE)
#creating additional column for zscores in order to better assign outliers to observations
outliers_z <- industrial_production[abs(industrial_production$growth_zscore) > 3, ]
print(outliers_z)
