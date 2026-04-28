#Load Data for tire industry
industrial_production = read.csv("fredgraph_production_nondurable_goods_tires.csv")
industrial_production$observation_date <- as.Date(industrial_production$observation_date, format = "%Y-%m-%d") #converting date into math object

#loading data for the industry as a whole
industrial_production_totalindex <- read.csv("fred_industrial_production_total_index.csv", header = TRUE, stringsAsFactors = FALSE)
print(industrial_production_totalindex)
median_totalindustry <- median(industrial_production_totalindex$INDPRO_PC1)
mean_totalindustry <- mean(industrial_production_totalindex$INDPRO_PC1)
print(median_totalindustry)
print(mean_totalindustry)
max_growthTI <- industrial_production_totalindex[which.max(industrial_production_totalindex$INDPRO_PC1), ]
min_growthTI <- industrial_production[which.min(industrial_production_totalindex$INDPRO_PC1), ]
print(max_growthTI)
print(min_growthTI)

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

wits_data <- read.csv("WITS-By-HS6Product(By-HS6Product).csv", sep = ";", stringsAsFactors = FALSE)
print(wits_data)
# Clean column names
names(wits_data) <- trimws(names(wits_data))

dfexp_q1 <- quantile(wits_data$Trade.Value.1000USD, probs = 0.25, na.rm = TRUE)
dfexp_q3 <- quantile(wits_data$Trade.Value.1000USD, probs = 0.75, na.rm = TRUE)
IQR = dfexp_q3 - dfexp_q1
observations <- sum(wits_data$TradeFlow == 'Export', na.rm = TRUE)
Freedman_Diaconis_rule = (2*(IQR))/(observations^(1/3))
print(Freedman_Diaconis_rule)
#basic export histogram
ggplot(wits_data,aes(x = Trade.Value.1000USD)) +
  ggtitle('histogram of volumes of exports') +
  labs(x = 'Trade value in 1000 USD') +
  geom_histogram(binwidth = Freedman_Diaconis_rule)
#log export histogram
ggplot(wits_data,aes(x = Trade.Value.1000USD)) +
  ggtitle('log-transformed scale histogram of volume of exports') +
  labs(x = 'Trade value in 1000 USD') +
  scale_x_log10() +
  geom_histogram()
#comparing top_10 exporters
top_10_value <- wits_data %>%
  arrange(desc(`Trade.Value.1000USD`)) %>%
  head(10)
ggplot(top_10_value, aes(x = reorder(Reporter, `Trade.Value.1000USD`), y = `Trade.Value.1000USD`)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(title = 'Top 10 Tire Exporters in 2024',
       x = 'Country',
       y = 'Trade Value in 1000 USD')

#export data in 2020
wits_data2020 <- read.csv("WITS-By-HS6Product(4)(By-HS6Product).csv",sep = ';', stringsAsFactors = FALSE)
names(wits_data2020) <- trimws(names(wits_data2020))
top_10_value2020 <- wits_data2020 %>%
  arrange(desc(`Trade.Value.1000USD`)) %>%
  head(10)
ggplot(top_10_value2020, aes(x = reorder(Reporter, `Trade.Value.1000USD`), y = `Trade.Value.1000USD`)) + 
  geom_bar(stat='identity') +
  theme_minimal() +
  labs(title = 'Top 10 Tire Exporters in 2020',
       x = 'Country',
       y = 'Trade value in 1000 USD')

#export data in 2014
wits_data2014 <- read.csv("WITS-By-HS6Product(3)(By-HS6Product).csv",sep = ';', stringsAsFactors = FALSE)
names(wits_data2014) <- trimws(names(wits_data2014))
top_10_value2014 <- wits_data2014 %>%
  arrange(desc(`Trade.Value.1000USD`)) %>%
  head(10)
ggplot(top_10_value2014, aes(x = reorder(Reporter, `Trade.Value.1000USD`), y = `Trade.Value.1000USD`)) + 
  geom_bar(stat='identity') +
  theme_minimal() +
  labs(title = 'Top 10 Tire Exporters in 2015',
       x = 'Country',
       y = 'Trade value in 1000 USD')
