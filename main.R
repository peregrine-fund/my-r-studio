

library(ggplot2)
library(dplyr)
library(here)
# use "here" library because we used git and colleges r studio had problem with paths.
introduction = here("data","introduction")
comparision= here("data","comparision")


#Load Data for tire industry
industrial_production = read.csv(file.path(introduction,"fredgraph_production_nondurable_goods_tires.csv"))
industrial_production$observation_date <- as.Date(industrial_production$observation_date, format = "%Y-%m-%d") #converting date into math object

#loading data for the industry as a whole
industrial_production_totalindex <- read.csv(file.path(introduction,"fred_industrial_production_total_index.csv"), header = TRUE, stringsAsFactors = FALSE)
print(industrial_production_totalindex)
median_totalindustry <- median(industrial_production_totalindex$INDPRO_PC1)
mean_totalindustry <- mean(industrial_production_totalindex$INDPRO_PC1)
print(median_totalindustry)
print(mean_totalindustry)
max_growthTI <- industrial_production_totalindex[which.max(industrial_production_totalindex$INDPRO_PC1), ]
min_growthTI <- industrial_production_totalindex[which.min(industrial_production_totalindex$INDPRO_PC1), ]
print(max_growthTI)
print(min_growthTI)


correlation="./data/correlation"
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
ggsave(file.path(introduction, "timeseries_productionvolume.png"))

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
ggsave(file.path(introduction, "timeseries_growthrates.png"))

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

workers_stats <- read.csv(file.path(introduction,"rubber-workers-data.csv"), sep = ";", check.names = FALSE)
table(workers_stats)

wits_data <- read.csv(file.path(introduction,"WITS-By-HS6Product(By-HS6Product).csv"), sep = ";", stringsAsFactors = FALSE)
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
ggplot(wits_data,aes(x = Quantity)) +
  ggtitle('histogram of volumes of exports') +
  labs(x = 'Quantity') +
  geom_histogram()
ggsave(file.path(introduction, "histogramofvolumesofexports.png"))

#log export histogram
ggplot(wits_data,aes(x = Quantity)) +
  ggtitle('log-transformed scale histogram of volume of exports') +
  labs(x = 'Quantity') +
  scale_x_log10() +
  geom_histogram()
ggsave(file.path(introduction, "loghistogramofexports.png"))

#comparing top_20 exporters $$$ here was 10 before
top_20_value <- wits_data %>%
  arrange(desc(`Trade.Value.1000USD`)) %>%
  head(20)
ggplot(top_20_value, aes(x = reorder(Reporter, `Trade.Value.1000USD`), y = `Trade.Value.1000USD`, fill = Reporter == 'United States')) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  theme_minimal() +
  labs(title = 'Top 20 Tire Exporters in 2024',
       x = 'Country',
       y = 'Trade Value in 1000 USD') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#export data in 2020
wits_data2020 <- read.csv(file.path(introduction,"WITS-By-HS6Product(4)(By-HS6Product).csv"),sep = ';', stringsAsFactors = FALSE) #$$$ doesnt exist
names(wits_data2020) <- trimws(names(wits_data2020))
print(wits_data2020)
top_20_value2020 <- wits_data2020 %>%
  arrange(desc(`Trade.Value.1000USD`)) %>%
  head(20)
ggplot(top_20_value2020, aes(x = reorder(Reporter, `Trade.Value.1000USD`), y = `Trade.Value.1000USD`, fill = Reporter == 'United States')) + 
  geom_bar(stat = 'identity', show.legend = FALSE) +
  theme_minimal() +
  labs(title = 'Top 20 Tire Exporters in 2020',
       x = 'Country',
       y = 'Trade value in 1000 USD') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#export data in 2014

wits_data2014 <- read.csv(file.path(introduction,"WITS-By-HS6Product(3)(By-HS6Product).csv"),sep = ';', stringsAsFactors = FALSE)
names(wits_data2014) <- trimws(names(wits_data2014))
print(wits_data2014)
top_10_value2014 <- wits_data2014 %>%
  arrange(desc(`Trade.Value.1000USD`)) %>%
  head(20)
ggplot(top_10_value2014, aes(x = reorder(Reporter, `Trade.Value.1000USD`), y = `Trade.Value.1000USD`, fill = Reporter == 'United States')) + 
  geom_bar(stat = 'identity', show.legend = FALSE) +
  theme_minimal() +
  labs(title = 'Top 20 Tire Exporters in 2015',
       x = 'Country',
       y = 'Trade value in 1000 USD') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#labor stats
#cleaning of the dataset + I will comment on that later
library(readr)
df <- read_delim('rubber-workers-data.csv', delim = ';', na = 'N.A.', show_col_types = FALSE)
target_measures <- c(
  'Unit labor costs', 'Capital share', 'Capital costs', 'Intermediate inputs share', 'Intermediate inputs costs', 'Employment', 'Real sectoral output', 'Output per worker')
clean_df <- df %>%
  filter(Measure %in% target_measures) %>%
  select(
    Sector, NAICS, Industry, Digit, Basis, Measure, Units,
    all_of(as.character(2000:2024))
  )
write_csv(clean_df, 'cleaned_rubber_data_2000_2024.csv')
head(clean_df)
library(tidyverse)
long_df <- clean_df %>%
  pivot_longer(
    cols = `2000`:`2024`,        
    names_to = 'Year',           
    values_to = 'Value'         
  ) %>%
  mutate(
    Year = as.numeric(Year),     
    Value = as.numeric(Value)   
  )
#filtering data for real sectoral output
df_labour <- long_df %>%
  filter(
    Measure == 'Real sectoral output',
    grepl('% Change', Units) 
  )
print(nrow(df_labour))

ggplot(df_labour, aes(x = Year, y = Value)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = 'real sectoral output',
    x = 'Year',
    y = 'Percentage Change'
  )
#filtering data for output per worker
df_output_per_worker <- long_df %>%
  filter(
    Measure == 'Output per worker',
    grepl('% Change', Units) 
  )
ggplot(df_output_per_worker, aes(x = Year, y = Value)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = 'output per worker',
    x = 'Year',
    y = 'Percentage Change'
  )
#filtering data for labor costs
df_unit_labor_costs <- long_df %>%
  filter(
    Measure == 'Unit labor costs',
    grepl('% Change', Units) 
  )
ggplot(df_unit_labor_costs, aes(x = Year, y = Value)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = 'Unit labor costs',
    x = 'Year',
    y = 'Percentage Change'
  )
#all three together
df_triple_labour <- long_df %>%
  filter(
   Measure %in% c('Real sectoral output', 'Output per worker', 'Unit labor costs'),
    grepl('% Change', Units) 
  )
ggplot(df_triple_labour, aes(x = Year, y = Value, color = Measure)) +
  geom_line() +
  theme_minimal() +
  labs(
    x = 'Year',
    y = 'Percentage Change'
  )
