
#!Correlation part

library(tidyverse)
rm(list=ls())
correlation <- here("data", "correlation")

#NOW LOADING DATA FROM ./data/correlation and merging dataframes together and filtering redundant data,
valueOfImports = read.csv(
  file = file.path(correlation, "import-census-4011.csv"), 
  sep = ";", 
  skip = 3, 
  header = TRUE
)

ppiPrices=read.csv(
  file = file.path(correlation, "PPI-&-Import-prices.csv"), 
  sep = ",", 
  
  header = TRUE
)

summary(ppiPrices)
print(ppiPrices$observation_date)
print(valueOfImports$Time)


imports_clean <- valueOfImports %>%
  filter(str_detect(Time, " ") & !str_detect(Time, "through")) %>%
  
  select(Time, ImportCifValue= 6)%>% 
  mutate(
    ImportCifValue = as.numeric(gsub(",", "", ImportCifValue)),
    Date = as.Date(paste("01", Time), format = "%d %b %Y")) %>%
  select(Date, ImportCifValue)
summary(ppiPrices)

ppi_clean <- ppiPrices %>%
  mutate(
    Date = as.Date(observation_date),
    PPI = PCU3262132621 
  ) %>%
  select(Date, PPI)
#IPI stands for  import price index
ipi_clean <- ppiPrices %>%
  mutate(
    Date = as.Date(observation_date),
    IPI = IZ32621 
  ) %>%
  select(Date, IPI)
view(ipi_clean)

startingIndex=48
# THIS DF BELLOW WILL BE USED MAINLY TO SEE CORRELATION
# ALSO ADDED INDEXES STARTING AT FIRST VALUE AND % CHANGES
correlationDf <- inner_join(imports_clean,ppi_clean, by = "Date") %>%
  inner_join(ipi_clean, by = "Date") %>%
  mutate(
    PPI_ind = (PPI / PPI[startingIndex]) * 100,
    IPI_ind=(IPI/IPI[startingIndex])*100,
    price_ratio=PPI_ind/IPI_ind,
    Import_ind = (ImportCifValue / ImportCifValue[1]) * 100,
    PPI_change =(PPI - lag(PPI)) / lag(PPI),
    IMPORT_change =(ImportCifValue - lag(ImportCifValue/IPI)) / lag(ImportCifValue/IPI)
    
  )

#view(correlationDf)

#main work ( chart and descriptive stats):

# 0.94 - really high correlation! Because increase in prices of pneumatics is all over the world - CIF value ( quanity * prices ) will also increase
cor(correlationDf$ImportCifValue,correlationDf$PPI)
#
cor(correlationDf$IMPORT_change, correlationDf$PPI_ind, use = "complete.obs")
cor(correlationDf$ImportCifValue, correlationDf$price_ratio, use = "complete.obs")

summary(correlationDf)

scatter.smooth(correlationDf$price_ratio, correlationDf$ImportCifValue)

print(ipi_clean$IPI_ind)
ggplot(data=correlationDf, aes(x = Date)) +
  geom_line(aes(y = PPI_ind, color = "PPI")) +
  geom_line(aes(y = Import_ind, color = "Import Value")) +
  geom_line(aes(y = IPI_ind, color = "Import price index Value")) +
  
  scale_x_date(
    date_breaks = '3 years',
    date_labels = '%Y'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggplot(correlationDf, aes(x = PPI_ind / IPI_ind, y = ImportCifValue)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", color = "red") + 
  theme_minimal() +
  labs(
    title = "Impact of Domestic vs. Import Price Ratio on Import Volume",
    x = "Relative Price Index (PPI / IPI)",
    y = "Import CIF Value"
  )
ggplot(correlationDf, aes(x = PPI_change, y = IMPORT_change)) +
  # This creates the "3D" effect using color density
  geom_bin2d(bins = 30) + 
  scale_fill_viridis_c() + # A nice color scale for density
  labs(
    title = "Density of Monthly % Changes",
    x = "PPI % Change",
    y = "Import Value % Change",z
    fill = "Frequency"
  ) +
  theme_minimal()
