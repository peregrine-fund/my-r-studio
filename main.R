

library(ggplot2)
library(dplyr)
library(here)
library(tidyverse)
library(readr)
library(stringr)

# USE these variables to specify paths where to save.
introduction = here("data","introduction")
comparision= here("data","comparision")
correlation= here("data","correlation")

image=here("images")


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


#time series graph for production volumes
ggplot(industrial_production, aes(x = observation_date, y = IPG326S)) +
  geom_line(size = 1.2) +
  scale_x_date(
    date_breaks = '3 years',
    date_labels = '%Y'
  ) +
  theme_minimal() +
  labs(x = 'Year') +
  labs(y = 'Industrial Production Level (Index 2017=100)') +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(file.path(image, "timeseries_productionvolume.png"))

#time series graph for production volumes growth rates
ggplot(industrial_production, aes(x = observation_date, y = IPG326S_CH1)) +
  geom_line(size = 1.2) +
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y"
  ) +
  theme_minimal() +
  labs(x = 'Year') +
  labs(y = 'Annual Growth Rate (%)') +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(file.path(image, "timeseries_growthrates.png"))

#median, mean, max, min for growth rates and removal of Not number
print(median(industrial_production$IPG326S_CH1, na.rm = TRUE))
print(mean(industrial_production$IPG326S_CH1, na.rm = TRUE))
max_growth <- industrial_production[which.max(industrial_production$IPG326S_CH1), ]
min_growth <- industrial_production[which.min(industrial_production$IPG326S_CH1), ]
print(max_growth)
print(min_growth)

#standard deviation and removal of Not number
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

library(tidyverse)

# 1. NAČTENÍ A ČIŠTĚNÍ (Tidyr way)
# ---------------------------------------------------------
raw_data <- read.csv(file.path(introduction, "rubber-workers-data.csv"), 
                     sep = ";", na.strings = "N.A.", check.names = FALSE)

profitability_tidy <- raw_data %>%
  # Odstraníme nepotřebné meta sloupce
  select(-(1:5)) %>%
  # Převedeme roky ze sloupců do řádků
  pivot_longer(cols = -c(Measure, Units), names_to = "Year", values_to = "Value") %>%
  # Vyčistíme data: odstraníme čárky, převedeme na čísla a zkrátíme názvy
  mutate(
    Year = as.numeric(Year),
    Value = as.numeric(gsub(",", "", Value)),
    Category = gsub("-Millions of current dollars", "", paste0(Measure, "-", Units))
  ) %>%
  # Převedeme zpět na široký formát pro snadné počítání
  pivot_wider(id_cols = Year, names_from = Category, values_from = Value) %>%
  # Vybereme jen sloupce s miliony (očištěné od dlouhých názvů)
  select(Year, Revenue = `Sectoral output`, Total_Cost = `Combined inputs costs`, 
         Labor = `Labor compensation`, Intermediate = `Intermediate inputs costs`, 
         Capital = `Capital costs`)

# 2. GRAF: ABSOLUTNÍ HODNOTY (REVENUE VS COSTS)
# ---------------------------------------------------------
ggplot(profitability_tidy, aes(x = Year)) +
  geom_line(aes(y = Revenue, color = "Revenue"), size = 1.2) +
  geom_point(aes(y = Revenue, color = "Revenue")) +
  geom_line(aes(y = Total_Cost, color = "Total Cost"), size = 1.2) +
  geom_point(aes(y = Total_Cost, color = "Total Cost")) +
  geom_line(aes(y = Labor, color = "Labor"), linetype = "dashed") +
  geom_line(aes(y = Intermediate, color = "Intermediate"), linetype = "dashed") +
  geom_line(aes(y = Capital, color = "Capital"), linetype = "dashed") +
  scale_color_manual(values = c(
    "Revenue" = "darkgreen", "Total Cost" = "red", 
    "Labor" = "blue", "Intermediate" = "orange", "Capital" = "grey40"
  )) +
  labs(
    title = "Tire Industry: Revenue vs. Costs (Millions)",
    subtitle = "Analysis of the narrowing profit margin",
    y = "Millions of USD", x = "Year"
  ) +
  theme_minimal()

# 3. GRAF: PROCENTUÁLNÍ PODÍL NA NÁKLADECH (%)
# ---------------------------------------------------------
# Nejdříve spočítáme podíly jednotlivých složek na celkových nákladech
cost_percentage_data <- profitability_tidy %>%
  mutate(
    Labor_Pct = (Labor / Total_Cost) * 100,
    Intermediate_Pct = (Intermediate / Total_Cost) * 100,
    Capital_Pct = (Capital / Total_Cost) * 100
  ) %>%
  select(Year, Labor_Pct, Intermediate_Pct, Capital_Pct) %>%
  pivot_longer(-Year, names_to = "Cost_Component", values_to = "Percentage")

ggplot(cost_percentage_data, aes(x = Year, y = Percentage, fill = Cost_Component)) +
  geom_area(alpha = 0.8, color = "white") +
  scale_fill_manual(
    values = c("Labor_Pct" = "blue", "Intermediate_Pct" = "orange", "Capital_Pct" = "grey40"),
    labels = c("Capital", "Intermediate", "Labor")
  ) +
  labs(
    title = "Tire Industry: Cost Structure (%)",
    subtitle = "Relative share of Labor, Intermediate Inputs, and Capital in Total Costs",
    y = "Share of Total Cost (%)", x = "Year", fill = "Component"
  ) +
  theme_minimal()
#############################
#############################
#############################

target_measures <- c(
  'Unit labor costs', 'Capital share', 'Capital costs', 'Intermediate inputs share', 'Intermediate inputs costs', 'Employment', 'Real sectoral output', 'Output per worker')
clean_df <- df %>%
  filter(Measure %in% target_measures) %>%
  select(
    Sector, NAICS, Industry, Digit, Basis, Measure, Units,
    all_of(as.character(2000:2024))
  )
write_csv(clean_df, file.path(introduction, "cleaned_rubber_data_2000_2024.csv"))
head(clean_df)


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


#import to the us
df_import_to_us <- read.csv(file.path(introduction, "WITS-By-HS6Product(By-HS6Product).csv"), sep = ";", skip = 3, stringsAsFactors = FALSE)
df_import_to_us_c <- df_import_to_us[, 1:4]
df_import_to_us_c[[4]] <- as.numeric(gsub(",", "", df_import_to_us[[4]]))
colnames(df_import_to_us_c) <- c("Commodity", "Country", "Year", "Value")
print(df_import_to_us_c)
#max,min, mean, median
max_import <- df_import_to_us_c[which.max(df_import_to_us_c$Value), ]
min_import <- df_import_to_us_c[which.min(df_import_to_us_c$Value), ]
print(max_import)
print(min_import)
#histogram 
ggplot(df_import_to_us_c, aes(x = Value)) +
  geom_histogram() +
  labs(
    x = 'Value of Imports to US (USD)',
    y = 'Number of Observations'
  ) +
  theme_minimal()
ggsave(file.path(image, "histogram-of-importers.png"))

#log histogram
ggplot(df_import_to_us_c,aes(x = Value)) +
  scale_x_log10() +
  theme_minimal() +
  labs(
    x = 'Value of Imports to US (USD, Log Scale)',
    y = 'Number of Observations'
  ) +
  geom_histogram()
ggsave(file.path(image, "loghistogram-of-importers.png"))


######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
#Comparison part





# use "here" library because we used git and colleges r studio had problem with paths.
introduction = here("data","introduction")
comparision= here("data","comparision")
image=here("images")


#importing data
ppiPrices=read.csv(
  file = file.path(comparision, "PPI-&-Import-prices.csv"), 
  sep = ",", 
  header = TRUE
)
valueOfImports = read.csv(
  file = file.path(comparision, "import-census-4011.csv"), 
  sep = ";", 
  skip = 3, 
  header = TRUE,
)
ImportVolumes =read.csv(
  file = file.path(comparision, "usitc.csv"), 
  sep = ";", 
  header = TRUE,
)



#removing data pre-2000 and one year summary values
ppiPrices2000 = ppiPrices[-c(1:313),]
valueOfImportsMonths = valueOfImports[-c(1:11),]
valueOfImportsMonths = valueOfImportsMonths %>%
  filter(row_number() %% 13 != 0)


#filtering relevant data,and sorting them out
ImportVolumesSort = ImportVolumes[-c(1:11),]
ImportVolumesSort = ImportVolumesSort %>%
  filter(Year>1999)
ImportVolumesSort = ImportVolumesSort[c(0:315),]
ImportVolumesSort <- ImportVolumesSort %>%
  mutate(
    Year = as.numeric(Year),
    Month = as.numeric(Month)
  )
ImportVolumesSort <- ImportVolumesSort %>%
  arrange(Year, Month)
write_csv(ImportVolumesSort, file.path(comparision, "ImportVolumesSort.csv"))

#filtering relevant data,and sorting them out
GeneralCif = ImportVolumes[c(906:1353),]
GeneralCif = GeneralCif %>%
  filter(Year>1999)
GeneralCif = GeneralCif[-1,]
GeneralCif = GeneralCif %>%
  rename(General_Cif_Imports_value = General.First.Unit.of.Quantity)
GeneralCif <- GeneralCif %>%
  mutate(
    Year = as.numeric(Year),
    Month = as.numeric(Month)
  )
GeneralCif <- GeneralCif %>%
  arrange(Year, Month)

#merging data into 1 table, for easier comparision
ppiPrices2000 = ppiPrices2000%>%
  bind_cols(ImportVolumesSort %>% select(General.First.Unit.of.Quantity))
ppiPrices2000 = ppiPrices2000%>%
  bind_cols(GeneralCif %>% select(General_Cif_Imports_value))
ppiPrices2000 = ppiPrices2000 %>%
  mutate(
    General.First.Unit.of.Quantity = as.numeric(General.First.Unit.of.Quantity),
    General_Cif_Imports_value = as.numeric(General_Cif_Imports_value)
  )
ppiPrices2000 = ppiPrices2000 %>%
  mutate(hsPrice = General_Cif_Imports_value / General.First.Unit.of.Quantity)

df=read.csv(file = file.path(comparision, "complete_df.csv"))

write_csv(ppiPrices2000, file.path(comparision, "complete_df.csv"))

#indexing data, so they start at the same base, plus removing missing values
ppiPrices2000r = remove_missing(ppiPrices2000)
indeXPCU = ppiPrices2000r$PCU3262132621 [c(1)]
indeXIZ =  ppiPrices2000r$IZ32621 [c(1)]
ideXHS = ppiPrices2000r$hsPrice [c(1)]
ppiPrices2000r$PCU3262132621=ppiPrices2000r$PCU3262132621 /(indeXPCU*0.01)
ppiPrices2000r$IZ32621 = ppiPrices2000r$IZ32621/(indeXIZ*0.01)
ppiPrices2000r$hsPrice = ppiPrices2000r$hsPrice/(ideXHS*0.01)

#scatter plot graph between PPU and IZ
ggplot(data = ppiPrices2000r,aes(x=IZ32621,y=PCU3262132621)) +
  geom_point(size=1,color=1) +
  geom_smooth(method = 'lm')
ggsave(file.path(image, "PcuIzCorellation.png"))

#scatter plot graph between PPU and HS price
ggplot(data = ppiPrices2000r,aes(x=hsPrice,y=PCU3262132621)) +
  geom_point(size=1,color=1) +
  geom_smooth(method = 'lm')
ggsave(file.path(image, "PpiHsCorellation.png"))


cor(ppiPrices2000r$IZ32621,ppiPrices2000r$PCU3262132621)
cor(ppiPrices2000r$hsPrice,ppiPrices2000r$PCU3262132621)
cor(ppiPrices2000r$hsPrice,ppiPrices2000r$IZ32621)

CagrPcu= ((ppiPrices2000$PCU3262132621 [c(315)])/ppiPrices2000$PCU3262132621 [c(1)])**(1/315)-1
CagrIz= ((ppiPrices2000r$IZ32621 [c(243)])/ppiPrices2000r$IZ32621 [c(1)])**(1/243)-1
CagrHs= ((ppiPrices2000$hsPrice [c(315)])/ppiPrices2000$hsPrice [c(1)])**(1/315)-1 



#unifying time period
PPIValueMerger = ppiPrices[-c(1:337),]
PPIValueMerger = PPIValueMerger[-c(291),]
PPIValueMerger = PPIValueMerger %>%
  bind_cols(valueOfImportsMonths %>% select(CIF.Value..Gen....US.)) %>%
  mutate(CIF.Value..Gen....US. = as.numeric(str_remove_all(`CIF.Value..Gen....US.`, "[,$]")) / 1e6
  )


######################################################################
######################################################################
######################################################################
######################################################################
######################################################################

#!Correlation part

library(tidyverse)
#rm(list=ls())
correlation <- here("data", "correlation")

correlationDf=read.csv(file.path(comparision, "complete_df.csv"))
industrial_production = read.csv(file.path(introduction,"fredgraph_production_nondurable_goods_tires.csv"))
industrial_production <- industrial_production %>%
  mutate(observation_date = as.Date(observation_date))
summary(industrial_production)



#main work ( chart and descriptive stats):

# 0.94 - really high correlation! Because increase in prices of pneumatics is all over the world - CIF value ( quanity * prices ) will also increase
startingIndex=1
summary(ppiPrices)
print(ppiPrices$observation_date)
print(valueOfImports$Time)
correlationDf <- correlationDf %>%

  rename(
    quantity = General.First.Unit.of.Quantity,
    importCifValue = General_Cif_Imports_value,
    PPI = PCU3262132621,
    IPI = IZ32621,
    HSP= hsPrice
  ) %>%
  mutate(
    observation_date = as.Date(observation_date),
    PPI_ind = (PPI / PPI[startingIndex]) * 100,
    HSP_ind=(HSP/HSP[startingIndex])*100,
    #IPI_ind=(IPI/IPI[startingIndex])*100,
    quantity_ind=(quantity/quantity[startingIndex])*100,
    price_ratio=PPI_ind/HSP_ind*100,
    import_ind = (importCifValue / importCifValue[startingIndex]) * 100,
    PPI_change =(PPI - lag(PPI)) / lag(PPI),
    importCifValue_ind=importCifValue/importCifValue[startingIndex],
    import_change =(importCifValue - lag(importCifValue/IPI)) / lag(importCifValue/IPI)
    
  )%>%
left_join(industrial_production, by = "observation_date") %>%
  rename(
    production = IPG326S,
    
    
  )%>%
  mutate(
    production_ind=production/production[startingIndex]*100,
    volume_ratio=production_ind/importCifValue_ind*100
  )

cor(correlationDf$HSP_ind,correlationDf$quantity)
cor(correlationDf$volume_ratio,correlationDf$price_ratio)

ggplot(data=correlationDf, aes(x = observation_date)) +
  geom_line(aes(y = HSP_ind, color = "quantity")) +
  geom_line(aes(y = IPI, color = "Import Value")) +

  scale_x_date(
    date_breaks = '3 years',
    date_labels = '%Y'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(file.path(image, "timeseries_growthrates.png"))

#NOW LOADING DATA FROM ./data/correlation and merging dataframes together and filtering redundant data,
valueOfImports = read.csv(
  file = file.path(correlation, "import-census-4011.csv"), 
  sep = ";", 
  skip = 3, 
  header = TRUE
)
hello = read.csv(
  file = file.path(correlation, "complete_df.csv"), 
  sep = ",", 
  header = TRUE
)
ppiPrices=read.csv(
  file = file.path(correlation, "PPI-&-Import-prices.csv"), 
  sep = ";", 
  
  header = TRUE
)
#Using complete_df.csv made in comparision part. 

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


