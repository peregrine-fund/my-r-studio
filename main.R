

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
ggplot(industrial_production, aes(x = observation_date, y = IPG32621S)) +
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
#ggplot(industrial_production, aes(x = observation_date, y = IPG326S_CH1)) +
 # geom_line(size = 1.2) +
  #scale_x_date(
   # date_breaks = "3 years",
    #date_labels = "%Y"
#  ) +
#  theme_minimal() +
#  labs(x = 'Year') +
#  labs(y = 'Annual Growth Rate (%)') +
#  theme(
#    axis.text.x = element_text(angle = 45, hjust = 1)
#  )
#ggsave(file.path(image, "timeseries_growthrates.png"))

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
    y = 'Number of Countries'
  ) +
  theme_minimal()
ggsave(file.path(image, "histogram-of-importers.png"))

#log histogram
ggplot(df_import_to_us_c,aes(x = Value)) +
  scale_x_log10() +
  theme_minimal() +
  labs(
    x = 'Value of Imports to US (USD, Log Scale)',
    y = 'Number of Countries'
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


# KUBA - adding BLS tires
blsRaw <- read.csv(file.path(comparision, "bls.csv"), stringsAsFactors = FALSE)

bls <- blsRaw %>%
  mutate(
    month_num = gsub("M", "", Period),
    observation_date = as.Date(paste(Year, month_num, "01", sep = "-"), format = "%Y-%m-%d"),
    BLS_Value = as.numeric(Value)
  ) %>%
  select(observation_date, BLS_Value )
ppiPrices2000 <- ppiPrices2000 %>%
  mutate(observation_date = as.Date(observation_date))
ppiPrices2000 <- ppiPrices2000 %>%
  left_join(bls, by = "observation_date")
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

#CARG values of idexes
CagrPcu= ((ppiPrices2000$PCU3262132621 [c(315)])/ppiPrices2000$PCU3262132621 [c(1)])**(1/315)-1
CagrIz= ((ppiPrices2000r$IZ32621 [c(243)])/ppiPrices2000r$IZ32621 [c(1)])**(1/243)-1
CagrHs= ((ppiPrices2000$hsPrice [c(315)])/ppiPrices2000$hsPrice [c(1)])**(1/315)-1 
CagrBLS= ((ppiPrices2000$BLS_Value [c(315)])/ppiPrices2000$BLS_Value [c(1)])**(1/315)-1

ppiPrices2000r=ppiPrices2000r %>%
  mutate(
    observation_date = as.Date(observation_date)
  )

#time sereies
ggplot(data=ppiPrices2000r, aes(x=observation_date)) +
  geom_line(aes(y = hsPrice, color = "HS price")) +
  geom_line(aes(y = IZ32621, color = "Import price index")) +
  geom_line(aes(y = PCU3262132621, color = "Producer price index")) +
  geom_line(aes(y = BLS_Value, color = "Consumer price index"))



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
`











#!Correlation part

library(tidyverse)
#rm(list=ls())
correlation <- here("data", "correlation")

rawCorrelationDf=read.csv(file.path(comparision, "complete_df.csv"))
industrial_production_n = read.csv(file.path(correlation,"IPG32621N.csv"))
industrial_production_n <- industrial_production_n %>%
mutate(observation_date = as.Date(observation_date))
summary(industrial_production_n)
######################
#adding tarrifs to the rawcorrelationdf
valueOfImports = read.csv(
  file = file.path(correlation, "import-census-4011.csv"), 
  sep = ";", 
  skip = 3, 
  header = TRUE
)

tarrifs_data <- valueOfImports %>%
  filter(str_detect(Time, " ") & !str_detect(Time, "through")) %>%
  
  select(Time, calculatedDuty= 8)%>% 
  mutate(
    calculatedDuty = as.numeric(gsub(",", "", calculatedDuty)),
    Date = as.Date(paste("01", Time), format = "%d %b %Y")) %>%
  select(Date, calculatedDuty)

rawCorrelationDf <-rawCorrelationDf %>%
  mutate(
    observation_date = as.Date(observation_date),
    
  )%>%
left_join(tarrifs_data, by = c("observation_date" = "Date"))
  
###################

#main work ( chart and descriptive stats):

startingIndex=1 # 72 is where ipi starts.
summary(ppiPrices)
print(ppiPrices$observation_date)
print(valueOfImports$Time)
correlationDf <- rawCorrelationDf %>%

  rename(
    quantity = General.First.Unit.of.Quantity,
    importCifValue = General_Cif_Imports_value,
    PPI = PCU3262132621,
    IPI = IZ32621,
    HSP= hsPrice
  ) %>%
  mutate(
    PPI_ind = (PPI / PPI[startingIndex]) * 100,
    HSP_ind=(HSP/HSP[startingIndex])*100,
    #IPI_ind=(IPI/IPI[startingIndex])*100, #- CANT DO IT BECAUSE IT STARTS IN 2005
    quantity_ind=(quantity/quantity[startingIndex])*100,
    price_ratio=PPI_ind/HSP_ind*100,
    import_ind = (importCifValue / importCifValue[startingIndex]) * 100,
    PPI_change =(PPI - lag(PPI)) / lag(PPI),
    importCifValue_ind=importCifValue/importCifValue[startingIndex]*100,

  )%>%

  
  
left_join(industrial_production_n, by = "observation_date") %>%
  rename(
    production = IPG32621N,
    
    
  )%>%
  mutate(
    production_ind=production/production[startingIndex]*100,
    volume_ratio=production_ind/importCifValue_ind*100
  )

#handle missing values (STL doesn't like NAs)
df_for_sa <- correlationDf %>%
  filter(!is.na(quantity) & !is.na(importCifValue))

#seasonal function
get_sa <- function(data_column, start_date = c(2000, 1)) {
  varTs <- ts(data_column, frequency = 12, start = start_date)
  
  varDecomp <- stl(varTs, s.window = "periodic")

  sa_values <- as.numeric(varTs - varDecomp$time.series[, "seasonal"])
  
  return(sa_values)
}

# 4. Add Seasonally Adjusted (SA) columns back to your dataframe
df_for_sa <- df_for_sa %>%
  filter(!is.na(quantity) & !is.na(importCifValue) & !is.na(BLS_Value)) %>%
  mutate(
    quantity_SA   = get_sa(quantity),
    production_SA= get_sa(production),
    PPI_SA=get_sa(PPI),
    BLS_SA=get_sa(BLS_Value),
    importCifValue_SA = get_sa(importCifValue),
    # Create an index for the SA quantity to match your other variables
    quantity_SA_ind = (quantity_SA / quantity_SA[startingIndex]) * 100,
    importCifValue_SA_ind = (importCifValue_SA / importCifValue_SA[startingIndex]) * 100,
    HSP_SA = importCifValue_SA / quantity_SA,
    HSP_SA_ind= (HSP_SA / HSP_SA[startingIndex]) * 100,
    volume_ratio=(production_ind/quantity_ind)*100,
    volume_SA_ratio = ((production_SA / production_SA[startingIndex]) / (quantity_SA / quantity_SA[startingIndex])) * 100,
    price_ratio=PPI_ind/HSP_ind*100,
    price_SA_ratio=PPI_SA/HSP_SA_ind*100,
    BLS_ind=BLS_Value/BLS_Value[startingIndex]*100,
    BLS_share=BLS_ind/PPI_ind*100,
    BLS_SA_share=((BLS_SA / BLS_SA[startingIndex]) / (PPI_SA / PPI_SA[startingIndex])) * 100,
    duty_ratio=calculatedDuty/importCifValue,
    HSP_duty_ind=HSP_ind*(duty_ratio+1),
    import_change =(importCifValue - lag(importCifValue/IPI)) / lag(importCifValue/IPI),
    volume_ratio_change=(volume_ratio - lag(volume_ratio)) / lag(volume_ratio)
  )

# 5. Use the SA data for your correlations
print("--- Raw Correlation ---")

#REALLY COOL! +0.93 COR - means that with decrease of CPI, 
cor(df_for_sa$HS, df_for_sa$volume_ratio, use = "complete.obs")
cor(df_for_sa$BLS_SA_share, df_for_sa$volume_SA_ratio, use = "complete.obs")

model <- lm(volume_ratio ~ BLS_share, data = df_for_sa)
summary(model)
#THIS CORRELATION IS MORE DEPRESING THAN FOOD FROM MENZA. CANT DEDUCT ANYTHING! -
cor(df_for_sa$price_SA_ratio, df_for_sa$volume_SA_ratio, use = "complete.obs")
cor(
  df_for_sa$price_SA_ratio[df_for_sa$observation_date >= as.Date("2012-01-01") & df_for_sa$observation_date <= as.Date("2024-01-01")],
  df_for_sa$volume_SA_ratio[df_for_sa$observation_date >= as.Date("2012-01-01") & df_for_sa$observation_date <= as.Date("2024-01-01")],
  use = "complete.obs"
)
library(dplyr)

# We create a new column where the price ratio is "shifted" forward by 2 months
df_lagged <- df_for_sa %>%
  mutate(price_ratio_lag2 = lag(price_ratio, 2))

# Run the regression using the PAST price to predict CURRENT volume
model_lagged <- lm(volume_ratio ~ price_ratio_lag2, data = df_lagged)

summary(model_lagged)


ggplot(data=df_for_sa, aes(x = observation_date)) +
  geom_line(aes(y = BLS_share, color = "1")) +
  geom_line(aes(y = volume_ratio, color = "2")) +
  
  scale_x_date(
    date_breaks = '3 years',
    date_labels = '%Y'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
)


cor(df_for_sa$quantity_SA_ind, df_for_sa$price_ratio, use = "complete.obs")
cor(correlationDf$price_ratio,correlationDf$volume_ratio)
cor(correlationDf$HSP_ind,correlationDf$quantity)

cor(correlationDf$quantity_ind,correlationDf$price_ratio)


#PRICE TIMESERIES - BEWARE IMPORT PRICE INDEX STARTS AT 2005
ggplot(data=df_for_sa, aes(x = observation_date)) +
  geom_line(aes(y = volume_ratio, color = "HS price")) +
  geom_line(aes(y = BLS_share, color = "import price index")) +
  geom_line(aes(y = BLS_Value, color = "CPI")) +
  geom_line(aes(y = PPI_ind, color = "PPI")) +
  
  scale_x_date(
    date_breaks = '3 years',
    date_labels = '%Y'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(file.path(image, "prices.png"))

ggplot(data=df_for_sa, aes(x = observation_date)) +
  geom_line(aes(y = price_ratio, color = "price ratio")) +
  geom_line(aes(y = volume_SA_ratio, color = "volume ratio")) +
  geom_line(aes(y = BLS_share, color = "BLS ratio")) +
  
  scale_x_date(
    date_breaks = '3 years',
    date_labels = '%Y'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
)
ggplot(data = df_for_sa, aes(x = BLS_SA_share, y = volume_SA_ratio)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "BLS Share vs Volume Ratio",
    x = "CPI/PPI",
    y = ""
    ) +
  theme_minimal()
ggsave(file.path(image, "bls-scatterplot.png"))
#NOW LOADING DATA FROM ./data/correlation and merging dataframes together and filtering redundant data,


# THIS DF BELLOW WILL BE USED MAINLY TO SEE CORRELATION
# ALSO ADDED INDEXES STARTING AT FIRST VALUE AND % CHANGES


#view(correlationDf)

#main work ( chart and descriptive stats):

#3d histogram
ggplot(df_for_sa, aes(x = PPI, y = volume_ratio)) +
  # This creates the "3D" effect using color density
  geom_bin2d(bins = 30) + 
  scale_fill_viridis_c() + # A nice color scale for density
  labs(
    title = "Density of Monthly % Changes",
    x = "PPI % Change",
    y = "Import Value % Change",
    fill = "Frequency"
  ) +
  theme_minimal()

#tariff 
ggplot(data=df_for_sa, aes(x = observation_date)) +
  geom_line(aes(y = HSP_duty_ind, color = "HS + duty price")) +
  geom_line(aes(y = HSP_ind, color = "HS price")) +
  
  scale_x_date(
    date_breaks = '3 years',
    date_labels = '%Y'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+labs(
    title = "Comparison of HSP and HSP duty Indices",
    x = "Year",
    y = "Price index ( 2000 as base year)",
    color = "Legend"
  )
ggsave(file.path(image,"tariff-price.png"))


######################
###################
#######################
# Define a cleaner growth function
calc_growth <- function(x) (x - lag(x)) / lag(x)

df_improved <- df_for_sa %>%
  arrange(observation_date) %>%
  mutate(
    # 1. Use Log Differences for better statistical properties in ratios
    d_price_ratio  = calc_growth(price_SA_ratio),
    d_volume_ratio = calc_growth(volume_SA_ratio),
    d_BLS_share    = calc_growth(BLS_SA_share),
    
    # 2. Lag the price change (The 'Reaction Time')
    d_price_ratio_lag2 = lag(d_price_ratio, 2)
  ) %>%
  # Filter NAs created by lag and growth calcs
  filter(!is.na(d_price_ratio_lag2))

# 3. Correlation check: This is the 'Real' proof
# If this is significantly positive, your theory holds.
cor_real <- cor(df_improved$d_BLS_share, df_improved$d_volume_ratio)
print(paste("Correlation of Changes:", round(cor_real, 3)))

