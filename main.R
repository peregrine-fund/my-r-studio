

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

workers_stats <- read.csv(file.path(introduction,"rubber-workers-data.csv"), sep = ";", check.names = FALSE)
table(workers_stats)

#labor stats
#cleaning of the dataset + I will comment on that later
# 1. Load and subset
rubberWorkers = read.csv(file.path(introduction,'rubber-workers-data.csv'), sep = ';', na.strings = "N.A.", check.names = FALSE)
cleanedRubberWorkers = rubberWorkers[, -(1:5)]

# 2. Reshape to LONG
cleanedRubberWorkers = reshape(
  cleanedRubberWorkers, 
  direction = "long", 
  varying   = names(cleanedRubberWorkers)[3:ncol(cleanedRubberWorkers)],
  times     = names(cleanedRubberWorkers)[3:ncol(cleanedRubberWorkers)],
  v.names   = "Value", 
  timevar   = "Year", 
  idvar     = c("Measure", "Units")
)

# 3. Create combined name
cleanedRubberWorkers$Measure_Units <- paste0(cleanedRubberWorkers$Measure, "-", cleanedRubberWorkers$Units)

# 4. Reshape to WIDE
cleanedRubberWorkers <- reshape(
  cleanedRubberWorkers, 
  direction = "wide", 
  idvar     = "Year", 
  timevar   = "Measure_Units",
  drop      = c("Measure", "Units")
)

# --- THE FIX STARTS HERE ---
# This searches for "Value." in the names and replaces it with nothing ""
names(cleanedRubberWorkers) <- gsub("Value.", "", names(cleanedRubberWorkers))
# --- THE FIX ENDS HERE ---

# 5. Set Year as index (Note: Fixed typo from 'year' to 'Year')
rownames(cleanedRubberWorkers) = cleanedRubberWorkers$Year

head(cleanedRubberWorkers)
# Find column indices that contain "Millions"
million_cols <- grep("Millions", names(cleanedRubberWorkers))

# Create the new dataframe including the Year column
profitability <- cleanedRubberWorkers[, c("Year", names(cleanedRubberWorkers)[million_cols])]

# Clean the data: Remove commas and convert to numeric
# (R can't plot "20,324.6" if it's stored as text)
for(i in 2:ncol(profitability)) {
  profitability[, i] <- as.numeric(gsub(",", "", profitability[, i]))
}
library(ggplot2)

# Ensure Year is numeric for the x-axis
profitability$Year_Num <- as.numeric(as.character(profitability$Year))

# Create the plot by adding layers manually
# Note: I am using the exact column names from your grep result
ggplot(data = profitability, aes(x = Year_Num)) +
  # 1. Revenue (The Top Line)
  geom_line(aes(y = `Sectoral output-Millions of current dollars`, color = "Revenue"), size = 1.2) +
  geom_point(aes(y = `Sectoral output-Millions of current dollars`, color = "Revenue")) +
  
  # 2. Total Cost
  geom_line(aes(y = `Combined inputs costs-Millions of current dollars`, color = "Total Cost"), size = 1.2) +
  geom_point(aes(y = `Combined inputs costs-Millions of current dollars`, color = "Total Cost")) +
  
  # 3. Labor (Component)
  geom_line(aes(y = `Labor compensation-Millions of current dollars`, color = "Labor"), linetype = "dashed") +
  
  # 4. Intermediate Inputs (Component)
  geom_line(aes(y = `Intermediate inputs costs-Millions of current dollars`, color = "Intermediate"), linetype = "dashed") +
  
  # 5. Capital (Component)
  geom_line(aes(y = `Capital costs-Millions of current dollars`, color = "Capital"), linetype = "dashed") +
  
  # Formatting
  scale_color_manual(values = c(
    "Revenue" = "darkgreen", 
    "Total Cost" = "red", 
    "Labor" = "blue", 
    "Intermediate" = "orange", 
    "Capital" = "grey40"
  )) +
  labs(
    title = "Tire Industry: Revenue vs. Costs (Millions)",
    subtitle = "Struggle Analysis: The narrowing gap between Green (Revenue) and Red (Total Cost)",
    x = "Year",
    y = "Millions of USD",
    color = "Financial Category"
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
df_import_to_us <- read.csv("WITS-By-HS6Product(By-HS6Product).csv", sep = ";", skip = 3, stringsAsFactors = FALSE)
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
  theme_minimal()
ggsave(file.path(image, "histogram-of-importers.png"))

#log histogram
ggplot(df_import_to_us_c,aes(x = Value)) +
  scale_x_log10() +
  theme_minimal() +
  geom_histogram()
ggsave(file.path(image, "loghistogram-of-importers.png"))


######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
#Comparison part



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

#unifying time period
PPIValueMerger = ppiPrices[-c(1:337),]
PPIValueMerger = PPIValueMerger[-c(291),]
PPIValueMerger = PPIValueMerger %>%
  bind_cols(valueOfImportsMonths %>% select(CIF.Value..Gen....US.)) %>%
  mutate(CIF.Value..Gen....US. = as.numeric(str_remove_all(`CIF.Value..Gen....US.`, "[,$]")) / 1e6
  )

ppiPrices2000r = remove_missing(ppiPrices2000)
indeXPCU = ppiPrices2000r$PCU3262132621 [c(1)]
indeXIZ =  ppiPrices2000r$IZ32621 [c(1)]

ppiPrices2000r$PCU3262132621=ppiPrices2000r$PCU3262132621 /(indeXPCU*0.01)
ppiPrices2000r$IZ32621 = ppiPrices2000r$IZ32621/(indeXIZ*0.01)



#scatter plot graph
ggplot(data = ppiPrices2000r,aes(x=IZ32621,y=PCU3262132621)) +
  geom_point(size=1,color=1) +
  geom_smooth(method = 'lm')
ggsave(file.path(image, "PcuIzCorellation.png"))


#filtering data and sorting data
ImportVolumesSort = ImportVolumes[-c(1:11),]
ImportVolumesSort = ImportVolumesSort %>%
  filter(Year>1999)
ImportVolumesSort = ImportVolumesSort[c(0:315),]
ImportVolumesSort <- ImportVolumesSort %>%
  mutate(
    Year = as.numeric(Year),
    Month = as.numeric(Month)
  )
ImportVolumesSort = sort_by(ImportVolumesSort, ~ Year + Month)
write_csv(ImportVolumesSort, file.path(comparision, "ImportVolumesSort.csv"))


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
GeneralCif = sort_by(GeneralCif, ~ Year + Month)

ppiPrices2000 = ppiPrices2000%>%
  bind_cols(ImportVolumesSort %>% select(General.First.Unit.of.Quantity))
ppiPrices2000 = ppiPrices2000%>%
  bind_cols(GeneralCif %>% select(General_Cif_Imports_value))
ppiPrices2000 = ppiPrices2000 %>%
  mutate(
    General.First.Unit.of.Quantity = as.numeric(General.First.Unit.of.Quantity),
    General_Cif_Imports_value = as.numeric(General_Cif_Imports_value)
  )
ppiPrices2000 = remove_missing(ppiPrices2000) %>%
mutate(hsPrice = General_Cif_Imports_value / General.First.Unit.of.Quantity)


write_csv(ppiPrices2000, file.path(comparision, "complete_df.csv"))

cor(ppiPrices2000r$IZ32621,ppiPrices2000r$PCU3262132621,use = 'complete.obs')


######################################################################
######################################################################
######################################################################
######################################################################
######################################################################

#!Correlation part

library(tidyverse)
#rm(list=ls())
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


