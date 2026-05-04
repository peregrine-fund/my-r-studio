
rm(list=ls())
library(here)

# 1. Define the path variable first
correlation <- here("data", "correlation")

# 2. Use TRUE in all caps
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