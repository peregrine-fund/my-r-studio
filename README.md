#To-do list
---
## PLEASE DONT EXPORT MANUALY PICTURES OR CSV FILES IN R. JUST WRITE FUNCTION GGSAVE to save and pick a folder from the options at the top of the code ( images, introduction ...).
## Michal
Please compare ppi and census import prices. Dont compare import price index from fred because its to 2005.  
Census import category for tires ( 4011 based on hs ) is different than fred (302.. NAICS number). I will explain it.  
Do scatter plot of the price changes ( census import price vs ppi fred). Please add comments to R-studio and try to make variables so when changing some parts of the code it will be easy. Save chartes via code ( like ggplot save). Use varaible comparision as path to save.

## LUKAS
Lukasi data byly procistene automaticky co jsem tam pridal s temi importy. Kdyztak napis kdyby neco nebylo nesedelo. A prosim zachovavej raw data. jestli chces na nich filtrovat tak vytvor novy csv   
<!--for labor.csv maybe look at unit labor cost, how much is capital how much intermediate-->
---
## Jacob Notes
Add dataset for importing countries to USA

<!--
add imp-code.txt - its a connection table, show how products labeled by HS ( harmonized standard, the first number) converts to NAICS (nort america industry codes, 2 last number)  
mention discontinuation export notice: https://www.bls.gov/mxp/publications/additional-publications/publication-changes-2022.htm
Explain maybe why I didnt used WPU- commodity ppi measure instead of Naics measure, also explain why export is missing and why i prioritize HS instead of Naics
--!>
