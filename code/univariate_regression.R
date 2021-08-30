## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------
#knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------------------------------------------------------------
#library
library(sparklyr)
library(dplyr)
library(data.table)
library(dtplyr)
library(caret)
library(tidyverse)
library(lubridate)
library(covdata)
library(zoo)
library(broom)
library(ggdag)
library(dagitty)

#plot
library(ggplot2)
library(plotly)
library(viridis)
library(viridisLite)
library(dplyr)
library(hrbrthemes)
library(kableExtra)
library(DT)
library(GGally)
library(corrplot)
library(Rmisc)
library(graphics)

library(sjPlot)
library(ggplot2)
library(ggeffects)


#options(knitr.table.format = "html")
library(extrafont)
#loadfonts(device = "win")


## ------------------------------------------------------------------------------------------------------------------------------------------
#spatial 
library(sf)
library(spdep)
library(tmap)
library(raster)
library(tmap)
library(mapview)
library(maptools)
library(spdep)
library(leaflet)
library(RColorBrewer)
library(sp)
library(rgdal)
library(mapproj)
library(leaflet.extras)
library(maps)
library(ggmap)
library(mapdata)
library(tmap)
library(rgeos)
library(spdplyr)



## ------------------------------------------------------------------------------------------------------------------------------------------

library(mvtnorm)
library(psych)
library(glasso)
library(glmnet)
library(igraph)
library(stringr)


## ------------------------------------------------------------------------------------------------------------------------------------------
# reading file for Florida counties (FL_allphase_matrix)

FL_reg_finalmatrix <- read.csv("FL_allphase_matrix.csv",header = TRUE)
FL_reg_finalmatrix 


## ------------------------------------------------------------------------------------------------------------------------------------------
# Univariate regression analysis of FLorida (first phase) using function
#CFR = case fatality rate in first phase, CFR.x = case fatality rate in second phase, CFR.y = case fatality rate in third phase

list_data_1 <- list("CFR","CFR.x","CFR.y")
list_data_2 <- list("X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value ", "X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")
"%+%" <- function(x,y) paste(x, y, sep = "")

lapply(list_data_2, function(x){
  summary(lm(as.formula("CFR ~ "  %+% x), data = FL_reg_finalmatrix ))
})


## ------------------------------------------------------------------------------------------------------------------------------------------
# Univariate regression analysis of Florida (second phase)
#CFR.x = Case fatality rate in second phase of Florida

list_data_1 <- list("CFR","CFR.x","CFR.y")
list_data_2 <- list("X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value ", "X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")
"%+%" <- function(x,y) paste(x, y, sep = "")

lapply(list_data_2, function(x){
  summary(lm(as.formula("CFR.x ~ "  %+% x), data =FL_reg_finalmatrix))
})


## ------------------------------------------------------------------------------------------------------------------------------------------
# Univariate analysis of FL (third phase)
# Univariate regression analysis of Florida (third phase) 
# CFR.y = case fatality ratio in third phase of Florida 


list_data_1 <- list("CFR","CFR.x","CFR.y")
list_data_2 <- list("X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value ", "X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")
"%+%" <- function(x,y) paste(x, y, sep = "")

lapply(list_data_2, function(x){
  summary(lm(as.formula("CFR.y ~ "  %+% x), data = FL_reg_finalmatrix))
})


## ------------------------------------------------------------------------------------------------------------------------------------------
# New York data file for regression analysis
NY_reg_finalmatrix <- read.csv("NY_allphase_matrix.csv",header = TRUE)
NY_reg_finalmatrix


## ------------------------------------------------------------------------------------------------------------------------------------------
#phase 1 New york state regression analysis including all counties
#CFR=case fatality rate in New York state in the first phase

list_data_3 <- list("CFR","CFR.x","CFR.y")
list_data_4 <- list("X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value ", "X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")
"%+%" <- function(x,y) paste(x, y, sep = "")

lapply(list_data_4, function(x){
  summary(lm(as.formula("CFR ~ "  %+% x), data = NY_reg_finalmatrix ))
})


## ------------------------------------------------------------------------------------------------------------------------------------------
#phase 2 New york state regression analysis including all counties
#CFR.x =case fatality rate in New York state in the second phase

list_data_3 <- list("CFR","CFR.x","CFR.y")
list_data_4 <- list("X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value ", "X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")
"%+%" <- function(x,y) paste(x, y, sep = "")

lapply(list_data_4, function(x){
  summary(lm(as.formula("CFR.x ~ "  %+% x), data = NY_reg_finalmatrix ))
})


## ------------------------------------------------------------------------------------------------------------------------------------------
#phase 3 New york state regression analysis including all counties
#CFR.y = case fatality rate in New York state in the third phase

list_data_3 <- list("CFR","CFR.x","CFR.y")
list_data_4 <- list("X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value ", "X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")
"%+%" <- function(x,y) paste(x, y, sep = "")

lapply(list_data_4, function(x){
  summary(lm(as.formula("CFR.y ~ "  %+% x), data = NY_reg_finalmatrix ))
})


## ------------------------------------------------------------------------------------------------------------------------------------------
###New-York univariate regression where data file includes all counties except New-York city
#Data file of all counties in New-York except New York City

NY_reg_finalmatrix[-c(29), ]


## ------------------------------------------------------------------------------------------------------------------------------------------
#phase 1 New york state regression analysis including all counties except New York City
#CFR = case fatality rate in New York state in the first phase including all counties except New York City

list_data_3 <- list("CFR","CFR.x","CFR.y")
list_data_4 <- list("X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value ", "X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")
"%+%" <- function(x,y) paste(x, y, sep = "")

lapply(list_data_4, function(x){
  summary(lm(as.formula("CFR ~ "  %+% x), data = NY_reg_finalmatrix[-c(29), ]
 ))
})


## ------------------------------------------------------------------------------------------------------------------------------------------
#phase 2 New york state regression analysis including all counties except New York City
#CFR.x = case fatality rate in New York state in the second phase including all counties except New York City

list_data_3 <- list("CFR","CFR.x","CFR.y")
list_data_4 <- list("X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value ", "X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")
"%+%" <- function(x,y) paste(x, y, sep = "")

lapply(list_data_4, function(x){
  summary(lm(as.formula("CFR.x ~ "  %+% x), data = NY_reg_finalmatrix[-c(29), ]
 ))
})


## ------------------------------------------------------------------------------------------------------------------------------------------
#Phase 3 New york state regression analysis including all counties except New York City
#CFR.y = case fatality rate in New York state in the third phase including all counties except New York City

list_data_3 <- list("CFR","CFR.x","CFR.y")
list_data_4 <- list("X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value ", "X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")
"%+%" <- function(x,y) paste(x, y, sep = "")

lapply(list_data_4, function(x){
  summary(lm(as.formula("CFR.y ~ "  %+% x), data = NY_reg_finalmatrix[-c(29), ]
 ))
})


## ------------------------------------------------------------------------------------------------------------------------------------------
##Selected variables file for graphical model analysis without newyork city county#

#Newyork first phase data file for graphical model analysis(all New York counties without New York City)
# counties where CFR with NA values are deleted from data file in first phase and missing value of variable is replaced by min value of that variable

NY_nonyc <- NY_reg_finalmatrix[-c(29), ]%>%
  dplyr::select("CFR","X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value","X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")

NY_nonyc$HIV.prevalence.raw.value[is.na(NY_nonyc$HIV.prevalence.raw.value)] <- min(NY_nonyc$HIV.prevalence.raw.value,na.rm = TRUE)
NY_nonyc <-NY_nonyc %>% 
  drop_na(CFR)
NY_nonyc


## ------------------------------------------------------------------------------------------------------------------------------------------
#Writing file for tetrad graphical analysis ( NY state counties except New York City with CFR and 21 variables) First phase

write.csv(NY_nonyc,'NY_noNYC_1.csv')


## ------------------------------------------------------------------------------------------------------------------------------------------
##Newyork second phase data file for graphical model analysis(all New York counties without New York City)

NY_nonyc_2_phase <- NY_reg_finalmatrix[-c(29), ]%>%
  dplyr::select("CFR.x","X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value","X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")

NY_nonyc_2_phase$HIV.prevalence.raw.value[is.na(NY_nonyc_2_phase$HIV.prevalence.raw.value)] <- min(NY_nonyc_2_phase$HIV.prevalence.raw.value,na.rm = TRUE)
NY_nonyc_2_phase <-NY_nonyc_2_phase %>% 
  drop_na(CFR.x)
NY_nonyc_2_phase
summary(NY_nonyc_2_phase)


## ------------------------------------------------------------------------------------------------------------------------------------------
#Writing file for tetrad graphical analysis ( NY state counties except New York City with CFR and 21 variables) Second phase

write.csv(NY_nonyc_2_phase,'NY_noNYC_2.csv')


## ------------------------------------------------------------------------------------------------------------------------------------------
#Newyork third phase data file for graphical model analysis(all New York counties without New York City)

NY_nonyc_3_phase <- NY_reg_finalmatrix[-c(29), ]%>%
  dplyr::select("CFR.y","X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value","X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")

NY_nonyc_3_phase$HIV.prevalence.raw.value[is.na(NY_nonyc_3_phase$HIV.prevalence.raw.value)] <- min(NY_nonyc_3_phase$HIV.prevalence.raw.value,na.rm = TRUE)
NY_nonyc_3_phase <-NY_nonyc_3_phase %>% 
  drop_na(CFR.y)
NY_nonyc_3_phase
summary(NY_nonyc_3_phase)


## ------------------------------------------------------------------------------------------------------------------------------------------
#Writing file for tetrad graphical analysis ( NY state counties except New York City with CFR and 21 variables) third phase

write.csv(NY_nonyc_3_phase,'NY_noNYC_3.csv')


## ------------------------------------------------------------------------------------------------------------------------------------------
#Florida

#Florda's first phase data file of all counties for graphical model analysis

FL_1_phase <- FL_reg_finalmatrix%>%
  dplyr::select("CFR","X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value","X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")
FL_1_phase <-FL_1_phase %>% 
  drop_na(CFR)

FL_1_phase

## ------------------------------------------------------------------------------------------------------------------------------------------
#Writing file for tetrad graphical analysis first phase of Florida ( CFR and 21 variables)

write.csv(FL_1_phase,'FL_phase_1.csv')


## ------------------------------------------------------------------------------------------------------------------------------------------
#Florida's second phase data file of all counties for graphical model analysis

FL_2_phase <- FL_reg_finalmatrix%>%
  dplyr::select("CFR.x","X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value","X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")
FL_2_phase


## ------------------------------------------------------------------------------------------------------------------------------------------
#Writing file for tetrad graphical analysis second phase of Florida (CFR and 21 variables)

write.csv(FL_2_phase,'FL_phase_2.csv')


## ------------------------------------------------------------------------------------------------------------------------------------------
#Florida's third phase data file of all counties for graphical model analysis

FL_3_phase <- FL_reg_finalmatrix%>%
  dplyr::select("CFR.y","X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value","X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")
FL_3_phase


## ------------------------------------------------------------------------------------------------------------------------------------------
#Writing file for tetrad graphical analysis third phase of Florida (CFR and 21 variables)

write.csv(FL_3_phase,'FL_phase_3.csv')


