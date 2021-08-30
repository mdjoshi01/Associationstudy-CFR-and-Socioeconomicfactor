## ------------------------------------------------------------------------------------------------------------------------------------------
#library(sparklyr)
#library(dplyr)
#sc <- spark_connect(master = "local")


## ------------------------------------------------------------------------------------------------------------------------------------------
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

#Reading NYtimes data from github (****)

county_nytimesdata <- read.csv("us-counties.csv",header = TRUE)
head(county_nytimesdata)
tail(county_nytimesdata)


## ------------------------------------------------------------------------------------------------------------------------------------------
#NY times dataset fips code in NA for New York City county
#replacing fips code for new-york-city county (NA to 36061)

county_nytimes_mod <- county_nytimesdata%>%
  dplyr::filter(state=="New York")
tail(county_nytimes_mod)
summary(county_nytimes_mod$fips)
dim(county_nytimes_mod)

if(county_nytimes_mod$county %in% c("New York City")){
   county_nytimes_mod[c("fips")][is.na(county_nytimes_mod[c("fips")])] <- 36061
   }

head(county_nytimes_mod)
summary(county_nytimes_mod$fips)
dim(county_nytimes_mod)




## ------------------------------------------------------------------------------------------------------------------------------------------
#function to convert into daily count

dailycount <- function(x){
  dailycount <- numeric(length(x))
  names(dailycount) <- x
  
  dailycount[1] = x[1]

  for(i in 2:length(x)){
    dailycount[i] <- abs(x[i] - x[i-1])
  }

  return(dailycount)
}


## ------------------------------------------------------------------------------------------------------------------------------------------
#fips code of New York City is missing 

library(stringr)
county_US <- county_nytimesdata%>%
  group_by(fips)%>%
  arrange(fips)%>%
  drop_na(fips)
county_US$fips<-str_pad(county_US$fips,width = 5,side = "left", pad = "0")
head(county_US)
tail(county_US)


## ------------------------------------------------------------------------------------------------------------------------------------------
#NYstate
library(stringr)
county_NY <- county_nytimes_mod%>%
  group_by(fips)%>%
  arrange(fips)%>%
  drop_na(fips)
county_NY$fips<-str_pad(county_NY$fips,width = 5,side = "left", pad = "0")
head(county_NY)





## ------------------------------------------------------------------------------------------------------------------------------------------
# cumulative death and cases for all US counties for 2021-01-15 or 1/15/2021

county_US_cum <- dplyr::filter(county_US, date =="2021-01-15")
  #dplyr::filter(cases>1,deaths>1)%>%
  #dplyr::mutate( log(deaths),log(cases))
  
head(county_US_cum)



## ------------------------------------------------------------------------------------------------------------------------------------------
# cumulative death and cases for US counties for the date 2020-07-01

county_US_cum_spring <- dplyr::filter(county_US, date =="2020-06-30")
  #dplyr::filter(cases>1,deaths>1)%>%
  #dplyr::mutate( log(deaths),log(cases))
  
head(county_US_cum_spring)


## ------------------------------------------------------------------------------------------------------------------------------------------
#cumulative death and cases for FL counties till date 2020-06-30 (county_FL_cum_spring)

county_FL_cum_spring <- county_US_cum_spring%>%
  dplyr::filter(state=="Florida")%>%
  dplyr::filter(cases>1,deaths>1)%>%
  dplyr::mutate( log(deaths),log(cases))
head(county_FL_cum_spring)



## ------------------------------------------------------------------------------------------------------------------------------------------
#cumulative deaths and cases for FL counties in between 2020-07-01  to  2020-09-30 (county_FL_summer)


county_FL_cum_summer_1 <- county_US %>%
  dplyr::filter(date == "2020-07-01")%>%
  dplyr::filter(state=="Florida")
  #filter(cases>1,deaths>1)%>%
  #dplyr::mutate( log(deaths),log(cases))
county_FL_cum_summer_2 <- county_US%>%
  dplyr::filter( date == "2020-09-30")%>%
  dplyr::filter(state=="Florida")
  #filter(cases>1,deaths>1)%>%
  #dplyr::mutate( log(deaths),log(cases))
county_FL_cum_summer <- merge(county_FL_cum_summer_1,county_FL_cum_summer_2,by="fips",all="TRUE")
county_FL_summer <- county_FL_cum_summer%>%
  dplyr::mutate(cases=abs(cases.y-cases.x),deaths=abs(deaths.y-deaths.x))
#(county_FL_cum_summer_1)
#county_FL_cum_summer_2
head(county_FL_cum_summer)
head(county_FL_summer)


## ------------------------------------------------------------------------------------------------------------------------------------------
#cumulative deaths and cases for FL counties in between 2020-10-01  to  2021-01-15 (County_FL-fall)

county_FL_cum_fall_1 <- county_US %>%
  dplyr::filter(date == "2020-10-01")%>%
  dplyr::filter(state=="Florida")
  #filter(cases>1,deaths>1)%>%
  #dplyr::mutate( log(deaths),log(cases))
county_FL_cum_fall_2 <- county_US%>%
  dplyr::filter( date == "2021-01-15")%>%
  dplyr::filter(state=="Florida")
  #filter(cases>1,deaths>1)%>%
  #dplyr::mutate( log(deaths),log(cases))
county_FL_cum_fall <- merge(county_FL_cum_fall_1,county_FL_cum_fall_2,by="fips",all="TRUE")
county_FL_fall <- county_FL_cum_fall%>%
  dplyr::mutate(cases=abs(cases.y-cases.x),deaths=abs(deaths.y-deaths.x))
#(county_FL_cum_summer_1)
#county_FL_cum_summer_2
head(county_FL_cum_fall)
head(county_FL_fall)


## ------------------------------------------------------------------------------------------------------------------------------------------
#NewYork
#cumulative death and cases for NY counties the date 2021-01-15
county_NY_cum <- filter(county_NY, date=="2021-01-15")%>%
  filter(cases>1,deaths>1)%>%
  dplyr::mutate( log(deaths),log(cases))
  
head(county_NY_cum)



## ------------------------------------------------------------------------------------------------------------------------------------------
# cumulative death and cases for NY counties for date 2020-05-31 (County_NY_cum_spring)
county_NY_cum_spring <- filter(county_NY, date =="2020-05-31")%>%
  filter(cases>1,deaths>1)%>%
  dplyr::mutate( log(deaths),log(cases))
  
head(county_NY_cum_spring)


## ------------------------------------------------------------------------------------------------------------------------------------------
# cumulative death and cases for NY counties for date in between 2020-06-01 & 2020-09-30 (county_NY_summer)

county_NY_cum_summer_1 <- filter(county_NY,  date == "2020-06-01")
  #filter(cases>1,deaths>1)%>%
  #dplyr::mutate( log(deaths),log(cases))
county_NY_cum_summer_2 <- filter(county_NY,  date == "2020-09-30")
  #filter(cases>1,deaths>1)%>%
  #dplyr::mutate( log(deaths),log(cases))
county_NY_cum_summer <- merge(county_NY_cum_summer_1,county_NY_cum_summer_2,by="fips",all="TRUE")
county_NY_summer <- county_NY_cum_summer%>%
  dplyr::mutate(cases=abs(cases.y-cases.x),deaths=abs(deaths.y-deaths.x))
#(county_NY_cum_summer_1)
#county_NY_cum_summer_2
head(county_NY_cum_summer)
head(county_NY_summer)
#summary(county_NY_cum_summer)


## ------------------------------------------------------------------------------------------------------------------------------------------
#cumulative death and cases for NY counties for the date in between 2020-10-01 & 2021-01-15 (county_NY_fall)

county_NY_cum_fall_1 <- filter(county_NY,  date == "2020-10-01")
  #filter(cases>1,deaths>1)%>%
  #dplyr::mutate( log(deaths),log(cases))
county_NY_cum_fall_2 <- filter(county_NY,  date == "2021-01-15")
  #filter(cases>1,deaths>1)%>%
  #dplyr::mutate( log(deaths),log(cases))
county_NY_cum_fall <- merge(county_NY_cum_fall_1,county_NY_cum_fall_2,by="fips",all="TRUE")
county_NY_fall <- county_NY_cum_fall%>%
  dplyr::mutate(cases=abs(cases.y-cases.x),deaths=abs(deaths.y-deaths.x))
#(county_NY_cum_fall_1)
#county_NY_cum_fall_2
head(county_NY_cum_fall)
head(county_NY_fall)


## ------------------------------------------------------------------------------------------------------------------------------------------
# Reading county health ranking file for all US counties (****)
#health ranking file
counties_healthrank <- read.csv("counties_healthrank.csv", header = TRUE)
head(counties_healthrank)
tail(counties_healthrank)
#names(unique(counties_healthrank))


## ------------------------------------------------------------------------------------------------------------------------------------------
#function to convert formatting of fips code

fips_format <- function(x){
  fipsformat <- numeric(length(x))
  names(fipsformat) <- x
  

  for(i in 1:length(x)){
    fipsformat[i] <- as.factor(formatC(x[i], width = 5, format = "d", flag = "0"))
  }
  
  print(fipsformat[1])

  return(fipsformat)
}


## ------------------------------------------------------------------------------------------------------------------------------------------
fips_format_1 <- function(x){
  fipsformat <- numeric(length(x))
  names(fipsformat) <- x
  

  for(i in 1:length(x)){
    fipsformat[i] <- (sprintf(x[i], "%05d"))
  }

  return(fips_format_1)
}


## ------------------------------------------------------------------------------------------------------------------------------------------
#county health ranking
#county health ranking (selecting variables from the counties_healthrank file)


library(dplyr)
county_hrank <- dplyr::select(counties_healthrank,State.Abbreviation,State.FIPS.Code,X5.digit.FIPS.Code,Name,Poor.or.fair.health.raw.value,Adult.smoking.raw.value,Adult.obesity.raw.value,Physical.inactivity.raw.value,Excessive.drinking.raw.value,Sexually.transmitted.infections.raw.value,Uninsured.raw.value,Diabetes.monitoring.raw.value,High.school.graduation.raw.value,Some.college.raw.value,Unemployment.raw.value,Children.in.poverty.raw.value,Children.in.poverty..Hispanic.,Income.inequality.raw.value,Air.pollution...particulate.matter.raw.value,Severe.housing.problems.raw.value,Diabetes.prevalence.raw.value,HIV.prevalence.raw.value,Food.insecurity.raw.value,Limited.access.to.healthy.foods.raw.value,Uninsured.adults.raw.value,Median.household.income.raw.value,Population.raw.value,X..below.18.years.of.age.raw.value,X..65.and.older.raw.value,X..Non.Hispanic.African.American.raw.value,X..American.Indian.and.Alaskan.Native.raw.value,X..Asian.raw.value,X..Native.Hawaiian.Other.Pacific.Islander.raw.value,X..Hispanic.raw.value,X..Non.Hispanic.white.raw.value,Cancer.incidence.raw.value,Poverty.raw.value,Communicable.disease.raw.value,Drug.overdose.deaths.raw.value,Food.insecurity.raw.value,Violent.crime.raw.value,Frequent.mental.distress.raw.value)%>%dplyr::rename(fips = X5.digit.FIPS.Code,county=Name)
county_hrank$fips<-str_pad(county_hrank$fips,width = 5,side = "left", pad = "0")

head(county_hrank,n=100)
#rename(county_hrank,fips = X5.digit.FIPS.Code,county = Name)


## ------------------------------------------------------------------------------------------------------------------------------------------
# Filtering NY counties data from county health ranking data set
county_hrank_NY <- county_hrank%>%
  dplyr::filter(State.Abbreviation == "NY")
head(county_hrank_NY)


## ------------------------------------------------------------------------------------------------------------------------------------------
#county abridge dataset from Yu group data doe US counties (****)
#selecting variables from county abridge data set
#county abridge dataset from yu group data

county_abr  <- read.csv("county_data_abridged.csv", header=TRUE)
head(county_abr)
county_new_hr <- dplyr::select(county_abr,countyFIPS,COUNTYFP,State,CountyName,lon,lat,POP_LONGITUDE,POP_LATITUDE,PopulationEstimate2018,PopulationEstimate65.2017,PopulationDensityperSqMile2010,HeartDiseaseMortality,StrokeMortality,RespMortalityRate2014,Smokers_Percentage,DiabetesPercentage,SVIPercentile)%>%
  dplyr::rename(fips=countyFIPS,state=State)
#county_new_hr$fips <- as.character(county_new_hr$fips)
#county_new_hr$lat<- as.numeric(county_new_hr$lat)
#county_new_hr$lat<- as.numeric(county_new_hr$lon)

county_new_hr$fips <- as.character(county_new_hr$fips)
head(county_new_hr)



## ------------------------------------------------------------------------------------------------------------------------------------------
#Function for daily count for cases and death

#daily count

dailycount <- function(x){
  dailycount <- numeric(length(x))
  names(dailycount) <- x
  
  dailycount[1] = x[1]

  for(i in 2:length(x)){
    dailycount[i] <- abs(x[i] - x[i-1])
  }

  return(dailycount)
}


## ------------------------------------------------------------------------------------------------------------------------------------------
# Merging county health ranking ,county abridge data with with ny times data file for US counties
#CFR calucation and CFR column added in (county health+Yu-group+NY times data merged file)
#merging county health ranking with County_us_cum

county_merge <- inner_join(county_US_cum,county_hrank, by = c("fips" = "fips"))%>%
  group_by(fips)%>%
  arrange(fips)
county_merge$fips <- as.character(county_merge$fips)

#head(county_merge)

# merge county_merge with county abridge data

county_merge_US_1 <- inner_join(county_merge,county_new_hr,by = c("fips" = "fips"))

# CFR calculation in merged file for US counties by the date 2021-01-15 
county_merge_US <- county_merge_US_1%>%
  dplyr::mutate(CFR=((deaths/cases)*100), death_100k = ((deaths/PopulationEstimate2018)*100000),cases_100k = ((cases/PopulationEstimate2018)*100000))%>%
  dplyr::select(-county.y)
head(county_merge_US)


## ------------------------------------------------------------------------------------------------------------------------------------------
##Florida
##### Florida data files: merged file for three different phases(county health rank+yugroup+nytimes) and CFR calculation with added
#CFR column in final merged file

#Merging county health ranking ,county yu grop abridge data with county_FL_cum_spring file (FL spring data ) dated  2020-06-30(FL data)
#CFR calculation and added column in final spring county_merge_FL file (health rank+yu group+nytimes data for spring)

#merging county health ranking with county_FL_cum_spring

county_merge_FL <- inner_join(county_FL_cum_spring,county_hrank, by = c("fips" = "fips"))%>%
  group_by(fips)%>%
  arrange(fips)
county_merge_FL$fips <- as.character(county_merge_FL$fips)

#head(county_merge_FL)

# merge county_merge_FL with county yu group abridge data

county_merge_FL_1 <- inner_join(county_merge_FL,county_new_hr,by = c("fips" = "fips"))


county_merge_FL <- county_merge_FL_1%>%
  dplyr::mutate(CFR=((deaths/cases)*100), death_100k = ((deaths/PopulationEstimate2018)*100000),cases_100k = ((cases/PopulationEstimate2018)*100000))%>%
  dplyr::select(-county.y)
head(county_merge_FL)


## ------------------------------------------------------------------------------------------------------------------------------------------
#Merging county health ranking ,county yu group abridge data with county_FL_summer file (FL fall data ) data
#CFR calculation and added column in final summer county_merge_FL_summer file (health rank+yu group+nytimes data for fall)


# merging county heath ranking with FL counties data( second phase 07-01-2020 to 09-30-2020) county_FL_summer-only 

#merging county health ranking with 
county_merge_FL_summer <- inner_join(county_FL_summer,county_hrank, by = c("fips" = "fips"))%>%
  group_by(fips)%>%
  arrange(fips)
county_merge_FL_summer$fips <- as.character(county_merge_FL_summer$fips)

head(county_merge_FL_summer)


# merge county_merge with county abridge data

county_merge_FL_1_summer <- inner_join(county_merge_FL_summer,county_new_hr,by = c("fips" = "fips"))


county_merge_FL_summer <- county_merge_FL_1_summer%>%
  dplyr::mutate(CFR=((deaths/cases)*100), death_100k = ((deaths/PopulationEstimate2018)*100000),cases_100k = ((cases/PopulationEstimate2018)*100000))%>%
  dplyr::select(-county.y)
head(county_merge_FL_summer)


## ------------------------------------------------------------------------------------------------------------------------------------------
#Merging county health ranking ,county yu group abridge data with county_FL_fall file (FL fall data ) data
#CFR calculation and added column in final fall county_merge_FL_fall file (health rank+yu group+nytimes data for fall)

#FL counties data( third phase 10-01-2020 to 01-15-2021) county_FL_fall-only 

#merging county health ranking with 
county_merge_FL_fall <- inner_join(county_FL_fall,county_hrank, by = c("fips" = "fips"))%>%
  group_by(fips)%>%
  arrange(fips)
county_merge_FL_fall$fips <- as.character(county_merge_FL_fall$fips)

head(county_merge_FL_fall)


# merge county_merge with county abridge data

county_merge_FL_1_fall <- inner_join(county_merge_FL_fall,county_new_hr,by = c("fips" = "fips"))


county_merge_FL_fall <- county_merge_FL_1_fall%>%
  dplyr::mutate(CFR=((deaths/cases)*100), death_100k = ((deaths/PopulationEstimate2018)*100000),cases_100k = ((cases/PopulationEstimate2018)*100000))%>%
  dplyr::select(-county.y)
(county_merge_FL_fall)


## ------------------------------------------------------------------------------------------------------------------------------------------
### New York ###

# merging county health ranking and county abridge data with NY counties data (only NY county data) dated till 2021-01-15
#merging county health ranking with 
county_merge_NY <- inner_join(county_NY_cum,county_hrank, by = c("fips" = "fips"))%>%
  group_by(fips)%>%
  arrange(fips)
county_merge_NY$fips <- as.character(county_merge_NY$fips)

head(county_merge_NY)

# merge county_merge with county abridge data

county_merge_NY_1 <- inner_join(county_merge_NY,county_new_hr,by = c("fips" = "fips"))

county_merge_NY <- county_merge_NY_1%>%
  dplyr::mutate(CFR=((deaths/cases)*100), death_100k = ((deaths/PopulationEstimate2018)*100000),cases_100k = ((cases/PopulationEstimate2018)*100000))%>%
  dplyr::select(-county.y)
head(county_merge_NY)


## ------------------------------------------------------------------------------------------------------------------------------------------
# NewYork
### NewYork  data files: merged file for three different phases(county health rank+yugroup+nytimes) and CFR calculation with added
#CFR column in final merged file


#Merging county health ranking ,county yu group abridge data with county_NY_cum_spring file (NY spring data ) dated  2020-05-31(NY data)
#CFR calculation and added column in final spring county_merge_NY_spring file (health rank+yu group+nytimes data for spring)


# NY counties final merged data file( first phase till 05-31-2020) 

#merging county health ranking with 
county_merge_NY_spring <- inner_join(county_NY_cum_spring,county_hrank, by = c("fips" = "fips"))%>%
  group_by(fips)%>%
  arrange(fips)
county_merge_NY_spring$fips <- as.character(county_merge_NY_spring$fips)

head(county_merge_NY_spring)


# merge county_merge with county abridge data

county_merge_NY_1_spring <- inner_join(county_merge_NY_spring,county_new_hr,by = c("fips" = "fips"))


county_merge_NY_spring <- county_merge_NY_1_spring%>%
  dplyr::mutate(CFR=((deaths/cases)*100), death_100k = ((deaths/PopulationEstimate2018)*100000),cases_100k = ((cases/PopulationEstimate2018)*100000))%>%
  dplyr::select(-county.y)
head(county_merge_NY_spring)


## ------------------------------------------------------------------------------------------------------------------------------------------
#Merging county health ranking ,county yu group abridge data with county_NY_summer file (NY summer data ) dated  second phase 06-01-2020 to 09-30-2020(NY data)
#CFR calculation and added column in final summer county_merge_NY_summer file (health rank+yu group+nytimes data for summer)

#NY counties merged data file ( second phase 06-01-2020 to 09-30-2020)

#merging county health ranking with 
county_merge_NY_summer <- inner_join(county_NY_summer,county_hrank, by = c("fips" = "fips"))%>%
  group_by(fips)%>%
  arrange(fips)
county_merge_NY_summer$fips <- as.character(county_merge_NY_summer$fips)

head(county_merge_NY_summer)


# merge county_merge with county abridge data

county_merge_NY_1_summer <- inner_join(county_merge_NY_summer,county_new_hr,by = c("fips" = "fips"))


county_merge_NY_summer <- county_merge_NY_1_summer%>%
  dplyr::mutate(CFR=((deaths/cases)*100), death_100k = ((deaths/PopulationEstimate2018)*100000),cases_100k = ((cases/PopulationEstimate2018)*100000))%>%
  dplyr::select(-county.y)
head(county_merge_NY_summer)


## ------------------------------------------------------------------------------------------------------------------------------------------
#Merging county health ranking ,county yu grop abridge data with county_NY_fall file (NY fall data ) dated third phase 10-01-2020 to 01-15-2021(NY data)
#CFR calculation and added column in final fall county_merge_NY_fall file (health rank+yu group+nytimes data for summer)

# merged NY counties data( third phase 10-01-2020 to 01-15-2021) county_NY_fall-only NYcounty data

#merging county health ranking with 
county_merge_NY_fall <- inner_join(county_NY_fall,county_hrank, by = c("fips" = "fips"))%>%
  group_by(fips)%>%
  arrange(fips)
county_merge_NY_fall$fips <- as.character(county_merge_NY_fall$fips)

head(county_merge_NY_fall)


# merge county_merge with county abridge data

county_merge_NY_1_fall <- inner_join(county_merge_NY_fall,county_new_hr,by = c("fips" = "fips"))


county_merge_NY_fall <- county_merge_NY_1_fall%>%
  dplyr::mutate(CFR=((deaths/cases)*100), death_100k = ((deaths/PopulationEstimate2018)*100000),cases_100k = ((cases/PopulationEstimate2018)*100000))%>%
  dplyr::select(-county.y)
head(county_merge_NY_fall)


## ------------------------------------------------------------------------------------------------------------------------------------------
# US counties with high HIV prevelence(>200) ??

library(dplyr)
county_US_HIV_200 <- county_merge_US%>%
  #filter(PopulationEstimate2018>=10000 )%>%
   #dplyr::filter(HIV.prevalence.raw.value )%>%
  dplyr::select(-state.y,-COUNTYFP,-date,-CountyName,-State.Abbreviation,-State.FIPS.Code,-Cancer.incidence.raw.value,-Children.in.poverty..Hispanic.,-PopulationEstimate65.2017,-Diabetes.prevalence.raw.value)
  #dplyr::rename(COUNTYFP=fips)
#county_US_HIV_200$county.x <- as.character(county_US_HIV_200$county.x)
#county_US_HIV_200$COUNTYFP <- as.factor(county_US_HIV_200$COUNTYFP)


head(county_US_HIV_200)


## ------------------------------------------------------------------------------------------------------------------------------------------
###Florida##

#cleaning up three phases data file of florida removing extra and duplicate covariates after merging 

# FL counties spring data (after removing some covariates)

county_fl_200_spring <- county_merge_FL %>%
  dplyr::select(-state.y,-COUNTYFP,-date,-CountyName,-State.Abbreviation,-State.FIPS.Code,-Cancer.incidence.raw.value,-Children.in.poverty..Hispanic.,-PopulationEstimate65.2017,-Diabetes.prevalence.raw.value)

head(county_fl_200_spring)


## ------------------------------------------------------------------------------------------------------------------------------------------

# FL counties summer data (after removing some covariates - second phase-07/01/2020-09/30/2020)

county_fl_200_summer <- county_merge_FL_summer %>%
  dplyr::select(-state.y,-state,-COUNTYFP,-county,-cases.x,-deaths.x,-date.x,-date.y,-cases.y,-deaths.y,-CountyName,-State.Abbreviation,-State.FIPS.Code,-Cancer.incidence.raw.value,-Children.in.poverty..Hispanic.,-PopulationEstimate65.2017,-Diabetes.prevalence.raw.value)

head(county_fl_200_summer)


## ------------------------------------------------------------------------------------------------------------------------------------------
# FL counties fall data (after removing some covariates -third phase-10/01/2020  - 01/15/2021)

county_fl_200_fall <- county_merge_FL_fall %>%
  dplyr::select(-state.y,-state,-COUNTYFP,-county,-cases.x,-deaths.x,-date.x,-date.y,-cases.y,-deaths.y,-CountyName,-State.Abbreviation,-State.FIPS.Code,-Cancer.incidence.raw.value,-Children.in.poverty..Hispanic.,-PopulationEstimate65.2017,-Diabetes.prevalence.raw.value)

head(county_fl_200_fall)


## ------------------------------------------------------------------------------------------------------------------------------------------
# FL (ALL THREE PHASES CFR) (merging FL spring, FL summer and FL fall files)

FL_all_phases <- merge(county_fl_200_summer[ ,c(2,52)],county_fl_200_fall[ ,c(2,52,25,28)],by.x = "county.x",by.y = "county.x")
FL_all_phases
FL_phases <- merge(county_fl_200_spring[ ,c(1,54)],FL_all_phases,by.x = "county.x",by.y ="county.x",all.x=TRUE,all.y =TRUE)
FL_phases[is.na(FL_phases)] <- 0
FL_phases


## ------------------------------------------------------------------------------------------------------------------------------------------
#FL final matrix for all three phases (cases= spring,cases.x = summer,cases.y=fall)

FL_final_matrix_1 <- merge(county_fl_200_summer,county_fl_200_fall[ ,c(2,4,5,52,53,54)],by.x = "county.x",by.y = "county.x",all.x = TRUE, all.y = TRUE)
FL_final_matrix_1
FL_final_matrix <- merge(county_fl_200_spring[ ,c(1,4,5,6,7,54,55,56)],FL_final_matrix_1,by.x = "county.x", by.y = "county.x",all.x = TRUE, all.y = TRUE)
FL_final_matrix
#summary(FL_final_matrix)


## ------------------------------------------------------------------------------------------------------------------------------------------
#Writing final Florida matrix file to csv file (FL_allphase_matrix) for regression analysis and graphical model (****)

#FL counties entire matrix(writing CSV)

write.csv(FL_final_matrix,'FL_allphase_matrix.csv')


## ------------------------------------------------------------------------------------------------------------------------------------------
##NY counties (after removing some covariates)

county_ny_200 <- county_merge_NY %>%
  dplyr::select(-state.y,-COUNTYFP,-date,-CountyName,-State.Abbreviation,-State.FIPS.Code,-Cancer.incidence.raw.value,-Children.in.poverty..Hispanic.,-PopulationEstimate65.2017,-Diabetes.prevalence.raw.value)

head(county_ny_200)
  


## ------------------------------------------------------------------------------------------------------------------------------------------
# NY counties spring data first phase ( after removing some covariates) 

county_ny_200_spring <- county_merge_NY_spring %>%
  dplyr::select(-state.y,-COUNTYFP,-date,-CountyName,-State.Abbreviation,-State.FIPS.Code,-Cancer.incidence.raw.value,-Children.in.poverty..Hispanic.,-PopulationEstimate65.2017,-Diabetes.prevalence.raw.value)

(county_ny_200_spring)


## ------------------------------------------------------------------------------------------------------------------------------------------
#writting file county_ny_200_spring as NY_firstphase (****)

write.csv(county_ny_200_spring,'NY_firstphase.csv')


## ------------------------------------------------------------------------------------------------------------------------------------------
NY_spring <- (county_ny_200_spring[ ,c(1,30,54,14,27,18,22,24,53)])
head(NY_spring)



## ------------------------------------------------------------------------------------------------------------------------------------------
NY_spring_age65 <- NY_spring[with(NY_spring,order(-X..65.and.older.raw.value)), ]
head(NY_spring_age65)
NY_spring_CFR <- NY_spring[with(NY_spring,order(-CFR)), ]
head(NY_spring_CFR)

#summary(NY_spring$CFR)
#summary(NY_spring$X..65.and.older.raw.value)



## ------------------------------------------------------------------------------------------------------------------------------------------
# NY counties summer data second phase( after removing some covariates)

county_ny_200_summer <- county_merge_NY_summer %>%
  dplyr::select(-state.y,-state,-COUNTYFP,-county,-cases.x,-deaths.x,-date.x,-date.y,-cases.y,-deaths.y,-CountyName,-State.Abbreviation,-State.FIPS.Code,-Cancer.incidence.raw.value,-Children.in.poverty..Hispanic.,-PopulationEstimate65.2017,-Diabetes.prevalence.raw.value)

head(county_ny_200_summer)


## ------------------------------------------------------------------------------------------------------------------------------------------
#writing CSV file (NY-second phase) as NY_secondphase (****)

write.csv(NY_secondphase.csv')


## ------------------------------------------------------------------------------------------------------------------------------------------
# Final matrix data for NY counties (including all three phases CFR (spring),CFR.x(summer),CFR.y(fall))

NY_final_matrix_1 <- merge(county_ny_200_summer,county_ny_200_fall[ ,c(2,4,5,52,53,54)],by.x = "county.x",by.y = "county.x",all.x = TRUE, all.y = TRUE)
NY_final_matrix_1
NY_final_matrix <- merge(county_ny_200_spring[ ,c(1,4,5,6,7,54,55,56)],NY_final_matrix_1,by.x = "county.x", by.y = "county.x",all.x = TRUE, all.y = TRUE)
NY_final_matrix



## ------------------------------------------------------------------------------------------------------------------------------------------
#NY counties entire matrix file NY_final_matrix(writing CSV file) for univariate analysis and graphical model analysis as NY_allphase_matrix (****)

write.csv(NY_final_matrix,'NY_allphase_matrix.csv')



## ------------------------------------------------------------------------------------------------------------------------------------------
#Ny counties-third phase file (writing csv file) (****)

write.csv(NY_thirdphase.csv)





#_______________________________________________________________________end___________________________________________________________________

