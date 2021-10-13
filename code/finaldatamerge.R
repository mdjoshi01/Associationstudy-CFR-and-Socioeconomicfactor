library(tidyverse)

## Merge county health ranking, county Yu's group abridge data with NYT covid data

## data folder

## Reading NYtimes data from github (****)

county_nytimesdata <- read.csv("us-counties.csv",header = TRUE)
head(county_nytimesdata)
tail(county_nytimesdata)

## NY times dataset fips code in NA for New York City county
## Replacing fips code for new-york-city county (NA to 36061)

county_nytimes_mod <- county_nytimesdata %>% dplyr::filter(state=="New York")
county_nytimes_mod$fips[county_nytimes_mod$county=="New York City"] <- 36061

head(county_nytimes_mod)
summary(county_nytimes_mod$fips)
dim(county_nytimes_mod)

## There are 10 rows with "unknown" county, they were assigned the New York fips
county_nytimes_mod[county_nytimes_mod$county=="Unknown",];

## Arrange the county data by fips
county_US <- county_nytimesdata%>%
  group_by(fips)%>%
  arrange(fips)%>%
  drop_na(fips)

## Pad the fips
county_US$fips<-str_pad(county_US$fips,width = 5,side = "left", pad = "0")
head(county_US)
tail(county_US)
dim(county_US)

## NY counties
county_NY <- county_nytimes_mod %>%
  group_by(fips)%>%
  arrange(fips)%>%
  drop_na(fips)

county_NY$fips<-str_pad(county_NY$fips,width = 5,side = "left", pad = "0")
head(county_NY)
tail(county_NY)
dim(county_NY)

## ------------------------------------------------------------------------------------------------------------------------------------------
#cumulative death and cases for FL counties till date 2020-06-30 (county_FL_cum_spring)





county_FL_cum_spring <- county_US %>% dplyr::filter(state=="Florida", date == "2020-06-30");

head(county_FL_cum_spring)
dim(county_FL_cum_spring)

county_FL_cum_spring_b <- county_US %>% dplyr::filter(state=="Florida", date == "2020-06-30", deaths==0);
county_FL_cum_spring_b

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

dim(county_FL_cum_spring)
dim(county_FL_summer)
dim(county_FL_fall)



## ------------------------------------------------------------------------------------------------------------------------------------------
## NewYork


## ------------------------------------------------------------------------------------------------------------------------------------------
## cumulative death and cases for NY counties for date 2020-05-31 (County_NY_cum_spring)
## county_NY_cum_spring <- filter(county_NY, date =="2020-05-31")%>%
##   filter(cases>1,deaths>1)%>%
##   dplyr::mutate( log(deaths),log(cases))


county_NY_cum_spring <- filter(county_NY, date =="2020-05-31");

head(county_NY_cum_spring)
dim(county_NY_cum_spring)

county_NY_cum_spring_b <- county_US %>% dplyr::filter(state=="New York", date == "2020-05-31", deaths==0);
county_NY_cum_spring_b


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
dim(county_NY_summer)
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
dim(county_NY_fall)


## ------------------------------------------------------------------------------------------------------------------------------------------
# Reading county health ranking file for all US counties (****)
#health ranking file
counties_healthrank <- read.csv("counties_healthrank.csv", header = TRUE)
head(counties_healthrank)
tail(counties_healthrank)
#names(unique(counties_healthrank))

county_hrank <- dplyr::select(counties_healthrank,State.Abbreviation,State.FIPS.Code,X5.digit.FIPS.Code,Name,Poor.or.fair.health.raw.value,Adult.smoking.raw.value,Adult.obesity.raw.value,Physical.inactivity.raw.value,Excessive.drinking.raw.value,Sexually.transmitted.infections.raw.value,Uninsured.raw.value,Diabetes.monitoring.raw.value,High.school.graduation.raw.value,Some.college.raw.value,Unemployment.raw.value,Children.in.poverty.raw.value,Children.in.poverty..Hispanic.,Income.inequality.raw.value,Air.pollution...particulate.matter.raw.value,Severe.housing.problems.raw.value,Diabetes.prevalence.raw.value,HIV.prevalence.raw.value,Food.insecurity.raw.value,Limited.access.to.healthy.foods.raw.value,Uninsured.adults.raw.value,Median.household.income.raw.value,Population.raw.value,X..below.18.years.of.age.raw.value,X..65.and.older.raw.value,X..Non.Hispanic.African.American.raw.value,X..American.Indian.and.Alaskan.Native.raw.value,X..Asian.raw.value,X..Native.Hawaiian.Other.Pacific.Islander.raw.value,X..Hispanic.raw.value,X..Non.Hispanic.white.raw.value,Cancer.incidence.raw.value,Poverty.raw.value,Communicable.disease.raw.value,Drug.overdose.deaths.raw.value,Food.insecurity.raw.value,Violent.crime.raw.value,Frequent.mental.distress.raw.value)%>%dplyr::rename(fips = X5.digit.FIPS.Code,county=Name)
county_hrank$fips<-str_pad(county_hrank$fips,width = 5,side = "left", pad = "0")

head(county_hrank,n=100)

## rename(county_hrank,fips = X5.digit.FIPS.Code,county = Name)


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
dim(county_merge_NY_spring)


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
dim(county_merge_NY_summer)

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
dim(county_merge_NY_fall)



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
head(county_merge_FL_1)

county_merge_FL <- county_merge_FL_1%>%
  dplyr::mutate(CFR=((deaths/cases)*100), death_100k = ((deaths/PopulationEstimate2018)*100000),cases_100k = ((cases/PopulationEstimate2018)*100000))%>%
  dplyr::select(-county.y)
head(county_merge_FL)
dim(county_merge_FL)


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
dim(county_merge_FL_summer)


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
dim(county_merge_FL_fall)

## Remove unused covariates


# FL counties spring data (after removing some covariates)

county_fl_spring <- county_merge_FL %>%
  dplyr::select(-state.y,-COUNTYFP,-date,-CountyName,-State.Abbreviation,-State.FIPS.Code,-Cancer.incidence.raw.value,-Children.in.poverty..Hispanic.,-PopulationEstimate65.2017,-Diabetes.prevalence.raw.value)

head(county_fl_spring)
dim(county_fl_spring)


## ------------------------------------------------------------------------------------------------------------------------------------------

# FL counties summer data (after removing some covariates - second phase-07/01/2020-09/30/2020)

county_fl_summer <- county_merge_FL_summer %>%
  dplyr::select(-state.y,-state,-COUNTYFP,-county,-cases.x,-deaths.x,-date.x,-date.y,-cases.y,-deaths.y,-CountyName,-State.Abbreviation,-State.FIPS.Code,-Cancer.incidence.raw.value,-Children.in.poverty..Hispanic.,-PopulationEstimate65.2017,-Diabetes.prevalence.raw.value)

head(county_fl_summer)


## ------------------------------------------------------------------------------------------------------------------------------------------
# FL counties fall data (after removing some covariates -third phase-10/01/2020  - 01/15/2021)

county_fl_fall <- county_merge_FL_fall %>%
  dplyr::select(-state.y,-state,-COUNTYFP,-county,-cases.x,-deaths.x,-date.x,-date.y,-cases.y,-deaths.y,-CountyName,-State.Abbreviation,-State.FIPS.Code,-Cancer.incidence.raw.value,-Children.in.poverty..Hispanic.,-PopulationEstimate65.2017,-Diabetes.prevalence.raw.value)

head(county_fl_fall)


## ------------------------------------------------------------------------------------------------------------------------------------------
#FL final matrix for all three phases (cases= spring,cases.x = summer,cases.y=fall)
id.cols = c(2, 4, 5, 52, 53, 54);
colnames(county_fl_fall)[id.cols]

FL_final_matrix_1 <- merge(county_fl_summer,county_fl_fall[ ,c("county.x", "cases", "deaths", "CFR", 
                                                               "death_100k", "cases_100k")],
                           by.x = "county.x",by.y = "county.x",all.x = TRUE, all.y = TRUE)
FL_final_matrix_1
colnames(FL_final_matrix_1)

## id.cols = c(1, 4, 5, 6, 7, 54, 55, 56);
## id.cols = c(1, 2, 3, 4, 5, 52, 53, 54);
## colnames(county_fl_spring)[id.cols]

FL_final_matrix <- merge(county_fl_spring[ , c("county.x", "cases", "deaths", "CFR",  "death_100k", "cases_100k")],
                         FL_final_matrix_1,by.x = "county.x", by.y = "county.x",all.x = TRUE, all.y = TRUE)
FL_final_matrix

dim(FL_final_matrix)
colnames(FL_final_matrix)


## ------------------------------------------------------------------------------------------------------------------------------------------
#Writing final Florida matrix file to csv file (FL_allphase_matrix) for regression analysis and graphical model (****)

#FL counties entire matrix(writing CSV)

write.csv(FL_final_matrix,'FL_allphase_matrix.csv')


## ------------------------------------------------------------------------------------------------------------------------------------------
# NY counties spring data first phase ( after removing some covariates) 

county_ny_200_spring <- county_merge_NY_spring %>%
  dplyr::select(-state.y,-COUNTYFP,-date,-CountyName,-State.Abbreviation,-State.FIPS.Code,-Cancer.incidence.raw.value,-Children.in.poverty..Hispanic.,-PopulationEstimate65.2017,-Diabetes.prevalence.raw.value)

head(county_ny_200_spring)
dim(county_ny_200_spring)


## ------------------------------------------------------------------------------------------------------------------------------------------
# NY counties summer data second phase( after removing some covariates)

county_ny_200_summer <- county_merge_NY_summer %>%
  dplyr::select(-state.y,-state,-COUNTYFP,-county,-cases.x,-deaths.x,-date.x,-date.y,-cases.y,-deaths.y,-CountyName,-State.Abbreviation,-State.FIPS.Code,-Cancer.incidence.raw.value,-Children.in.poverty..Hispanic.,-PopulationEstimate65.2017,-Diabetes.prevalence.raw.value)

head(county_ny_200_summer)
dim(county_ny_200_summer)

## ------------------------------------------------------------------------------------------------------------------------------------------
# NY counties fall data second phase( after removing some covariates)

county_ny_200_fall <- county_merge_NY_fall %>%
  dplyr::select(-state.y,-state,-COUNTYFP,-county,-cases.x,-deaths.x,-date.x,-date.y,-cases.y,-deaths.y,-CountyName,-State.Abbreviation,-State.FIPS.Code,-Cancer.incidence.raw.value,-Children.in.poverty..Hispanic.,-PopulationEstimate65.2017,-Diabetes.prevalence.raw.value)

head(county_ny_200_fall)
dim(county_ny_200_fall)


## ------------------------------------------------------------------------------------------------------------------------------------------
# Final matrix data for NY counties (including all three phases CFR (spring),CFR.x(summer),CFR.y(fall))
NY_final_matrix_1 <- merge(county_ny_200_summer,county_ny_200_fall[ ,c("county.x", "cases", "deaths", "CFR",  "death_100k", "cases_100k")],by.x = "county.x",by.y = "county.x",all.x = TRUE, all.y = TRUE)
NY_final_matrix_1
NY_final_matrix <- merge(county_ny_200_spring[ ,c("county.x", "cases", "deaths", "CFR",  "death_100k", "cases_100k")],
                         NY_final_matrix_1,by.x = "county.x", by.y = "county.x",all.x = TRUE, all.y = TRUE)
NY_final_matrix
write.csv(NY_final_matrix,'NY_allphase_matrix.csv')

