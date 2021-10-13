library(tidyverse);

## set working directory to point to the data folder

## Shorten covariate names
shorten_names = function(names) {
  names = str_replace(names, ".raw.value", "")
  names = str_replace(names, "X..", "")
  names[names=="65.and.older"] = "Age.65.and.older";
  names
}

## 2021-09-09
##
## Regress <response> against each covariate specified in <predictors> using the
## provided data_matrix
## 
## Return the univariate regression coefficient and its se, test statistic for
## the predictor, p-value, and df of the model from all regressions (one row for
## each predictor)
fit.univariate.regression = function(response, predictors, data_matrix) {
  res = lapply(predictors, function(x){
    m = lm(as.formula(paste(response, "~", x)), data = data_matrix);
    s = summary(m);
    c(coef(s)[2,], df=s$df[2])
  })
  
  res1 = t(matrix(unlist(res), 5, 21));
  colnames(res1) = names(res[[1]]);
  rownames(res1)= shorten_names(predictors)
  res1
}

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

## Try regression with one of the covariates
m = lm(as.formula(paste("CFR ~", "X..Asian.raw.value")), data = FL_reg_finalmatrix);
s = summary(m) 
coef(s);

## Run univariate regression against all covariates
FL_res1 = fit.univariate.regression("CFR", list_data_2, FL_reg_finalmatrix);
FL_res2 = fit.univariate.regression("CFR.x", list_data_2, FL_reg_finalmatrix);
FL_res3 = fit.univariate.regression("CFR.y", list_data_2, FL_reg_finalmatrix);


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

all.equal(list_data_4, list_data_2)

## ------------------------------------------------------------------------------------------------------------------------------------------
###New-York univariate regression where data file includes all counties except New-York city
#Data file of all counties in New-York except New York City
NY_reg_finalmatrix[29, ]$county.x

## Try regression with one of the covariates
m = lm(as.formula(paste("CFR ~ ", "X..Asian.raw.value")), data = NY_reg_finalmatrix[-29,]);
s = summary(m) 
coef(s);

NY_res1 = fit.univariate.regression("CFR", list_data_4, NY_reg_finalmatrix[-29,]);
NY_res2 = fit.univariate.regression("CFR.x", list_data_4, NY_reg_finalmatrix[-29,]);
NY_res3 = fit.univariate.regression("CFR.y", list_data_4, NY_reg_finalmatrix[-29,]);

## ------------------------------------------------------------------------------------------------------------------------------------------
## Selected variables file for graphical model analysis without newyork city county#

#Newyork first phase data file for graphical model analysis(all New York counties without New York City)
# counties where CFR with NA values are deleted from data file in first phase and missing value of variable is replaced by min value of that variable

NY_nonyc <- NY_reg_finalmatrix[-29, ]%>%
  dplyr::select("CFR","X..65.and.older.raw.value", "Uninsured.adults.raw.value","Median.household.income.raw.value","HIV.prevalence.raw.value","Unemployment.raw.value","Adult.obesity.raw.value","Excessive.drinking.raw.value","Severe.housing.problems.raw.value" , 
    "X..below.18.years.of.age.raw.value" , "StrokeMortality" ,"RespMortalityRate2014", 
    "HeartDiseaseMortality" , "DiabetesPercentage", "SVIPercentile", 
    "Adult.smoking.raw.value", "X..Non.Hispanic.African.American.raw.value", 
    "X..American.Indian.and.Alaskan.Native.raw.value","X..Asian.raw.value" ,
    "X..Native.Hawaiian.Other.Pacific.Islander.raw.value" , "X..Hispanic.raw.value", 
    "X..Non.Hispanic.white.raw.value")

## Fill NA HIV prevalence with minimum value 
NY_nonyc$HIV.prevalence.raw.value[is.na(NY_nonyc$HIV.prevalence.raw.value)] <- min(NY_nonyc$HIV.prevalence.raw.value,na.rm = TRUE)
## NY_nonyc <-NY_nonyc %>%  drop_na(CFR)

## Shorten covariate names
colnames(NY_nonyc) = shorten_names(colnames(NY_nonyc));

head(NY_nonyc)
dim(NY_nonyc)

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

## Shorten covariate names
colnames(NY_nonyc_2_phase) = shorten_names(colnames(NY_nonyc_2_phase));
colnames(NY_nonyc_2_phase)[1] = "CFR";
colnames(NY_nonyc_2_phase)

## NY_nonyc_2_phase <-NY_nonyc_2_phase %>%  drop_na(CFR.x)
summary(NY_nonyc_2_phase)
dim(NY_nonyc_2_phase)


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
## NY_nonyc_3_phase <-NY_nonyc_3_phase %>%  drop_na(CFR.y)
## NY_nonyc_3_phase

## Shorten covariate names
colnames(NY_nonyc_3_phase) = shorten_names(colnames(NY_nonyc_3_phase));
colnames(NY_nonyc_3_phase)[1] = "CFR";
colnames(NY_nonyc_3_phase)

summary(NY_nonyc_3_phase)
dim(NY_nonyc_3_phase)


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
## FL_1_phase <-FL_1_phase %>%  drop_na(CFR)
any(is.na(FL_1_phase))

colnames(FL_1_phase) = shorten_names(colnames(FL_1_phase))
summary(FL_1_phase)

dim(FL_1_phase)

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

any(is.na(FL_2_phase))

colnames(FL_2_phase) = shorten_names(colnames(FL_2_phase))
colnames(FL_2_phase)[1] = "CFR"
summary(FL_2_phase)

dim(FL_2_phase)


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

any(is.na(FL_3_phase))


colnames(FL_3_phase) = shorten_names(colnames(FL_3_phase))
colnames(FL_3_phase)[1] = "CFR"
summary(FL_3_phase)

dim(FL_3_phase)

## ------------------------------------------------------------------------------------------------------------------------------------------
#Writing file for tetrad graphical analysis third phase of Florida (CFR and 21 variables)
write.csv(FL_3_phase,'FL_phase_3.csv')

