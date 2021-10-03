# Nigeria MTF
library(tidyverse)
library(tidyr)
#library(RCMIP5)
library(haven)
library(readstata13)
library(foreign)
library(sjlabelled)
library(expss)
library(seplyr)
library(fastDummies)
library(purrr)
setwd("~/Catalyst/MTF_Nigeria/src/data_processing/raw_data") #Change file path for personal use

haven_read <- function(file_name, read_factor = 1){
  if(read_factor == 1){
    df <- haven::as_factor(haven::read_dta(file_name))
  }else{
    df <- haven::read_dta(file_name)
  }
  names(df) <- tolower(names(df))
  return(df)

}
stata_read <- function(file_name){
  df <- readstata13::read.dta13(file_name)
  names(df) <- tolower(names(df))
  return(df)

}
source_use_freq <- function(test_str, var_name){
  freqTemp <-
    mtf2[,str_detect(question_labs, test_str)] %>%
    rename(var_name = 1)
  freqTemp <- cbind(mtf_info,freqTemp)
  freqTemp %>%
    group_by(!! sym(var_name)) %>%
    summarise(total = n())
  return(freqOut)
}

# Read STATA files and combine into a Large List
StataList <- list.files(pattern = '*.dta')
merged_StataList <- lapply(list.files(pattern="*.dta",recursive=FALSE, full.names=TRUE), readstata13::read.dta13, nonint.factors = TRUE)
nga_dta<- haven_read('NGA.dta')

# names(nga_dta)[names(nga_dta) == "hh_id"] <- "HH_ID"
nga_dta <- nga_dta %>%
  mutate(locality = ifelse(nga_dta$rur == 'Rural', 1, 0))
elc_aggr_tier <- nga_dta[,c(16,22,1)]

# View the first data set, "MTF_HH_SEC_C_BATTERY.dta"
# test returns the sub dataset from the list, test2 returns the dataset with questions tagged.
test <- data.frame(merged_StataList[[33]])
#test2 <- haven_read('MTF_HH_SEC_C_BATTERY.dta')

# Try to combine list of 33 elements - DO NOT RUN
#test2 <- data.frame(matrix(unlist(merged_StataList), nrow=2, byrow=TRUE), stringsAsFactors=FALSE)
#rm(test2)

#electricity <- data.frame(merged_StataList[[8]])
electric_assets <- haven_read('MTF_NG_HH_SEC_N_ELEC_ASSET.dta') 

electric_assets <-
  list(electric_assets, elc_aggr_tier) %>%
  reduce(inner_join, by='hh_id')

write.csv(electric_assets, '~/Catalyst/MTF_Nigeria/data/nigeria_elec_assets.csv')

finance <- haven_read('MTF_NG_HH_SEC_B.dta')

finance <-
  list(finance, elc_aggr_tier) %>%
  reduce(inner_join, by='hh_id')

finance <- finance %>%
  transform(
    b17__1 = as.character(b17__1),
    b17__2 = as.character(b17__2),
    b17__3 = as.character(b17__3),
    b17__555 = as.character(b17__555)
  )

finance <- finance %>%
  mutate(account_institution= case_when(
    b17__1=='Yes' ~ 'Commercial Bank',
    b17__2=='Yes' ~ 'Cooperative Credit Union',
    b17__3=='Yes' ~'Microfinance Institution',
    b17__555=='Yes' ~ 'Other')
  )

finance$account_institution[1906] = "Commercial Bank and Microfinance Institution"

for (i in c(867, 1852, 1866, 2028, 2093, 2147, 2187, 2259, 2455, 2495, 2496, 3434)) {
  finance$account_institution[i] = "Commercial Bank and Cooperative Credit Union"
}

finance <- finance %>%
  transform(
    b19__1 = as.character(b19__1),
    b19__2 = as.character(b19__2),
    b19__555 = as.character(b19__555)
  )

finance <- finance %>%
  mutate(informal_institution= case_when(
    b19__1=='Yes' ~ 'Group Savings (Rotational)',
    b19__2=='Yes' ~ 'Group Savings (One-Time Disbursement)',
    b19__555=='Yes' ~ 'Other')
  )

finance <- finance %>%
  transform(
    b20__1 = as.character(b20__1),
    b20__2 = as.character(b20__2),
    b20__3 = as.character(b20__3),
    b20__4 = as.character(b20__4),
    b20__5 = as.character(b20__5),
    b20__6 = as.character(b20__6),
    b20__7 = as.character(b20__7),
    b20__8 = as.character(b20__8),
    b20__9 = as.character(b20__9),
    b20__10 = as.character(b20__10),
    b20__11 = as.character(b20__11),
    b20__12 = as.character(b20__12),
    b20__13 = as.character(b20__13),
    b20__555 = as.character(b20__555)
  )

finance <- finance %>%
  mutate(loan= case_when(
    b20__1=='Yes' ~ 'Commercial/Government Bank',
    b20__2=='Yes' ~ 'Cooperative Credit Union/SACCO',
    b20__3=='Yes' ~ 'Microfinance Instituion',
    b20__4=='Yes' ~ 'Rural Bank',
    b20__5=='Yes' ~ 'State Loan',
    b20__6=='Yes' ~ 'NGO',
    b20__7=='Yes' ~ 'Business Firm',
    b20__8=='Yes' ~ 'Employer',
    b20__9=='Yes' ~ 'Moneylender/Shylock',
    b20__10=='Yes' ~ 'Shop',
    b20__11=='Yes' ~ 'Relative/Friend/Neighbor',
    b20__12=='Yes' ~ 'Mobile Money Services',
    b20__13=='Yes' ~ 'Cannot get a loan/credit',
    b20__555=='Yes' ~ 'Other')
  )

electricity <- haven_read('MTF_NG_HH_SEC_C.dta') #Clean electricity data for grid access filter
electricity <- electricity %>%
  select(1, 4)

finance <- #add grid access column to finance dataset
  list(finance, electricity) %>%  
  reduce(inner_join, by='hh_id')

cooking <- haven_read('MTF_NG_HH_SEC_I_STOVE.dta') %>%
  select(c(1, 3, 33, 55))

finance <- #add grid access column to finance dataset
  list(finance, cooking) %>%  
  reduce(inner_join, by='hh_id')

write.csv(finance, '~/Catalyst/MTF_Nigeria/data/nigeria_finance.csv')
