# Nigeria MTF
library(tidyverse)
library(tidyr)
#library(RCMIP5)
library(haven)
library(readstata13)
#library(foreign)
#library(sjlabelled)
#library(expss)
library(seplyr)
#library(fastDummies)
#library(purrr)
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
grepl('locality', names(test), ignore.case = T)

# Try to combine list of 33 elements - DO NOT RUN
#test2 <- data.frame(matrix(unlist(merged_StataList), nrow=2, byrow=TRUE), stringsAsFactors=FALSE)
#rm(test2)

#electricity <- data.frame(merged_StataList[[8]])
wtp_solar <- haven_read('MTF_NG_HH_SEC_E.dta') #wtp = willingness to pay

wtp_solar <-
  list(wtp_solar, elc_aggr_tier) %>%
  reduce(inner_join, by='hh_id')

write.csv(wtp_solar, '~/Catalyst/MTF_Nigeria/data/nigeria_wtp_solar.csv')

housing_expense_30days <- haven_read("MTF_NG_HH_SEC_L_30_DAYS_EXPEN.dta")

housing_expense_30days <-
  list(housing_expense_30days, elc_aggr_tier) %>%
  reduce(inner_join, by='hh_id')

cooking <- haven_read('MTF_NG_HH_SEC_I_STOVE.dta') %>% #get primary cooking fuel and stove info
  select(c(1, 3, 33, 56))

housing_expense_30days <- #add cooking fuel and stove info
  list(housing_expense_30days, cooking) %>%  
  reduce(inner_join, by='hh_id')

write.csv(housing_expense_30days, '~/Catalyst/MTF_Nigeria/data/nga_housing_expense_30days.csv')

housing_expense_year <- haven_read("MTF_NG_HH_SEC_L_12_MONTHS_EXPEN.dta")

housing_expense_year <-
  list(housing_expense_year, elc_aggr_tier) %>%
  reduce(inner_join, by='hh_id')

housing_expense_year <- #add cooking fuel and stove info
  list(housing_expense_year, cooking) %>%  
  reduce(inner_join, by='hh_id')

write.csv(housing_expense_year, '~/Catalyst/MTF_Nigeria/data/nga_housing_expense_year.csv')

housing_expense_consumption <- haven_read("MTF_NG_HH_SEC_L_CONSUMPTION.dta")

housing_expense_consumption <-
  list(housing_expense_consumption, elc_aggr_tier) %>%
  reduce(inner_join, by='hh_id')

housing_expense_consumption <- #add cooking fuel and stove info
  list(housing_expense_consumption, cooking) %>%  
  reduce(inner_join, by='hh_id')

write.csv(housing_expense_consumption, '~/Catalyst/MTF_Nigeria/data/nga_housing_consumption.csv')

cooking_expense <- haven_read("MTF_NG_HH_SEC_K.dta")

cooking_expense <-
  list(cooking_expense, elc_aggr_tier) %>%
  reduce(inner_join, by='hh_id')

write.csv(cooking_expense, '~/Catalyst/MTF_Nigeria/data/nigeria_cooking_expense.csv')

wtp_grid <- haven_read('NG_MTF_HH_SEC_D.dta')

wtp_grid <-
  list(wtp_grid, elc_aggr_tier) %>%
  reduce(inner_join, by='hh_id')

write.csv(wtp_grid, '~/Catalyst/MTF_Nigeria/data/nga_wtp_grid.csv')

lighting <- haven_read('MTF_NG_HH_SEC_F.dta')
