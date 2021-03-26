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
grepl('locality', names(test), ignore.case = T)

# Try to combine list of 33 elements - DO NOT RUN
#test2 <- data.frame(matrix(unlist(merged_StataList), nrow=2, byrow=TRUE), stringsAsFactors=FALSE)
#rm(test2)

#electricity <- data.frame(merged_StataList[[8]])
electricity <- haven_read('MTF_NG_HH_SEC_C.dta')
electricity <- electricity %>%
  select(1:72) %>%
  select(-2)

electricity <- 
  list(electricity, elc_aggr_tier) %>%  
  reduce(inner_join, by='hh_id')

write.csv(electricity, '~/Catalyst/MTF_Nigeria/data/nigeria_grid_access.csv')

electricity2 <- haven_read('MTF_NG_HH_SEC_C.dta')
electricity2 <- electricity2 %>%
  select(1 | 129:287)

electricity2 <- 
  list(electricity2, elc_aggr_tier) %>%  
  reduce(inner_join, by='hh_id')

write.csv(electricity2, '~/Catalyst/MTF_Nigeria/data/nigeria_grid_access_ext.csv')

solar <- haven_read('MTF_NG_HH_SEC_C_SOLAR.dta')
solar2 <- electricity2 %>%
  select(1 | 108:111 | 137:154)

solar <- 
  list(solar, elc_aggr_tier) %>%  
  reduce(inner_join, by='hh_id')

solar <-
  list(solar, solar2) %>%
  reduce(inner_join, by='hh_id')

solar$c137[which(is.na(solar$c137))] <- 0
solar$c138[which(is.na(solar$c138))] <- 0
solar$c139[which(is.na(solar$c139))] <- 0

solar <- solar %>%
  mutate(
    percent_lantern = solar$c137/solar$sum_solar*100,
    percent_lighting = solar$c138/solar$sum_solar*100,
    percent_home = solar$c139/solar$sum_solar*100
  )

write.csv(solar, '~/Catalyst/MTF_Nigeria/data/nigeria_grid_solar.csv')

cooking <- haven_read('MTF_NG_HH_SEC_I_STOVE.dta')
cooking <- 
  list(cooking, elc_aggr_tier) %>%  
  reduce(inner_join, by='hh_id')

write.csv(cooking, '~/Catalyst/MTF_Nigeria/data/nigeria_cooking.csv')

stopgap <- haven_read('MTF_NG_HH_SEC_F.dta')
stopgap2 <- haven_read('MTF_NG_HH_SEC_F_FUEL.dta')
stopgap <- 
  list(stopgap, stopgap2, elc_aggr_tier) %>%  
  reduce(inner_join, by='hh_id')

write.csv(stopgap, '~/Catalyst/MTF_Nigeria/data/nigeria_stopgap.csv')

