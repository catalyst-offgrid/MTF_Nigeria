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

cooking <- haven_read('MTF_NG_HH_SEC_I_STOVE.dta')
cooking <- 
  list(cooking, elc_aggr_tier) %>%  
  reduce(inner_join, by='hh_id')

write.csv(cooking, '~/Catalyst/MTF_Nigeria/data/nigeria_cooking.csv')
