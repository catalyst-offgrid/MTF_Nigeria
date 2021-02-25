# Nigeria MTF
library(tidyverse)
library(tidyr)
library(RCMIP5)
library(haven)
library(readstata13)
library(foreign)
library(sjlabelled)
library(expss)
library(seplyr)
library(fastDummies)
library(purrr)
setwd("~/Catalyst Off-Grid Advisors/Access Insights Platform/raw-data (1)/TO UPLOAD")

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
merged_StataList <- lapply(list.files(pattern="*.dta",recursive=FALSE, full.names=TRUE), read.dta13, nonint.factors = TRUE)

# View the first data set, "MTF_HH_SEC_C_BATTERY.dta"
# test returns the sub dataset from the list, test2 returns the dataset with questions tagged.
test <- data.frame(merged_StataList[[1]])
test2 <- haven_read('MTF_HH_SEC_C_BATTERY.dta')

# Try to combine list of 33 elements - DO NOT RUN
test2 <- data.frame(matrix(unlist(merged_StataList), nrow=2, byrow=TRUE), stringsAsFactors=FALSE)
rm(test2)




