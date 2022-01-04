######################################################################################
# Rhythm Analysis Methods in R 
# Transfering script from Matlab to R
# author: Dr. Lara S. Burchardt
# 
# date: 08.11.2021, version: 1.0
# 
# batch processing
######################################################################################

batch_processing <- function(){

## 00: load packages-----------

library(tidyverse)
library(svDialogs)

## 01: establish vector with all data, that is to be processed
  
 
  # path is chosen from windows explorer by the researcher
  path <- choose.dir()
  
  pattern <<- dlgInput("Enter a file pattern, like csv or xlsx", Sys.info()["user"])$res
  
  # a list of files in that folder corresponidng to the chosen pattern is made
  files <<- list.files(path = path, pattern = pattern)

  
  
 
  
}