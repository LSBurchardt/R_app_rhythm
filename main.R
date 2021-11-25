######################################################################################
# Rhythm Analysis Methods in R 
# Transfering script from Matlab to R
# author: Dr. Lara S. Burchardt
# 
# date: 08.11.2021, version: 1.0
# 
# main script
######################################################################################

## 00a: load packages-----------

library(tidyverse)

## 00b: prepare data ------

#define global variables

 results_rhythm <<- data.frame()      # <<- global assignment operator, needs to be used when changing as well



## 00c: prepare functions
 
 #to source all files in the latest version: list.files("path_to_folder", full.names = TRUE) %>% map(source)
 # could also be useful to set path and find all files
 
 source("ioi.R")
 source("binary.R")
 source("fourier.R")
 
 data <- read_delim("data/S bil_IC beat_pup 1_call 02.csv", delim = ",", col_names = FALSE)
 
## 01: Set path ----------------
# batch processing, choose whole folder to conduct analysis


## 02: loop through all file
 
list_of_file <- 1   #change here to list_of_files produced in 01: Set Path
 
for (a in 1) { #:nrow(list_of_file)){
  
  binary(data)
  ioi_calc(data)
  fourier_calc(binarydata)
  #ugof(data = data, beat = results_rhythm[i,1] ) # i being the looping variable through rows in the results_rhythm dataframe
  # beat_2 is an optional argument and can be set to the fft rhythm for example like this: [..]beat_2 = results_rhyth,[i,3]
  
}
 
 colnames(results_rhythm) <- c("ioi beat", "unbiased cv", "fft beat", "freq resolution", "n elements", "n samples")

