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
library(readxl)
library(svDialogs)

## 00b: prepare data ------

#define global variables

 results_rhythm <<- data.frame()      # <<- global assignment operator, needs to be used when changing as well



## 00c: prepare functions
 
 #to source all files in the latest version: list.files("path_to_folder", full.names = TRUE) %>% map(source)
 # could also be useful to set path and find all files
 
 source("ioi.R")
 source("binary.R")
 source("fourier.R")
 source("ugof.R")
 source("npvi.R")
 source("recurrence.R")
 #source("batch.R")

 
 
 #example data for testing
 #data <- read_delim("data/S bil_IC beat_pup 1_call 02.csv", delim = ",", col_names = FALSE)
 
## 01: Set path ----------------
# batch processing, choose whole folder and pattern to conduct analysis on

 path <- choose.dir()
 
 pattern <- dlgInput("Enter a file pattern, like csv or xlsx", Sys.info()["user"])$res
 
 # a list of files in that folder corresponidng to the chosen pattern is made
 list_of_files <- list.files(path = path, pattern = pattern)
 
 #batch_processing() #problem mit Übergabe von pattern aus funktion, muss vielleicht hier ins main file
 


## 02: loop through all file
 
#list_of_files <- 1   #change here to list_of_files produced in 01: Set Path
 
for (a in 1:length(list_of_files)) { #:nrow(list_of_file)){
  
  #idea for here: if abfrage:  if pattern == csv/xlsx/xls/txt dann xy, else: choose a different pattern 
  if (pattern == "csv"){
  data <- read_delim(paste(path, list_of_files[a], sep = "\\"), delim  = ",", col_names = FALSE)
  } else if (pattern == "xls"){
  data <- read_xls(paste(path, list_of_files[a], sep = "\\"), sheet = 1, col_names = FALSE)
  colnames(data) <- c("X1", "X2", "X3") 
  #  } else if (pattern == "xlsx") {
  #data <- read.xlsx(paste(path, list_of_files[a], sep = "\\"), sheet = 1, colNames = FALSE)
  #colnames(data) <- c("X1", "X2", "X3") 
  } else {
    print("Please choose a different file format: either .csv or .xls")
    stop()
  }
  
  #multiple problems with the xlsx files: for now, we do not allow xlsx
  
  binary(data)
  print(paste('Binary', a, 'done', sep =" "))
  
  ioi_calc(data)
  print(paste('IOI calc', a, 'done', sep =" "))
  
  fourier_calc(binarydata)
  print(paste('Fourier calc', a, 'done', sep =" "))
  
  ugof(data = data, beat = results_rhythm[a,1], beat_2 = results_rhythm[a,3] ) # i being the looping variable through rows in the results_rhythm dataframe
   #beat_2 is an optional argument and can be set to the fft rhythm for example like this: [..]beat_2 = results_rhyth,[i,3]
  print(paste('ugof', a, 'done', sep =" "))
  
  npvi(ioi_seq)
  print(paste('npvi', a, 'done', sep =" "))
  
  recurrence(data)
  print(paste('Recurrence Plot', a, 'done', sep =" "))
}
 
 colnames(results_rhythm) <- c("ioi beat", "unbiased cv", "fft beat", "freq resolution", "n elements", "n samples", "ugof beat 1", "ugof beat 2", "npvi")

