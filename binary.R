######################################################################################
# Rhythm Analysis Methods in R 
# Transfering script from Matlab to R
# author: Dr. Lara S. Burchardt
# 
# date: 08.11.2021, version: 1.0
# 
# binary script
######################################################################################

binary <- function(data){

## 00a: load packages-----------

library(tidyverse)

## 00b: load example data ------

#only for building/testing otherwise read in in main.R

#data <- read_delim("data/S bil_IC beat_pup 1_call 02.csv", delim = ",", col_names = FALSE)


## 01:  Binary -----------------
# transform time labels to binary sequence


event_timepoint <- vector(length = nrow(data))
event_timepoint <- data [,1]

event_timepoint_fs <- round(event_timepoint*1000/5) # index of events with fs 200 Hz
# see whether we can soft code fs somehow and include it as necessary input argument for binary function

event_timepoint_fs <- as.data.frame(event_timepoint_fs)

binarydata <<- data.frame(matrix(ncol = 1, nrow = max(event_timepoint_fs), 0))
colnames(binarydata) <<- c('timeseries')


for (l in 1: max(event_timepoint_fs)){
  #print(l)
  for ( i in 1: nrow(event_timepoint_fs)){
    #print(i)
    if (l == event_timepoint_fs[i,1]){
      
      binarydata[l,1] <<- 1 
      
    }         # end of if  
    
  }          # end of for loop i
  
}           #end of for loop l

} # end of function

