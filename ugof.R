######################################################################################
# Rhythm Analysis Methods in R 
# Transfering script from Matlab to R
# author: Dr. Lara S. Burchardt
# 
# date: 08.11.2021, version: 1.0
# 
# ugof script
######################################################################################

ugof <- function(data_ugof, beat, beat_2 = NULL){

  #optional arguments: beat_2 = NULL
  # https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions 
  
## 00a: load packages-----------

library(tidyverse)

## 00b: load example data ------

  data_ugof <- data[,1]
  
#for testing
  beat <- results_rhythm[1,1]
  
## 01: ugof calculations -------
# calculate goodness-of-fit for IOI analysis and Fourier analysis


## 01a: ugof IOI

  maxoriginal <- max(data_ugof)
  timesteps <- 1000/round(beat, digits = 1)
  count <- 0
  theotime_value <- 0
  theotime_seq <- data.frame()
  
  while (theotime_value < maxoriginal){
  
  count <- count + 1;
  theotime_value <- count * timesteps /1000;
  theotime_seq[count,1] <- theotime_value;
  
  }
  
## 01b: ugof Fourier


## 02: ugof modelling ---------

## 02a: modelling

## 02b: z-scores

## 02c: p-values

## 02d: +- 1 Hz (lowest priotity)
  
}