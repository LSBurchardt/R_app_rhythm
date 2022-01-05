######################################################################################
# Rhythm Analysis Methods in R 
# Transfering script from Matlab to R
# author: Dr. Lara S. Burchardt
# 
# date: 08.11.2021, version: 1.0
# 
# fourier script
######################################################################################

fourier_calc <- function(binarydata){


## 00a: load packages-----------
library(tidyverse)
library(pracma)       # findpeaks
library(SynchWave)    # fftshift

## 00b: load example data ------

  #only for building/testing
 # binarydata <- c(0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0)

## 01: Fourier anlysis ---------

  #https://statisticsglobe.com/find-index-positions-non-zero-values-matrix-r
  
  k <- which(binarydata != 0, arr.ind = T)   # find outputs all values unequal zero -> finds all 1's, saves indices in k
  X <- binarydata$timeseries[min(k[,1]):max(k[,1])]             # X in binary data cut to start and end with 1
  L <- length(X)                              # length of actual signal 
  kk <- nrow(k)                             #number of elements in sequence! save!
  
 
  fs <- 200; # should be soft coded to match sampling rate calculated with in "binary"
  X <- 1/L * SynchWave::fftshift(fft(X,L))
  df <- fs/L #frequency resolution
  sample_index <- (-L/2):(L/2-1) #ordered index for FFT plot
  f <- sample_index*df
  

  index_0 <- which.min(abs(f-0))      #index where f is zero to select only positive fs 
  X <- X[(index_0 - 1): length(X) ]   # select only amplitudes for positive fs
  f <- f[(index_0 - 1): length(f) ]   # select only positive fs
 
# https://stackoverflow.com/questions/14968954/return-index-from-a-vector-of-the-value-closest-to-a-given-element  
# https://www.oreilly.com/library/view/the-r-book/9780470510247/ch002-sec020.html
  
  
# find peaks in fft signal
  
peaks<- findpeaks(abs(X), npeaks = 11, sortstr = TRUE) #sortstr = TRUE so that it is sorted decreasing as need and as done in matlab
peaks <- peaks %>%      # column 1: amplitude/peak height, column 2: index, column 3 and 4 start and end of peak, delted, because we do not need it
  as.data.frame() %>% 
  select(V1, V2)

colnames(peaks) <- c("amplitude", "index") 
  
# we know amplitude and index but not yet the corresponding frequency
# we now need to couple index with corresponding frquency from f

# f at indexes in peaks$index
peak_freq <- f[peaks$index]
# add frequency to the amplitude dataframe
peaks[,"freq"] <- peak_freq

#Account for shift in zero-bin component

if (peaks[1,3] != 0){
  
  peaks$freq <- peaks$freq - peaks$freq[1]
  
}

#save data

results_rhythm[a,3] <<- peaks$freq[2] #change row index to soft coding once loops are in place [i,3]
results_rhythm[a,4] <<- df            # frequency resolution
results_rhythm[a,5] <<- kk            # number of elements 
results_rhythm[a,6] <<- L             # length of signal

}