######################################################################################
# Rhythm Analysis Methods in R 
# Transfering script from Matlab to R
# author: Dr. Lara S. Burchardt
# 
# date: 08.11.2021, version: 1.0
# 
# fourier script
######################################################################################

fourier_calc <- function(binary_data){


## 00a: load packages-----------
library(tidyverse)
library(spectral)     # spec.fft or plot.fft (delete if not used in the end)
library(pracma)       # findpeaks
library(SynchWave)    # fftshift

## 00b: load example data ------

  #only for building/testing
  binary_data <- c(0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0)

## 01: Fourier anlysis ---------
# calculate FFT over binary sequence to calculate best fitting beat
# from Matlab
#  FS = 200;
#  X = 1/L*fftshift(fft(X,L)); %N-point complex DFT
#  df=FS/L;                    %frequency resolution
#  sampleIndex = -L/2:L/2-1;   %ordered index for FFT plot
#  f=sampleIndex*df;
 
#k = find(binarydata(:,i)); # find outputs all values unequal zero -> finds all 1's, saves indices in k
#X= binarydata(min(k):max(k),i); # X is defined as the binary sequence between the first 1 and the last 1 in the sequence
#L = length(X);
  #https://statisticsglobe.com/find-index-positions-non-zero-values-matrix-r
  
  k <- which(binary_data != 0, arr.ind = T)   # find outputs all values unequal zero -> finds all 1's, saves indices in k
  X <- binary_data[min(k):max(k)]             # X in binary data cut to start and end with 1
  L <- length(X)                              # length of actual signal 
  kk <- length(k)  #number of elements in sequence! save!
  
 
  fs <- 200; # should be soft coded to match sampling rate calculated with in "binary"
  X <- 1/L * SynchWave::fftshift(fft(X,L))
  df <- fs/L #frequency resolution
  sample_index <- (-L/2):(L/2-1) #ordered index for FFT plot
  f <- sample_index*df
  
#matlab  
#index_0 = knnsearch(f',0);  % index where f is zero to select only positive fs later
#X = X((index_0-1):end);     % select only amplitudes for positive fs
#f = f((index_0-1):end);     % select only positive fs

index_0 <- which.min(abs(f-0))
X <- X[(index_0 - 1): length(X) ]
f <- f[(index_0 - 1): length(f) ]
 
# https://stackoverflow.com/questions/14968954/return-index-from-a-vector-of-the-value-closest-to-a-given-element  
# https://www.oreilly.com/library/view/the-r-book/9780470510247/ch002-sec020.html
  
  
# find peaks in fft signal
  
peaks<- findpeaks(abs(X), npeaks = 3, sortstr = TRUE) #sortstr = TRUE so that it is sorted decreasing as need and as done in matlab
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

#account for shift in zero-bin component

#matlab
#if P1peakFreq(1,1) ~=0                          %account for shift in zero-bin component
#P1peakFreq(1,:) = P1peakFreq - P1peakFreq(1,1); % gets shifted back to 0 here
#else 
#  P1peakFreq(1,:) = P1peakFreq;
# end

if (peaks[1,3] != 0){
  
  
}


}