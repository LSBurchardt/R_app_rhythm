######################################################################################
# Rhythm Analysis Methods in R 
# Transfering script from Matlab to R
# author: Dr. Lara S. Burchardt
# 
# date: 08.11.2021, version: 1.0
# 
# IOI analysis script
######################################################################################

ioi_calc <- function(data){

## 00a: load packages-----------

library(tidyverse)

## 00b: prepare data ------

#data <- read_delim("data/S bil_IC beat_pup 1_call 02.csv", delim = ",", col_names = FALSE)

#results_rhythm <<- data.frame()

#reads in the data from a .csv file, without column headers use option col_names = FALSE

## 01: IOI analysis-------------
# calculate IOI rhythm over original sequence to calculate best fitting beat


# calculate iois from start and end times

ioi <- data.frame()                 # set up empty dataframe to store ioi values in

#for (a in filenumber){             #start of loop for number of files, needs to be added, maybe better add in main! 

for (x in  1:nrow(data)) {          # start of loop through rows of data to calculate iois

z = x+1 
ioi[x,1] <- data[z,1]-data[x,1]

}                                   #end of loop through rows of data to calculate iois

colnames(ioi) <- c("X1")

ioi_beat <- 1/mean(ioi$X1, na.rm = TRUE) # calculate mean of iois

ioi_cv <- sd(ioi$X1, na.rm = TRUE)/mean(ioi$X1, na.rm = TRUE)
ioi_cv_unbiased <-  (1+1/(4*(nrow(ioi)-1)))*ioi_cv


#add parameters to results

results_rhythm[a,1] <<- ioi_beat 
results_rhythm[a,2] <<- ioi_cv_unbiased

## 02: plots ---------------

# ioi needs to be the complete set of iois, from all datasets not just one
# needs to be changed, once batch processing is working

ggplot(data= ioi, aes(x= X1))+
  geom_histogram(color = "white", fill = "darkblue", na.rm = TRUE)+               #change binwidth here
  aes(y=stat(count)/sum(stat(count))*100) +     # y is shown in percentages
  xlab("IOI [sec]")+                            
  ylab("Percentage [%]")

# save plot in folder, needs to use a changing name depending on loop once
# batch processing works

ggsave(paste('plots/histogram_', a, '.jpg', sep = ""),
       dpi =300 , 
       device = "jpg")


#} #end of loop for number of files, needs to be added
ioi_seq <<- ioi #to be used in npvi and recurrence plot calculations, global variable 

}
