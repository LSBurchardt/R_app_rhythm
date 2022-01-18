######################################################################################
# Rhythm Analysis Methods in R 
# Transfering script from Matlab to R
# author: Dr. Lara S. Burchardt
# 
# date: 08.11.2021, version: 1.0
# 
# main script
######################################################################################


##to do: -------

# 3) ioi seq extra abspeichern, so das die für art/sprache etc. zusammengefügt werden kann
# 4) recurrence plots: check theme...something changed, set aspect ratio to 1 again! tiles are not squares
# like they should be
# 5) include a savename with Abfrage just like for pattern and fs, to be included both as info in an extra column
# and for saving plots and results
# 6) save results
# 7) user questions with dropdown menu instead of free choice
# 8) ioi hist for whole dataset as extra function at the end, maybe delete ioi plot in the ioi function, doesn't make 
# much sense for individual sequences in most cases anyway
# 9) recurrence plots as interactive with ggplotly() einfügen maybe as completly new file something like "main2" to plot
# with ability to decide which one to plot, can plots be saved as R object? so that we can easily reopen them?
# then ability to zoom is given --> var1/2 can be read and numbers used as input for zoomed analysis
# so in this main2 after this "selection process" we rerun the analysis from main


## issues: -------

# - binary() is extremly slow, this is ok up to about ~50 elements, then it gets problematic
# when only running ioi calcs, then we need to change the order of functions and the order of how
# to write into the results, so that no holes result in columns that are not used when functions are left out
# that produces errors, we want to program it such, that we start with the basics and add to it optionally
# so that everything would always work
# - is there a way to check for linearity in Onsets, to prevent infinit loops in ugof calculations? 

## 00a: load packages-----------

library(tidyverse)
library(readxl)
library(openxlsx)
library(svDialogs)
library(pracma)       # for fourier.R, findpeaks
library(SynchWave)    # for fourier.R, fftshift
library(vegan)        # for recurrence.R, to calculate euclidean distance
library(corrplot)     # for recurrence.R, multiple plots
library(plotly)       # for recurrence.R, interactive recurrence plots

## 00b: prepare data ------

#define global variables

 results_rhythm <<- data.frame()      # <<- global assignment operator, needs to be used when changing as well
 ioi_all <<- list(NA)


## 00c: prepare functions
 
 #to source all files in the latest version: list.files("path_to_folder", full.names = TRUE) %>% map(source)
 # could also be useful to set path and find all files
 
 source("ioi.R")
 source("binary.R")
 source("fourier.R")
 source("ugof.R")
 source("npvi.R")
 source("recurrence.R")
 source("ioi_hist_plot.R")
 #source("batch.R")

 
 
 #example data for testing
 #data <- read_delim("data/S bil_IC beat_pup 1_call 02.csv", delim = ",", col_names = FALSE)
 
## 01: Set path ----------------
# batch processing, choose whole folder and pattern to conduct analysis on

 path <- choose.dir()
 
 answer_pattern <- c("csv", "xls")
 pattern <- dlg_list(title = "Enter file pattern", answer_pattern, multiple = FALSE)$res
 
 savename <- dlgInput("Enter a savename, i.e. dataset_x.", Sys.info()["user"])$res
 
 answer_fs_dividend <- c("1", "5", "50")
 fs_dividend <<- dlg_list(title = "FS dividend ", answer_fs_dividend, multiple = FALSE)$res #for fs 0f 1000 enter: 1, for fs 200 enter 5, for fs 20 enter 50
 fs_dividend <- as.numeric(fs_dividend)
 
 answer_options_list_produce <- c("yes", "no")
 answer_produce_plots <- dlg_list(title= "Produce Plots?", answer_options_list_produce, multiple = FALSE)$res
 # a list of files in that folder corresponidng to the chosen pattern is made
 list_of_files <- list.files(path = path, pattern = pattern)
 
 #batch_processing() #problem mit Übergabe von pattern aus funktion, muss vielleicht hier ins main file
 


## 02: loop through all file-------------
 
#list_of_files <- 1   #change here to list_of_files produced in 01: Set Path
 
for (a in 1:length(list_of_files)) { #:nrow(list_of_file)){
  
  #be aware: if you have column names: set col_names = TRUE! if not: col_names = FALSE
  
  if (pattern == "csv"){
  data <- read_delim(paste(path, list_of_files[a], sep = "\\"), delim  = ",", col_names = TRUE)
  } else if (pattern == "xls"){
  data <- read_xls(paste(path, list_of_files[a], sep = "\\"), sheet = 1, col_names = FALSE)
  colnames(data) <- c("X1", "X2", "X3") 
    } else if (pattern == "xlsx") {
  data <- read.xlsx(paste(path, list_of_files[a], sep = "\\"), sheet = 1, colNames = FALSE)
  colnames(data) <- c("X1", "X2", "X3") 
  } else {
    print("Please choose a different file format: either .csv or .xls")
    stop()
  }
  
  #multiple problems with the xlsx files: for now, we do not allow xlsx
  
  binary(data)
  print(paste('Binary', a, 'of', length(list_of_files), 'done', sep =" "))
  
  ioi_calc(data)
  ioi_all[[a]] <- ioi
  print(paste('IOI calc', a, 'of', length(list_of_files), 'done', sep =" "))
  
  fourier_calc(binarydata)
  print(paste('Fourier calc', a, 'of', length(list_of_files), 'done', sep =" "))
  
  ugof(data = data, beat = results_rhythm[a,1], beat_2 = NA )# ,beat_2 = results_rhythm[a,3] ) # i being the looping variable through rows in the results_rhythm dataframe
   #beat_2 is an optional argument and can be set to the fft rhythm for example like this: [..]beat_2 = results_rhyth,[i,3]
  print(paste('ugof', a, 'of', length(list_of_files), 'done', sep =" "))
  
  npvi(ioi_seq)
  print(paste('npvi', a, 'of', length(list_of_files), 'done', sep =" "))
  
  if(answer_produce_plots == "yes"){
  recurrence(data)
  print(paste('Recurrence Plot', a, 'of', length(list_of_files), 'done', sep =" "))
} else {print("You chose to not produce recurrence plots.")}
  
  }

# 03: postprocessing ------------- 
# include filenames
 
 filenames <- as.data.frame(list_of_files)
 results_rhythm <- cbind(results_rhythm, filenames)
 
 ioi_all <<- as.data.frame(ioi_all)
 
 ioi_hist_plot()
 
# colnames(results_rhythm) <- c("ioi beat", "unbiased cv", "fft beat", "freq resolution", "n elements", "n samples", "ugof beat 1", "ugof beat 2", "npvi")
 colnames(results_rhythm) <- c("ioi_beat", "unbiased_cv",  "npvi", "filename")

 # 04: Saving ---------------
