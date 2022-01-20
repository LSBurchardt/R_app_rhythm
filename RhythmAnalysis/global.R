# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# This app shows the pCO2, Temperature, Salinity and cO2 measurments - 
# in a given time- and coordinate window,
# chosen by the user- over time. Databasis are continous pCO2 measurments from 
# the ferry Finmaid starting in June 2003 until today. 
#
##########################################################################

#global.R

# 00: load packages -- ---------------------------------------------------------
library(shiny)
library(shinyFiles)
library(shinybusy)
library(tidyverse)
library(readxl)
library(openxlsx)
library(svDialogs)
library(pracma)       # for fourier.R, findpeaks
library(SynchWave)    # for fourier.R, fftshift
library(vegan)        # for recurrence.R, to calculate euclidean distance
library(corrplot)     # for recurrence.R, multiple plots
library(plotly)       # for recurrence.R, interactive recurrence plots

# 01: load functions -- -------------------------------------------------------------

source("ioi.R")
#source("binary.R")
#source("fourier.R")
#source("ugof.R")
#source("npvi.R")
#source("recurrence.R")
#source("ioi_hist_plot.R")

#defined in server with user input

# 02:attributes  -----------------------------------------------------------

results_rhythm <<- data.frame()      # <<- global assignment operator, needs to be used when changing as well
ioi_all <<- list(NA)

# 03: define ui --------------------------------------------------------------

#defined in ui.R

# 04: Server function --------------------------------------------------------

#defined in server.R