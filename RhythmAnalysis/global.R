# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# 
# With this app you can analyse the temporal structure or rhythm of a timeseries
# i.e. animal vocalizations
#
# author: Dr. Lara S. Burchardt
# github: LSBurchardt//R_app_rhythm
# 
# Rhythm analysis is performed using ioi analysis and Fourier analysis
# See for example: Burchardt & Knörnschild, 2020, Comparison of methods for rhythm analysis of complex animals’ acoustic signals
# https://doi.org/10.1371/journal.pcbi.1007755
# 
# Furthermore different variability parameters and a universal goodness-of-fit value is calculated
# See: Burchardt, Briefer, Knörnschild, 2021, Novel ideas to further expand the applicability of rhythm analysis 
# https://doi.org/10.1002/ece3.8417
##########################################################################

#global.R

# 00: load packages -- ---------------------------------------------------------
if (!require(install.load)) {
  install.packages("install.load")
}

library(install.load)

install_load("shiny", "shinyFiles","shinybusy", "shinyjs", "shinyWidgets",
             "tidyverse","readxl","openxlsx","svDialogs",
             "pracma","SynchWave","vegan" ,"corrplot", "plotly","DT", "scales", "stringr")


#svDialogs: for dialouge boxes, not needed in app, but in raw code
#pracma: for fourier, findpeaks
#SynchWave: for fourier, fftshift
#vegan: for recurrence plots, to calculate euclidean distance
#corrplot: for recurrence plots, multiple plots
#plotly: interactive recurrence plots


# 01: load functions -- -------------------------------------------------------------

# currently not necessary

# 02:attributes  -----------------------------------------------------------

results_rhythm <<- data.frame()      # <<- global assignment operator, needs to be used when changing as well
ioi_all <<- list(NA)
results_rerun <<- data.frame()       # <<- global assignment operator, needs to be used when changing as well

# 03: define ui --------------------------------------------------------------

#defined in ui.R

# 04: Server function --------------------------------------------------------

#defined in server.R