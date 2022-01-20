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

#server.R

# 00: load packages -- ---------------------------------------------------------

#defined in global.R

# 01: load data -- -------------------------------------------------------------

#defined in global.R

# 02: map attributes and other values

#defined in "global.R"

# 03: define UI --------------------------------------------------------------

#defined in ui.R

# 04: Server function ---------------------------------------------------------
# Define server logic required to draw mapPlot and other plots for mainpanel
server <- function(input, output) {
  
  
   # 04a: author  ------------------------------------------    
  output$authors <- renderText({
    paste("written by Dr. Lara S. Burchardt")
  })
  
  # 04b: choosing data to analyse  ------------------------------------------
  
  # dir
  pattern <- reactive({input$fileextension})
  
  output$files <- renderText({
    paste("You chose the following file extension pattern", pattern(), ".")
  })
  
  
  observe({
    if(input$goButton_1 > 0){
      
      path <- choose.dir()
      pattern <- pattern()
      list_of_files <- list.files(path = path, pattern = pattern)
      
      output$session <- renderText({
        list.files(path = path, pattern = pattern)})
    }
    
  })
  
  
  
  
  
  #reactive expression df.sub: generates subset of all datapoints for chosen parameters in app
  results <- observe({
    if(input$goButton_2 > 0){
  
  
    if(input$all == TRUE){
      
      for (a in 1:length(list_of_files)) {
        
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
        
        
        output$loop_a <- renderText({
          paste("We are in loop", a)})
        #multiple problems with the xlsx files: for now, we do not allow xlsx
        
        #binary(data)
        #print(paste('Binary', a, 'of', length(list_of_files), 'done', sep =" "))
        
        ioi_calc(data)
        #ioi_all[[a]] <- ioi
        #print(paste('IOI calc', a, 'of', length(list_of_files), 'done', sep =" "))
        
        #fourier_calc(binarydata)
        #print(paste('Fourier calc', a, 'of', length(list_of_files), 'done', sep =" "))
        
        #ugof(data = data, beat = results_rhythm[a,1], beat_2 = NA )# ,beat_2 = results_rhythm[a,3] ) # i being the looping variable through rows in the results_rhythm dataframe
        #beat_2 is an optional argument and can be set to the fft rhythm for example like this: [..]beat_2 = results_rhyth,[i,3]
        #print(paste('ugof', a, 'of', length(list_of_files), 'done', sep =" "))
        
        #npvi(ioi_seq)
        #print(paste('npvi', a, 'of', length(list_of_files), 'done', sep =" "))
        
        #recurrence(data)
        #print(paste('Recurrence Plot', a, 'of', length(list_of_files), 'done', sep =" "))
      }
      } 
    
     
  
  
  }
})    
  
  output$calc <- renderText({
    paste("The calculations are done. The results are:", results())})
          
# ################################
#     if (input$rec_plot == TRUE)
#       #recurrence(data) else {NULL} 
#     print("recurrence") else {NULL}
#       
#     if (input$hist_plot == TRUE)
#      # ioi_hist_plot() else {NULL} 
#     print("ioi") else {NULL}
   

  
} #end server function