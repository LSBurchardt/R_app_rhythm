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
      
      path <<- choose.dir()
      pattern <<- pattern()
      list_of_files <<- list.files(path = path, pattern = pattern)
      
      output$list_files <- renderTable({
        list.files(path = path, pattern = pattern)})
    }
    
  })
  
  
  #reactive expression df.sub: generates subset of all datapoints for chosen parameters in app
  results <- observe({
    
    if(input$goButton_2 > 0){
  
  
    if(input$all == TRUE){
      
      results_rhythm <<- data.frame()      # <<- global assignment operator, needs to be used when changing as well
      ioi_all <<- list(NA)
  
      
       for (a in 1:length(list_of_files)) {
         
        
         
         #be aware: if you have column names: set col_names = TRUE! if not: col_names = FALSE

        if (input$fileextension == 'csv'){
          data <- read_delim(paste(path, list_of_files[a], sep = "\\"), delim  = ",", col_names = TRUE)
        } #else if (input$fileextension == "xls"){
          #data <- read_xls(paste(path, list_of_files[a], sep = "\\"), sheet = 1, col_names = FALSE)
          #colnames(data) <- c("X1", "X2", "X3")
        #} else if (input$fileextension == "xlsx") {
        #  data <- read.xlsx(paste(path, list_of_files[a], sep = "\\"), sheet = 1, colNames = FALSE)
        #  colnames(data) <- c("X1", "X2", "X3")
        #} else {
        #  print("Please choose a different file format: either .csv or .xls")
        #  stop()
        #}

         output$loop_a <- renderText({
           paste("We are in loop", a)})
       
        
        #multiple problems with the xlsx files: for now, we do not allow xlsx
        
        #binary(data)
        #print(paste('Binary', a, 'of', length(list_of_files), 'done', sep =" "))
        
         output$table_input_data <- renderTable({
                data
         })
         ### ioi calc ----------
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
         ioi_all[[a]] <<- ioi
        
         #ioi_seq <<- ioi
         
         output$plot_ioi_beat <- renderPlotly({
           
           p <- ggplot(data = results_rhythm, aes(x= npvi, y = ioi_beat))+
             geom_point(na.rm = TRUE)+                         
             ylab("nPVI")+
             xlab("IOI Beat [Hz]")
           
           p <- ggplotly(p)
           
           p
           
         })
         
         output$plot_ioi_all <- renderPlotly({
           
           ioi_all <<- as.data.frame(unlist(ioi_all))
                                     
           p <- gather(ioi_all, cols, value) %>% 
             ggplot(aes(x= value))+
             geom_histogram(color = "white", fill = "darkblue", na.rm = TRUE)+               #change binwidth here
             aes(y=stat(count)/sum(stat(count))*100) +     # y is shown in percentages
             xlab("IOI [sec]")+                            
             ylab("Percentage [%]")+
             theme_minimal()
           ggplotly(p)
           
           p
         })
         
        #print(paste('IOI calc', a, 'of', length(list_of_files), 'done', sep =" "))
        
        #fourier_calc(binarydata)
        #print(paste('Fourier calc', a, 'of', length(list_of_files), 'done', sep =" "))
        
        #ugof(data = data, beat = results_rhythm[a,1], beat_2 = NA )# ,beat_2 = results_rhythm[a,3] ) # i being the looping variable through rows in the results_rhythm dataframe
        #beat_2 is an optional argument and can be set to the fft rhythm for example like this: [..]beat_2 = results_rhyth,[i,3]
        #print(paste('ugof', a, 'of', length(list_of_files), 'done', sep =" "))
        
        ### npvi calculations (ioi_seq) -----------
         
         ioi_seq <- drop_na(ioi)
         z <- c()
         b <- c()
         
         for (l in 1: nrow(ioi_seq)){
           
           z[l] <- (ioi_seq[l,1] - ioi_seq[l+1,1])
           b[l] <- (ioi_seq[l,1] + ioi_seq[l+1,1])/2
           
         } #end of for loop
         
         z <- na.omit(z)
         b <- na.omit(b)
         c <- sum(abs(z/b))
         
         npvi <- c*(100/(length(z)-1))
         
         results_rhythm[a,3] <<- npvi #change back to 9 when all is running
         
        #print(paste('npvi', a, 'of', length(list_of_files), 'done', sep =" "))
        
        #recurrence(data)
        #print(paste('Recurrence Plot', a, 'of', length(list_of_files), 'done', sep =" "))
     
        } #end for loop through list_of_files
      
     
      } #end of input$all == TRUE
      
     
      filenames <- as.data.frame(list_of_files)
      results_rhythm <<- cbind(results_rhythm, filenames)
      colnames(results_rhythm) <<- c("ioi_beat", "unbiased_cv",  "npvi", "filename")
      

      
  } # end of input goBUtton_2, rethink, does that really work in all cases? needs to be later possible when all other options start working
  
    
    
    })  #end of observer results  
  
  
  
  output$table_ioi <- renderTable({
    
    results_rhythm
    
  })
  
  output$calc <- renderText({
    paste("Once the results are done, they will appeare here. The results are:")})
          
# ################################
#     if (input$rec_plot == TRUE)
#       #recurrence(data) else {NULL} 
#     print("recurrence") else {NULL}
#       
#     if (input$hist_plot == TRUE)
#      # ioi_hist_plot() else {NULL} 
#     print("ioi") else {NULL}
   

  
} #end server function