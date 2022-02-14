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
  
  # Filextension
  pattern <- reactive({input$fileextension})
  
  output$files <- renderText({
    paste("You chose the following file extension pattern of the input data", pattern(), ".")
  })
  
  # pattern_out <- reactive({input$fileextension_output})
  # 
  # output$files_out <- renderText({
  #   paste("You chose the following file extension pattern for the output data", pattern_out(), ".")
  # })
  # Directory and Files
  observe({
    if(input$goButton_1 > 0){
      
      path <<- choose.dir()
      pattern <<- pattern()
      list_of_files <<- list.files(path = path, pattern = pattern)
      
      output$list_files <- renderTable({
        list.files(path = path, pattern = pattern)})
    }
    
  })
  

  
  # 04c: Calculations ---------------
  results <- observe({
    
    if(input$goButton_2 > 0){
  
      rm(results_rhythm, binarydata)
  
    if(input$all == TRUE){
      
      results_rhythm <<- data.frame()      # <<- global assignment operator, needs to be used when changing as well
      ioi_all <<- list(NA)
  
      
       for (a in 1:length(list_of_files)) {
         
        ## load data ----------
         
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
           paste("We finished loop", a , "of", length(list_of_files))})
       
        
        #multiple problems with the xlsx files: for now, we do not allow xlsx
        
        #binary(data)
        #print(paste('Binary', a, 'of', length(list_of_files), 'done', sep =" "))
        
         output$table_input_data <- renderTable({
                data
         })
         ## ioi calc & plot ----------
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
         
        
        
        
        ## npvi calculations (ioi_seq) -----------
         
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
         
        ## end npvi 
        
        ## fourier -----------
        
         ### binary -----------
         
         # transform time labels to binary sequence for Fourier analysis
         
         
         event_timepoint <- vector(length = nrow(data))
         event_timepoint <- data [,1]
         
         
         
         event_timepoint_fs <- round(event_timepoint*input$fs) # index of events with fs chosen in interface
         
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
         
         ### end binary
        
         ### fourier calc ----------
         
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
         
         results_rhythm[a,4] <<- peaks$freq[2] #change row index to soft coding once loops are in place [i,3]
         results_rhythm[a,5] <<- df            # frequency resolution
         results_rhythm[a,6] <<- kk            # number of elements 
         results_rhythm[a,7] <<- L             # length of signal
         
         ### end fourier calc
        ## end fourier
         
        ## ugof ------------- 
        #ugof(data = data, beat = results_rhythm[a,1], beat_2 = NA )# ,beat_2 = results_rhythm[a,3] ) # i being the looping variable through rows in the results_rhythm dataframe
        #beat_2 is an optional argument and can be set to the fft rhythm for example like this: [..]beat_2 = results_rhyth,[i,3]
         
         data_ugof <- pull(data[,1])
         beat_ioi <- results_rhythm[a,1]
         beat_fft <- results_rhythm[a,4]

         # calculate goodness-of-fit for IOI analysis and Fourier analysis
         
         maxoriginal <- max(data_ugof)
         timesteps <- 1000/round(beat_ioi, digits = 1)
         count <- 0
         theotime_value <- 0
         theotime_seq <- data.frame()
         
         while (theotime_value < maxoriginal){
           
           count <- count + 1;
           theotime_value <- count * timesteps /1000;
           theotime_seq[count,1] <- theotime_value;
           
         }
         
         # match original sequence to theoretical timeseries and calculate actual deviation
         x <- length(data_ugof)
         min_value <- c(1:x)
         ugof_value_beat <- c()
         
         
         for (n in 1:x){
           
           min_value[n] <- min(abs(data_ugof[n]- theotime_seq))
           
         }
         
         # calculate uGof
         
         maxdev <- timesteps/2/1000;
         
         ugof_value_beat <- min_value/maxdev;
         
         
         m_ugof_beat_1 <- median(ugof_value_beat[2:length(ugof_value_beat)])
         
         results_rhythm[a,8] <<- m_ugof_beat_1 ##change back to 7 when all is running
         
         ## ugof Fourier
         
         if(is.na(beat_fft) == FALSE){  
         
           timesteps_2 <- 1000/round(beat_fft, digits = 1)
           count_2 <- 0
           theotime_value_2 <- 0
           theotime_seq_2 <- data.frame()

            while (theotime_value_2 < maxoriginal){
           
              count_2 <- count_2 + 1;
              theotime_value_2 <- count_2 * timesteps_2 /1000;
              theotime_seq_2[count_2,1] <- theotime_value_2;
            }

            # match original sequence to theoretical timeseries and calculate actual deviation
           
            x_2 <- length(data_ugof)
            min_value_2 <- c(1:x)
            ugof_value_beat_2 <- c()
           
           
            for (n in 1:x){
              min_value_2[n] <- min(abs(data_ugof[n]- theotime_seq_2))
            }
           
            # calculate uGof
           
            maxdev_2 <- timesteps_2/2/1000;
           
            ugof_value_beat_2 <- min_value_2/maxdev_2;
           
            m_ugof_beat_2 <- median(ugof_value_beat_2[2:length(ugof_value_beat_2)])
           
            results_rhythm[a,9] <<- m_ugof_beat_2 } else {
              
              results_rhythm[a,9] <<- NA
            }
         
        ## end ugof
        
        ## recurrence plot -------
        
         if (input$rec_plot == TRUE){
           
           
           ioi_seq <- data.frame()                 # set up empty dataframe to store ioi values in
           
           #for (a in filenumber){             #start of loop for number of files, needs to be added, maybe better add in main!
           
           for (x in  1:nrow(data)) {          # start of loop through rows of data to calculate iois
             
             z = x+1
             ioi_seq[x,1] <- data[z,1]-data[x,1]
             
           }
           
           ##  recurrence matrix
           
           # euclidian distance matrix 
           
           eucl_dist <- (as.matrix(vegdist(ioi_seq, "euclidean", na.rm = TRUE)))
           eucl_dist <- eucl_dist[1:(nrow(eucl_dist)-1),1:(nrow(eucl_dist)-1) ]

           threshold <- mean(ioi_seq$X1, na.rm = TRUE)*0.1 # as input?

           eucl_dist[eucl_dist < threshold] <- 0
           
           # transform matrix as to be able to plot it with ggplot as tile plot
           #https://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2
           
           levels <- 1:(nrow(eucl_dist))

           eucl_dist_2 <- eucl_dist %>%
             tibble::as_tibble() %>%
             rownames_to_column('Var1') %>%
             gather(Var2, value, -Var1) %>%
             mutate(
               Var1 = factor(Var1, levels = levels),
               Var2 = factor(gsub("V", "", Var2), levels = levels)
             )
           ## recurrence plot 
           output$rec_plot <- renderPlot({
             
           rec_plot <- ggplot(eucl_dist_2, aes(Var1, Var2)) +
             geom_tile(aes(fill = value)) +
             #geom_text(aes(label = round(value, 1))) +
             scale_fill_gradient(low = "white", high = "black")


           #ggplotly(rec_plot)
           
           rec_plot
           })
           
           
           ## loop version
           #https://stackoverflow.com/questions/22840892/r-shiny-loop-to-display-multiple-plots
           #part 1: define dynamic output
           output$plots <- renderUI({
             plot_output_list <- lapply(1:length(list_of_files), function(a) {
               plotname <- paste("plot", a, sep="")
               plotOutput(plotname)
             }) # end lapply
             
             do.call(tagList, plot_output_list)
           }) ##End outputplots
        
           # part 2: print plots
           PrintPlots <- observe({
             # Call renderPlot for each one. Plots are only actually generated when they
             # are visible on the web page.
             
             for (a in 1:length(list_of_files)) {
               # Need local so that each item gets its own number. Without it, the value
               # of i in the renderPlot() will be the same across all instances, because
               # of when the expression is evaluated.
               local({
                 my_a <- a
                 plotname <- paste("plot", my_a, sep="")
                 
                 # calculations for plot
                 for (x in  1:nrow(data)) {          # start of loop through rows of data to calculate iois
                   
                   z = x+1
                   ioi_seq[x,1] <- data[z,1]-data[x,1]
                   
                 }
                 
                 ##  recurrence matrix
                 
                 # euclidian distance matrix 
                 
                 eucl_dist <- (as.matrix(vegdist(ioi_seq, "euclidean", na.rm = TRUE)))
                 eucl_dist <- eucl_dist[1:(nrow(eucl_dist)-1),1:(nrow(eucl_dist)-1) ]
                 
                 threshold <- mean(ioi_seq$X1, na.rm = TRUE)*0.1 # as input?
                 
                 eucl_dist[eucl_dist < threshold] <- 0
                 
                 # transform matrix as to be able to plot it with ggplot as tile plot
                 #https://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2
                 
                 levels <- 1:(nrow(eucl_dist))
                 
                 eucl_dist_2 <- eucl_dist %>%
                   tibble::as_tibble() %>%
                   rownames_to_column('Var1') %>%
                   gather(Var2, value, -Var1) %>%
                   mutate(
                     Var1 = factor(Var1, levels = levels),
                     Var2 = factor(gsub("V", "", Var2), levels = levels)
                   )
                 # end: calculations for plot
                 
                 output[[plotname]] <- renderPlot({
                   
                   #works with another plot, such as the example below...
                   #ggplot(mtcars, aes(x = mpg, y = cyl))+
                    #  geom_point()
                   
                   ggplot(eucl_dist_2, aes(Var1, Var2)) +
                   geom_tile(aes(fill = value)) +
                   # geom_text(aes(label = round(value, 1))) +
                   scale_fill_gradient(low = "white", high = "black")
                   
                   
                 }) # end renderPlot
               }) # end local
             }##### End for Loop
           })# END OF PRINT PLOTS
           
           
         } else {NULL}       ## end recurrence plot 
         
        } #end for loop through list_of_files
      
     
      } #end of input$all == TRUE
      
        ## results details ------
      
      filenames <- as.data.frame(list_of_files)
      fs <- input$fs
      results_rhythm <<- cbind(results_rhythm, fs, filenames)
      colnames(results_rhythm) <<- c("ioi_beat", "unbiased_cv",  "npvi", "fourier_beat", "freq_reso", "n_elements","signal_length","ugof_ioi","ugof_fft", "fs", "filename")
      

      
  } # end of input goBUtton_2, rethink, does that really work in all cases? needs to be later possible when all other options start working
  
    
    
})  #end of observer results  
  
  
  

  output$table_ioi <- renderTable({
    
    results_rhythm
    
  })
  
  output$calc <- renderText({
    paste("Once the results are done, they will appeare here. The results are:")})
  
## 05: Reset Button
  
  observeEvent(input$resetAll, {
    reset("fs")
    reset( "savename")
    reset("goButton_1")
    reset("table_ioi")
    
    rm(results_rhythm)
    
    output$table_ioi <- renderTable({
      
      results_rhythm
      
    })
    
  })
    
## 06: Download Results --------------
  
  datasetInput <- reactive({
    
    results_rhythm
    
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("rhythm_analysis_", input$savename,"_fs_",input$fs,".csv", sep = "")
    },
    content = function(file){
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
          
## 07: Tab Recurrence Plots -------------
  
  #   observe({
  #     
  # if (input$rec_plot == TRUE){
  # 
  # 
  # ioi_seq <- data.frame()                 # set up empty dataframe to store ioi values in
  # 
  # #for (a in filenumber){             #start of loop for number of files, needs to be added, maybe better add in main!
  # 
  # for (x in  1:nrow(data)) {          # start of loop through rows of data to calculate iois
  # 
  #   z = x+1
  #   ioi_seq[x,1] <- data[z,1]-data[x,1]
  # 
  # }

  ## 02: recurrence matrix ---------------------

  #euclidian distance matrix (when using phillips way)

  # eucl_dist <- (as.matrix(vegdist(ioi_seq, "euclidean", na.rm = TRUE)))
  # eucl_dist <- eucl_dist[1:(nrow(eucl_dist)-1),1:(nrow(eucl_dist)-1) ]
  # 
  # threshold <- mean(ioi_seq$X1, na.rm = TRUE)*0.1 # as input?
  # 
  # eucl_dist[eucl_dist < threshold] <- 0

  # transform matrix as to be able to plot it with ggplot as tile plot
  #https://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2

  # levels <- 1:(nrow(eucl_dist))
  # 
  # eucl_dist_2 <- eucl_dist %>%
  #   tibble::as_tibble() %>%
  #   rownames_to_column('Var1') %>%
  #   gather(Var2, value, -Var1) %>%
  #   mutate(
  #     Var1 = factor(Var1, levels = levels),
  #     Var2 = factor(gsub("V", "", Var2), levels = levels)
  #   )
  ## 03: recurrence plot -----------------

  # p <- ggplot(eucl_dist_2, aes(Var1, Var2)) +
  #   geom_tile(aes(fill = value)) +
  #   #geom_text(aes(label = round(value, 1))) +
  #   scale_fill_gradient(low = "white", high = "black")
  # 
  # 
  # ggplotly(p)
  #} else {NULL}

  #})

  
#     if (input$hist_plot == TRUE)
#      # ioi_hist_plot() else {NULL} 
#     print("ioi") else {NULL}
   

  
} #end server function