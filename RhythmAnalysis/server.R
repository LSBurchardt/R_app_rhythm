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
# Define server logic required to run calculations, draw plots etc.
server <- function(input, output) {
  
  
# author  ------------------------------------------ 
  
  output$authors <- renderText({
    paste("written by Dr. Lara S. Burchardt")
  })
  
# 04a: choosing data to analyse  -------------------
  
# File extension, Textoutput which file extension was chosen
  pattern <- reactive({input$fileextension})
  
  output$files <- renderText({
    paste("You chose the following file extension pattern of the input data", pattern(), ".")
  })
  
# Directory and Files, 
  observe({
    if(input$goButton_1 > 0){
      
      path <<- choose.dir()
      pattern <<- pattern()
      list_of_files <<- list.files(path = path, pattern = pattern)
      
      output$list_files <- renderTable({
        #list.files(path = path, pattern = pattern)
        
        list <-list.files(path = path, pattern = pattern)
        list <- as.data.frame(list)
        
        list$index <- row.names(list)
        colnames(list) <- c("file", "index")
        list
        }) # end renderTable
      
      } # end if input$go Button
  })# end observe go button
  
# 04b: Standard Calculations ---------------
results <- observe({
    
    if(input$goButton_2 > 0){
  
      rm(results_rhythm, binarydata)
  
    if(input$all == TRUE){
      
      results_rhythm <<- data.frame()      # <<- global assignment operator, needs to be used when changing as well
      ioi_all <<- list(NA)
      plot_list <<- list(NA)
    
for (a in 1:length(list_of_files)) {
         
  ## load data ----------
         
  #be aware: independent of your column names, they are overwritten to be X1, X2, and X3

        if (input$fileextension == 'csv'){
          data <- read_delim(paste(path, list_of_files[a], sep = "\\"), delim  = ",", col_names = FALSE)
          colnames(data) <- c("X1", "X2", "X3")
        } else if (input$fileextension == "xls"){
          data <- read_xls(paste(path, list_of_files[a], sep = "\\"), sheet = 1, col_names = FALSE)
          colnames(data) <- c("X1", "X2", "X3")
        } else if (input$fileextension == "xlsx") {
          data <- read.xlsx(paste(path, list_of_files[a], sep = "\\"), sheet = 1, colNames = FALSE)
          colnames(data) <- c("X1", "X2", "X3")
        } else {NULL}

        

    output$loop_a <- renderText({
         paste("We finished loop", a , "of", length(list_of_files))})
        
         output$table_input_data <- renderTable({
                data
         }) #end renderText

### subset for element types -----------         
# did you load data with element types in column X3? if-clause to check for third column
# if not, message is shown that all elements are used, because not subsetable for elementtype        
      
          if("X3" %in% colnames(data) == TRUE) {

           elementlist<- as.vector(NULL)

           if (input$element_a == TRUE)
             elementlist <- c(elementlist, "a") else {NULL}

           if (input$element_b == TRUE)
             elementlist <- c(elementlist, "b") else {NULL}

           if(input$element_c == TRUE)
             elementlist <- c(elementlist, "c") else {NULL}

           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "d") else {NULL}

           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "e") else {NULL}

           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "f") else {NULL}
           
# filter data for the chosen element types
           
           elements_seq[a] <<- str_c(data$X3, collapse = "") #needs to be run before(!) filtering the data
           #otherwise the saved sequence will be the sequence that was actually analysed, would we want that?
           # maybe include both?
           
           data <- data %>%
             filter(X3 %in% elementlist)
           
           
# output elementlist to see, which elements where chosen, just a back up to see, if elementlist
# works correctly
           
           output$elementlist <- renderTable({
             
            elementlist
             
           })
# else to "X3" %in% colnames(data) == TRUE)
           } else {

             data <- data

          output$element <- renderText({

               print("You did not assign different element types. All elements will be used.")

             })

           } #end of else

         
## ioi calc & plot ----------
         ioi <- data.frame()                 # set up empty dataframe to store ioi values in
         
         #for (a in filenumber){             #start of loop for number of files, needs to be added, maybe better add in main! 
         
         for (x in  1:nrow(data)) {          # start of loop through rows of data to calculate iois
           
           z = x+1 
           ioi[x,1] <- data[z,1]-data[x,1]
           
         }                                   #end of loop through rows of data to calculate iois
         
         colnames(ioi) <- c("X1")
         
         #ioi_beat <- 1/mean(ioi$X1, na.rm = TRUE) # calculate mean of iois
         #ioi_beat <- 1/median(ioi$X1, na.rm = TRUE) #
         ioi_beat <- 1/get(input$method)(ioi$X1, na.rm = TRUE) #the user chooses whether to calculate ioi beat based on median or mean
         ioi_cv <- sd(ioi$X1, na.rm = TRUE)/mean(ioi$X1, na.rm = TRUE)
         ioi_cv_unbiased <-  (1+1/(4*(nrow(ioi)-1)))*ioi_cv
         
         
         #add parameters to results
         results_rhythm[a,1] <<- a
         results_rhythm[a,2] <<- ioi_beat 
         results_rhythm[a,3] <<- ioi_cv_unbiased
         ioi_all[[a]] <<- ioi
        
         #ioi_seq <<- ioi
         
### plot Beats ------------
         output$plot_beat <- renderPlotly({
           
           p <- ggplot(data = results_rhythm, )+
             geom_jitter(aes(x = "IOI", y = ioi_beat, color = filename),
                         width = 0.1, alpha = 0.8)+
             geom_jitter(aes(x = "Fourier", y = fourier_beat, color = filename),
                         width = 0.1, )+
             ylab("Beat [Hz]")+
             xlab("Beat Parameter")+
             theme_minimal()+
             ggtitle("Beat Results in Hertz")+
             ylim(0,input$fs/2)
           
           p <- ggplotly(p)

           p
           
         })
         
         
### plot ugof ----------------
         output$plot_ugof <- renderPlotly({
           
           p <- ggplot(data = results_rhythm)+
             geom_jitter(aes(x = "IOI ugof", y = ugof_ioi, color = filename),
                         width = 0.1,
                         alpha = 0.8)+
             geom_jitter(aes(x = "Fourier ugof", y = ugof_fft, color = filename),
                         width = 0.1,
                         alpha = 0.8)+
             geom_jitter(aes(x = "Coefficient of Variation", y = unbiased_cv, color = filename),
                         width = 0.1,
                         alpha = 0.8)+
             ylab("Value")+
             xlab("Parameter")+
             theme_minimal()+
             ggtitle("Goodness of Fit an Coefficient of Variation")+
             ylim(0,1)
           
           p <- ggplotly(p)
           
           p
           
         })
### plot Variability Parameter ----------
         output$plot_var <- renderPlotly({
           
           p <- ggplot(data = results_rhythm)+
             geom_jitter(aes(x = "npvi", y = npvi, color = filename),
                         width = 0.1,
                         alpha = 0.8)+
             ylab("Value")+
             xlab("")+
             theme_minimal()+
             ggtitle("normalized Pairwise Variability Index")+
             ylim(0,max(results_rhythm$npvi))
           
           p <- ggplotly(p)
           
           p
           
         })
         
### ioi hist --------------------
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
         
         results_rhythm[a,4] <<- npvi #change back to 9 when all is running
         
        ## end npvi 
        
## fourier calculations -----------
        
### binary -----------
         
#transform time labels to binary sequence for Fourier analysis
         
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
         
         fs <- input$fs # should be soft coded to match sampling rate calculated with in "binary"
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
         # we now need to couple index with corresponding frequency from f
         
         # f at indexes in peaks$index
         peak_freq <- f[peaks$index]
         # add frequency to the amplitude dataframe
         peaks[,"freq"] <- peak_freq
         
         #Account for shift in zero-bin component
         
         if (peaks[1,3] != 0){
           
           peaks$freq <- peaks$freq - peaks$freq[1]
           
         }
         
         #save data
         
         results_rhythm[a,5] <<- peaks$freq[2] # fourier beat
         results_rhythm[a,6] <<- df            # frequency resolution
         results_rhythm[a,7] <<- kk            # number of elements 
         results_rhythm[a,8] <<- L             # length of signal
         
         ### end fourier calc
        ## end fourier
         
## ugof ------------- 

### ugof ioi ---------
         
         rm(maxoriginal, timesteps, theotime_value, theotime_seq)
         
         data_ugof <- data$X1
         #data_ugof <- pull(data[,1]) #pull doesn't work with all input data, i.e. when loaded from xlsx
         beat_ioi <- results_rhythm[a,2]
         beat_fft <- results_rhythm[a,5]

         # calculate goodness-of-fit for IOI analysis and Fourier analysis
         
         maxoriginal <- max(data_ugof)
         timesteps <- 1000/round(beat_ioi, digits = 1)
         #timesteps <- 1000/30
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
         
###recurrence ugof ioi -------------
         
         output$rec_ugof_plots <- renderUI({
           plot_output_list <- lapply(1:length(list_of_files), function(i) {
             plotname <- paste("ugof_plot", i, sep="")
             plotlyOutput(plotname, height = 300, width = 300)
           }) # end lapply
           
           do.call(tagList, plot_output_list)
         }) 
         
         local({
         
         eucl_dist_ugof <- (as.matrix(vegdist(ugof_value_beat, "euclidean", na.rm = TRUE)))
         eucl_dist_ugof <- eucl_dist_ugof[1:(nrow(eucl_dist_ugof)-1),1:(nrow(eucl_dist_ugof)-1) ]
         
         threshold <- mean(ugof_value_beat, na.rm = TRUE)*0.1 # as input?
         
         eucl_dist_ugof[eucl_dist_ugof < threshold] <- 0
         
# transform matrix as to be able to plot it with ggplot as tile plot
 #https://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2
         
         levels <- 1:(nrow(eucl_dist_ugof))
         
         eucl_dist_ugof_2 <- eucl_dist_ugof %>%
           tibble::as_tibble() %>%
           rownames_to_column('Var1') %>%
           gather(Var2, value, -Var1) %>%
           mutate(
             Var1 = factor(Var1, levels = levels),
             Var2 = factor(gsub("V", "", Var2), levels = levels)
           )
         
         my_i <- a
         plotname <- paste("ugof_plot", my_i, sep="")
         
         output[[plotname]] <- renderPlotly({
          
        rec_plot_ugof <- ggplot(eucl_dist_ugof_2, aes(Var1, Var2)) +
           geom_tile(aes(fill = value)) +
           # geom_text(aes(label = round(value, 1))) +
           scale_fill_gradient(low = "white", high = "black")+
           xlab("#ugof")+
           ylab("#ugof")+
           coord_fixed(ratio=1)+
           ggtitle(paste("ugof ioi,File:", results_rhythm$filename[my_i]))+
           theme_minimal()+
           theme(
             plot.background = element_rect(fill = "white"),
             panel.grid = element_blank(),
             title = element_text(size = 6))
         
         rec_plot_ugof<- ggplotly(rec_plot_ugof)
         
         rec_plot_ugof
         
         }) #end renderPlotly
      }) #end local
         
  ### end recurrence ugof
         results_rhythm[a,9] <<- m_ugof_beat_1 
         #problem with [a,10] is not showing what I would expect it to show
         silent_beat_ioi <- nrow(theotime_seq)-kk
         results_rhythm[a,10] <<- silent_beat_ioi
         
  ### ugof Fourier --------------
         
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
           
# calculate ugof for Fourier beat
           
            maxdev_2 <- timesteps_2/2/1000;
           
            ugof_value_beat_2 <- min_value_2/maxdev_2;
           
            m_ugof_beat_2 <- median(ugof_value_beat_2[2:length(ugof_value_beat_2)])
           
### recurrence ugof fft -------------
            
            output$rec_ugof_fft_plots <- renderUI({
              plot_output_list <- lapply(1:length(list_of_files), function(i) {
                plotname <- paste("ugof_fft_plot", i, sep="")
                plotlyOutput(plotname, height = 300, width = 300)
              }) # end lapply
              
              do.call(tagList, plot_output_list)
            }) 
            
            local({
              
              eucl_dist_ugof <- (as.matrix(vegdist(ugof_value_beat_2, "euclidean", na.rm = TRUE)))
              eucl_dist_ugof <- eucl_dist_ugof[1:(nrow(eucl_dist_ugof)-1),1:(nrow(eucl_dist_ugof)-1) ]
              
              threshold <- mean(ugof_value_beat, na.rm = TRUE)*0.1 # as input?
              
              eucl_dist_ugof[eucl_dist_ugof < threshold] <- 0
              
              # transform matrix as to be able to plot it with ggplot as tile plot
              #https://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2
              
              levels <- 1:(nrow(eucl_dist_ugof))
              
              eucl_dist_ugof_2 <- eucl_dist_ugof %>%
                tibble::as_tibble() %>%
                rownames_to_column('Var1') %>%
                gather(Var2, value, -Var1) %>%
                mutate(
                  Var1 = factor(Var1, levels = levels),
                  Var2 = factor(gsub("V", "", Var2), levels = levels)
                )
              
              my_i <- a
              plotname <- paste("ugof_fft_plot", my_i, sep="")
              
              output[[plotname]] <- renderPlotly({
                
                rec_plot_ugof <- ggplot(eucl_dist_ugof_2, aes(Var1, Var2)) +
                  geom_tile(aes(fill = value)) +
                  # geom_text(aes(label = round(value, 1))) +
                  scale_fill_gradient(low = "white", high = "black")+
                  xlab("#ugof")+
                  ylab("#ugof")+
                  coord_fixed(ratio=1)+
                  ggtitle(paste("ugof fft,File:", results_rhythm$filename[my_i]))+
                  theme_minimal()+
                  theme(
                    plot.background = element_rect(fill = "white"),
                    panel.grid = element_blank(),
                    title = element_text(size = 6))
                
                rec_plot_ugof<- ggplotly(rec_plot_ugof)
                
                rec_plot_ugof
                
              }) # end renderPlotly
            }) # end local 
          ### end recurrence plot fft ugof
            
            
            results_rhythm[a,11] <<- m_ugof_beat_2
            
            silent_beat_fft <- nrow(theotime_seq_2)-kk
           
            results_rhythm[a,12] <<- silent_beat_fft} else {
              
              results_rhythm[a,11] <<- NA
              results_rhythm[a,12] <<- NA
              
            }
         
        ## end ugof
        
## recurrence plot -------
        
         if (input$rec_plot == TRUE){
           
## loop version for recurrence plots
#https://stackoverflow.com/questions/22840892/r-shiny-loop-to-display-multiple-plots
           output$plots <- renderUI({
             plot_output_list <- lapply(1:length(list_of_files), function(i) {
               plotname <- paste("plot", i, sep="")
               plotlyOutput(plotname, height = 300, width = 300)
             }) # end lapply
             
             do.call(tagList, plot_output_list)
           }) 
           
           local({
           
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
           
                 my_i <- a
                 plotname <- paste("plot", my_i, sep="")

                 output[[plotname]] <- renderPlotly({
                   
                  rec_plot <- ggplot(eucl_dist_2, aes(Var1, Var2)) +
                   geom_tile(aes(fill = value)) +
                   # geom_text(aes(label = round(value, 1))) +
                   scale_fill_gradient(low = "white", high = "black")+
                   xlab("#IOI")+
                   ylab("#IOI")+
                  coord_fixed(ratio=1)+
                  ggtitle(paste("IOI,File:", results_rhythm$filename[my_i]))+
                  theme_minimal()+
                  theme(
                    plot.background = element_rect(fill = "white"),
                    panel.grid = element_blank(),
                    title = element_text(size = 6))

                   rec_plot <- ggplotly(rec_plot)
                   
                   rec_plot
                   
                 }) # end renderPlotly
               }) # end local
           
         } else {NULL}       ## end if recurrence plot 
         
        } #end for loop through list_of_files
      
     
      } #end of input$all == TRUE
      
# 04c: Standard results details ----------
      
      filenames <- as.data.frame(list_of_files)
      
      if("X3" %in% colnames(data) == FALSE) {
        
      elements <- "all"
      elements_raw <- NA
        
      } else if ("X3" %in% colnames(data) == TRUE & length(elementlist) == 6){
        
        elements <- "all"
        elements_raw <- elements_seq} else {
      
      elements <- str_c(elementlist, collapse = " ")
      elements_raw <- elements_seq
      
      }
      
      fs <- input$fs
      savename <- input$savename
      results_rhythm <<- cbind(results_rhythm, fs,elements,elements_raw, filenames, savename)
      colnames(results_rhythm) <<- c("index", "ioi_beat", "unbiased_cv",  "npvi", "fourier_beat", "freq_reso", "n_elements","signal_length","ugof_ioi","silent_beats_ioi","ugof_fft","silent_beats_fft", "fs","elements","raw_element_seq", "filename", "savename")
      

      
   # end of input goBUtton_2, rethink, does that really work in all cases? needs to be later possible when all other options start working
  
  } 
    
})  #end of observer results  
  

  output$table_ioi <- renderTable({
    
    results_rhythm
    
  })
  
  output$calc <- renderText({
    paste("Once the results are done, they will appeare here. The results are:")})
  
#04d: Reset Button -------------
  
  observeEvent(input$resetAll, {
    reset("fs")
    reset( "savename")
    reset("goButton_1")
    reset("table_ioi")
    
    rm(results_rhythm)
    
    output$table_ioi <- renderTable({
      
      results_rhythm
      
    }) #end renderTable
    
  }) #end observeEvent reset

#04e: Rerun analysis on subsection chosen from the Recurrence plots------------
# The rerun is only calculating ioi beat and ugof, should it calculate the others, too?
# tags:  file_nr --> File Nr. to rerun analysis
#         start --> start IOI, for analysis
#         end   --> end IOI, for analysis
#         rerun_ioi --> name of action button

# observeEvent could be used for the goButtons, too?
  observeEvent(input$rerun_ioi, {  
    
    if (input$fileextension == 'csv'){
      data_rerun <- read_delim(paste(path, list_of_files[input$file_nr], sep = "\\"), delim  = ",", col_names = FALSE)
      colnames(data_rerun) <- c("X1", "X2", "X3")
    } else if (input$fileextension == "xls"){
      data_rerun <- read_xls(paste(path, list_of_files[input$file_nr], sep = "\\"), sheet = 1, col_names = FALSE)
      colnames(data_rerun) <- c("X1", "X2", "X3")
    } else if (input$fileextension == "xlsx") {
      data_rerun <- read.xlsx(paste(path, list_of_files[input$file_nr], sep = "\\"), sheet = 1, colNames = FALSE)
      colnames(data_rerun) <- c("X1", "X2", "X3")
    } else {NULL}
   
   data_rerun_section <- data_rerun[input$start:input$end,1]

   ## rerun ioi beat ----------
   
   ioi_rerun <- data.frame()                 # set up empty dataframe to store ioi values in
   
   for (x in  1:nrow(data_rerun_section)) {          # start of loop through rows of data to calculate iois
     
     z = x+1 
     ioi_rerun[x,1] <- data_rerun_section[z,1]-data_rerun_section[x,1]
     
   }                                   #end of loop through rows of data to calculate iois
   
   colnames(ioi_rerun) <- c("X1")
   
   ioi_beat_rerun <- 1/mean(ioi_rerun$X1, na.rm = TRUE) # calculate mean of iois
    
      results_rerun[1,1] <<- ioi_beat_rerun
      
##rerun ugof ioi beat -------------
      data_rerun_ugof <- data_rerun_section$X1

      beat_rerun_ioi <- results_rerun[1,1]
      
      # calculate goodness-of-fit for IOI analysis 
      
      maxoriginal <- max(data_rerun_ugof)
      timesteps <- 1000/round(beat_rerun_ioi, digits = 1)
      count <- 0
      theotime_value <- 0
      theotime_seq <- data.frame()
      
      while (theotime_value < maxoriginal){
        
        count <- count + 1;
        theotime_value <- count * timesteps /1000;
        theotime_seq[count,1] <- theotime_value;
        
      }
      
      # match original sequence to theoretical timeseries and calculate actual deviation
      x <- length(data_rerun_ugof)
      min_value <- c(1:x)
      ugof_value_beat_rerun <- c()
      
      for (n in 1:x){
        
        min_value[n] <- min(abs(data_rerun_ugof[n]- theotime_seq))
        
      }
      
      # calculate uGof ioi beat rerun
      
      maxdev <- timesteps/2/1000;
      
      ugof_value_beat_rerun <- min_value/maxdev;
      
      m_ugof_beat_1_rerun <- median(ugof_value_beat_rerun[2:length(ugof_value_beat_rerun)])
      
      results_rerun[1,2] <<- m_ugof_beat_1_rerun
      
      results_rerun[1,3] <<- list_of_files[input$file_nr]
      
      results_rerun[1,4] <<- input$start
      
      results_rerun[1,5] <<- input$end
      
      results_rerun[1,6] <<- input$end - input$start + 1
      
      colnames(results_rerun) <<- c("rerun_ioi_beat", "rerun_ugof", "file_nr", "rerun_start_ioi","rerun_end_ioi", "number of elements" )
      
      output$table_rerun <- renderTable({
        
        results_rerun
        
      })
      
  }) #end observerEvent rerun IOI
  
# 04f: beat precision details -------------
## theoretical maximum deviations ---------
  
  output$warning_ugof_detail <- renderText({
    
    "Running this analysis is quite time intensive, as all ugofs are calculated between
    0.1 and 100 Hz for all input files you chose. Are you sure you want to run this analysis?"
    
  })
  
  observeEvent(input$ugof_detail, {  
  
  maxdev <- vector()
  count <- 0
    
for (x in seq(from = 0.1, to= 100, by = 0.1)){
    
      count <- count + 1
    
      timestep <- 1000/x
    
      maxdev[count] <- timestep/2
    }

  maxdev <- as.data.frame(maxdev)
  
  maxdev <- maxdev %>% 
    mutate(beat = seq(0.1,100,0.1),
           maxdev = maxdev/1000) #transform max dev from ms to seconds
  
  output$maxdev100 <- renderPlotly({
    
    p <- ggplot(data = maxdev, )+
      geom_jitter(aes(x = beat, y = maxdev),
                  width = 0.2, alpha = 0.5, shape = 1, size = 0.5)+
      ylab("max deviation in sec")+
      xlab("Beat [Hz]")+
      theme_minimal()+
      ggtitle("Maximum possible deviation from 1 to 100 Hz")+
      coord_cartesian(ylim = c(0.001,10))+
      scale_y_log10(labels = scales::label_comma(accuracy = 0.01))

    p <- ggplotly(p)
    
    p
    
  })

## modelling ugofs ----------------
# for various rhythms as calculated with 

# there is an error thrown for certain rhythms that are too slow for the max orginal
# needs to check first if theotime_value is bigger than maxoriginal
# why wasn't that a problem in Matlab?
  
  for (k in 1:length(list_of_files)){
    
    if (input$fileextension == 'csv'){
      data_ugof <- read_delim(paste(path, list_of_files[k], sep = "\\"), delim  = ",", col_names = FALSE)
      colnames(data_ugof) <- c("X1", "X2", "X3")
    } else if (input$fileextension == "xls"){
      data_ugof <- read_xls(paste(path, list_of_files[k], sep = "\\"), sheet = 1, col_names = FALSE)
      colnames(data_ugof) <- c("X1", "X2", "X3")
    } else if (input$fileextension == "xlsx") {
      data_ugof <- read.xlsx(paste(path, list_of_files[k], sep = "\\"), sheet = 1, colNames = FALSE)
      colnames(data_ugof) <- c("X1", "X2", "X3")
    } else {NULL} 
    
    data_ugof <- data_ugof[,1]
    
    a <- 0
    
    for (rhythm in seq(from = 0.1, to = 100, by = 0.01)){
      #rm(timesteps, count, theotime_value, theotime_seq, x, minValue, maxdev, ugof_value)
      a <- a +1
      maxoriginal <- max(data_ugof)
      timesteps <- 1000/rhythm
      count <- 0
      theotime_value <- 0
      theotime_seq <- data.frame()
      
      while (theotime_value < maxoriginal){
        
        count <- count + 1
        theotime_value <- count * timesteps/1000
        theotime_seq[count,1] <- theotime_value  # here is the problem, theotime_seq is 
        # only of length 1 for certain rhythms and certain max values of data
        # in min_value[n] <- min(abs(data_ugof[n]- theotime_seq)) this leads to a 
        # problem, because data_ugof[n] is not the same size as theotime_seq
        # probably easiest to implement the if condition in the for loop below
        # check what happened here in Matlab and why it did work there 
        #
        # is there an implementation issue to begin with? what was calculated here? what was subtracted
        # from what here? go back to Matlab and check there
        ##### HIER WEITER---------
      }
        # x <- length(data_ugof)
        # min_value <- c(1:x)
        # ugof_value_beat <- c()
        # 
        # 
        # for (n in 1:x){
        #   
        #   min_value[n] <- min(abs(data_ugof[n]- theotime_seq))
        #   
        # }
        # 
        # # calculate uGof
        # 
        # maxdev <- timesteps/2/1000
        # 
        # ugof_value_beat <- min_value/maxdev
        # 
        # 
        # m_ugof[a,k] <- median(ugof_value_beat[1:length(ugof_value_beat)])  
      }
      }
    
  # output$ugof_hist <- renderPlotly({
  #   
  #   p <- gather(m_ugof, cols, value) %>% 
  #     ggplot(aes(x= value))+
  #     geom_histogram(color = "white", fill = "darkblue", na.rm = TRUE)+               #change binwidth here
  #     aes(y=stat(count)/sum(stat(count))*100) +     # y is shown in percentages
  #     xlab("ugof")+                            
  #     ylab("Percentage [%]")+
  #     theme_minimal()
  #   
  #   ggplotly(p)
  #   
  #   p
  #   
  # })
  
# code from matlab for modelling ugofs
#   for k= 1: length(listOfFileNames)
#   matfilename = listOfFileNames{:,k};
#   data = xlsread(matfilename); 
#   data = data(:,1);
#   % one sequence, 1000 rhythms
#   a= 0;
#   for rhythm = 0.1:0.01:100
#   clear timesteps count theotime_value theotime_seq x minValue maxdev ugof_value
#   a = a+1;
#   maxoriginal = max(data);
#   timesteps = 1000/rhythm;
#   count = -1;
#   theotime_value = 0;
#   % theotime ioi rhythm
#   while theotime_value < maxoriginal
#   count = count + 1;
#   theotime_value = count * timesteps /1000;
#   theotime_seq(count+1) = theotime_value;
#   end
#   x = length(data);
#   minValue = [];
#   ugof_value = [];
#   for n = 1:x
#   minValue(n) = min(abs(data(n) - theotime_seq.'));
# 
# minValue_mean_ioi = mean(minValue(2:end));
# end 
# % calculate uGof ioi rhythm 
# maxdev = timesteps/2/1000;
# ugof_value = minValue./maxdev;
# m_ugof(a,k) = median(ugof_value(2:end)); %calculate the mean of the sequence, maybe it should be median? 
# end
# end

  })
# 04g: Download Results --------------

  ## Dataset Results----------
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
  
  ## Rerun Dataset Results
  
  datasetInput_rerun <- reactive({
    
    results_rerun
    
  })
  
  output$download_rerun_Data <- downloadHandler(
    filename = function(){
      paste("rerun_rhythm_analysis_", input$savename,"filenr_", input$file_nr,".csv", sep = "")
    },
    content = function(file){
      write.csv(datasetInput_rerun(), file, row.names = FALSE)
    }
  )
   
## Recurrence Plots  
# https://stackoverflow.com/questions/66788578/shiny-export-multiple-figures-dynamically-created-through-renderui
# not working
  
  # output$downloadPlot <- downloadHandler(
  #   filename = "RecurrencePlots.png",
  #   #filename = function(){
  #   #  paste("recurrence_plot",input$savename,"_fs_",input$fs,".png", sep = "")
  #   #},
  #   content = function(file){
  #     #params <- list(Plot_list = plot_list)
  #     #png(plot_list)
  #     png(plot_list, file)
  #     dev.off()
  #   }
  # )
  
} #end server function