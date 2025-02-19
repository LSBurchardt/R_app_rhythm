# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# 
# With this app you can analyse the temporal structure or rhythm of a time series
# i.e. animal vocalizations, human speech, movement patterns, heart beats
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
  
  
# functions ------

    
# Function to save plots in a dynamically created folder automatically after rendering
  #save_plot <- function(plot, filename, folder) {
  #  if (!dir.exists(folder)) {
  #    dir.create(folder, recursive = TRUE)  # Create folder if it doesn't exist
  #  }
  #  saveRDS(plot, file = file.path(folder, filename))
  #}

  ## preparations ----
  
  # create path to save plots 
  observe({
    req(input$savename)  # Ensure save name is available
    
    # Create folder if it doesn't exist
    save_path <<- file.path("plots_app", input$savename)
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
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
      
      } # end of input$go Button
  })# end observe go button
  
# 04b: Standard Calculations ---------------
results <- observe({
    
    if(input$goButton_2 > 0){
  
      rm(results_rhythm, binarydata)
  
    #if(input$all == TRUE){
      
      results_rhythm <<- data.frame()      # <<- global assignment operator, needs to be used when changing as well
      ioi_all <<- list(NA)
      ir_all <<- list(NA)
      #ioi_all <<- data.frame()
      plot_list <<- list(NA)
    
for (a in 1:length(list_of_files)) {
  
     print(a)    # as a checkpoint, if software crashes because of a faulty file
  
  ## load data ----------
         
  #be aware: independent of your column names, they are overwritten to be X1, X2, and X3
  
  # transform any timepoint series to start at 0.1 --> maybe the start is at 47098.9 and the second at 47101.1
  
  if(input$colnames_present == "TRUE"){
  colnames <- TRUE} else {colnames <- FALSE}
  
  
        if (input$fileextension == 'csv'){
          data <- read_delim(paste(path, list_of_files[a], sep = "\\"), delim  = ";", col_names = colnames) 
          colnames(data) <- c("X1", "X2", "X3")
          #colnames(data) <- c("X1", "X2", "X3", "X4")
          data <- data %>%  select(X1, X2, X3) # only short fix for things with 4 columns (i.e. doreco data word level (then also add X4 for selction here) or zf shared)
          } else if (input$fileextension == "xls"){
          data <- read_xls(paste(path, list_of_files[a], sep = "\\"), sheet = 1, col_names = colnames)
          colnames(data) <- c("X1", "X2", "X3")
        } else if (input$fileextension == "xlsx") {
          data <- read.xlsx(paste(path, list_of_files[a], sep = "\\"), sheet = 1, colNames = colnames)
          
          if(nrow(data) == 3){
          colnames(data) <- c("X1", "X2", "X3")
          } else if(nrow(data) == 2){
            colnames(data) <- c("X1", "X2")
          } else if (nrow(data) == 1){
            colnames(data) <- c("X1")}
          
        } else {NULL}

        if (nrow(data) <= 2){
          results_rhythm[a,1] <<- a
          results_rhythm[a,2:14] <<- NA
          next} 
  
  # make sure, every sequence, independent of the actual start point, gets a start point of 0.1
  # this is to decrease calculation times, as some calculations like theo_time start at 0
  
  # make input as question, transformation needed?
  
  #subtractor <- as.numeric(data[1,1])
  data[,1] <- (data[,1]+0.1) - as.numeric(data[1,1])
  

    output$loop_a <- renderText({
         paste("We finished loop", a , "of", length(list_of_files))})
        
         output$table_input_data <- renderTable({
                data
         }) #end renderText

         
         
### subset for element types -----------         
# did you load data with element types in column X3? if-clause to check for third column
# if not, message is shown that all elements are used, because not subsetable for element type        
      
          if("X3" %in% colnames(data) == TRUE) {

           elementlist<- as.vector(NULL)

           if (input$element_a == TRUE)
             elementlist <- c(elementlist, "a") else {NULL} #changed for seal pups, should be a

           if (input$element_b == TRUE)
             elementlist <- c(elementlist, "b") else {NULL} #changed for seal pups, should be b

           if(input$element_c == TRUE)
             elementlist <- c(elementlist, "c") else {NULL}

           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "d") else {NULL}

           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "e") else {NULL}

           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "f") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "g") else {NULL}
           if(input$element_c == TRUE)
             elementlist <- c(elementlist, "h") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "i") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "j") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "k") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "l") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "m") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "n") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "o") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "p") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "q") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "r") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "s") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "t") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "u") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "v") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "w") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "x") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "y") else {NULL}
           
           if(input$element_d == TRUE)
             elementlist <- c(elementlist, "z") else {NULL}
           
# filter data for the chosen element types
           
           elements_seq[a] <<- str_c(data$X3, collapse = "") #needs to be run before(!) filtering the data
           #otherwise the saved sequence will be the sequence that was actually analysed
           
           data <- data %>%
             dplyr::filter(X3 %in% elementlist)
           
           
# output element list to see, which elements where chosen, just a back up to see, if element list
# works correctly
           
           output$elementlist <- renderTable({
             
            elementlist
             
           })

           } else {

             data <- data

          output$element <- renderText({

               print("You did not assign different element types. All elements will be used.")

             })

           } #end of else

         
## ioi calc & plot & duration (if applicable) ----------
         ioi <- data.frame()                 # set up empty dataframe to store ioi values in
         
         for (x in  1:nrow(data)) {          # start of loop through rows of data to calculate iois
           
           z = x+1 
           ioi[x,1] <- data[z,1]-data[x,1]
           
         }                                   #end of loop through rows of data to calculate iois
         
         colnames(ioi) <- c("X1")
         
         ioi_beat <- 1/get(input$method)(ioi$X1, na.rm = TRUE) #the user chooses whether to calculate ioi beat based on median or mean
         ioi_cv <- sd(ioi$X1, na.rm = TRUE)/mean(ioi$X1, na.rm = TRUE)
         ioi_cv_unbiased <-  (1+1/(4*(nrow(ioi)-1)))*ioi_cv
         
         #transforming iois to degrees and radians based on ioi_beat for circular statistics
         # looping through calculated iois
         
         # Function to convert degrees to radians
         deg_to_rad <- function(degrees) {
           return(degrees * pi / 180)
         }
         reference <- 1 / get(input$method)(ioi$X1, na.rm = TRUE) #something wrong here, reference does not seem to work 
         
         for (x in 1:nrow(ioi)){
           
           ioi[x,2] <- ioi[x,1] / reference * 360     
           ioi[x,3] <- deg_to_rad(ioi[x,2])
           
           # Add filename and reference columns
           ioi[x,4] <- list_of_files[a]  # filename
           ioi[x,5] <- reference          # reference
           
         }
         
         colnames(ioi) <- c("ioi", "degree", "radians", "filename", "reference_beat")
         
         #add parameters to results
         results_rhythm[a,1] <<- a
         results_rhythm[a,2] <<- ioi_beat 
         results_rhythm[a,3] <<- ioi_cv_unbiased
         ioi_all[[a]] <<- ioi
### integer ratio calculations ----
         # based on ioi, which is calculated per sequence
         
         n <- nrow(ioi)
         
         # Ensure there are at least 2 intervals to calculate the ratio
         if (n > 1) {
           
           # Create a dataframe to store results for the current sequence
           # Only consider adjacent pairs (i, i+1)
           integer_r <- data.frame(
             i = 1:(n-1),        # Index of the first element in the pair
             j = 2:n             # Index of the second element in the pair (i+1)
           )
           
           # Calculate the ratio for each adjacent pair
           integer_r$ratio <- with(integer_r, ioi$ioi[i] / (ioi$ioi[i] + ioi$ioi[j]))
           integer_r$file <- list_of_files[a]
         }
         
         ir_all[[a]] <<- integer_r
         
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
           
           saveRDS(p, file.path(save_path, "plot_ioi_beat.rds"))
           
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
             ggtitle("Goodness of Fit and Coefficient of Variation")+
             ylim(0,1)
           
           saveRDS(p, file.path(save_path, "plot_beat_precision_cv.rds"))
           
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
           
           saveRDS(p, file.path(save_path, "plot_npvi.rds"))
           
           p <- ggplotly(p)
           
           p
           
         })
         
### ioi hist --------------------
         output$plot_ioi_all <- renderPlotly({
           
           #ioi_all <<- as.data.frame(unlist(ioi_all))
           
           # we append all lists in ioi_all, as they are all the same format, for plotting and saving
           ioi_all <<- do.call(rbind, ioi_all)
           # Compute histogram data separately
           hist_data <- ggplot_build(
             ggplot(ioi_all, aes(x = ioi)) +
               geom_histogram(bins = 30, na.rm = TRUE)
           )$data[[1]]
           
           # Compute percentage
           hist_data <- hist_data %>%
             mutate(percentage = round(count / sum(count) * 100, digits = 2))
           
           # Plot with precomputed values
           p <- ggplot(hist_data, aes(x = xmin, y = percentage)) +
             geom_bar(
               stat = "identity",
               aes(),#text = paste0("Percentage: ", round(percentage, 2), "%")),
               width = diff(hist_data$x)[1],  # Ensures correct bar width
               color = "white",
               fill = "darkblue"
             ) +
             xlab("IOI [sec]") +
             ylab("Percentage [%]") +
             theme_minimal()
           # save plot autoamtically
           
           saveRDS(p, file.path(save_path, "plot_hist_ioi.rds"))
           
           # Convert to plotly with the correct tooltip
           p_plotly <- ggplotly(p)
           p_plotly
           
           p
           
         })
        # integer ratio plot ------
        
output$plot_ir_all <- renderPlotly({
  
  ir_all <<- do.call(rbind, ir_all)
  
  p <- ir_all %>% 
    ggplot(aes(x = ratio))+
    geom_density()+
    coord_cartesian(xlim = c(0, 1))+
    theme_minimal()+
    xlab("Integer Ratios")+
    ylab(" Density")
  
  saveRDS(p, file.path(save_path, "plot_integer_ratio.rds"))
  
  p_plotly <- ggplotly(p)
  
  p_plotly
  
  
}) 
         
         
         # rose plot --------------------         
         
output$plot_rose <- renderPlot({
  
  req(ioi_all)
  # Create a rose plot with averaged radians
  rose_plot <- ioi_all %>% 
    ggplot(aes(x = radians)) +
    #geom_histogram(binwidth = 2 * pi / 30, fill = "darkblue", color = "white") +
    geom_histogram(fill = "darkblue", color = "white")+
    coord_polar(start = 0) +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust radial limits
    labs(x = "Radians") +
    ggtitle(" Polar Plot: IOIs in Reference to IOI Beat")+
    theme_minimal()
  
  saveRDS(rose_plot, file.path(save_path, "plot_ioi_rose.rds"))
  
  rose_plot
  
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
         
         results_rhythm[a,4] <<- npvi 
         
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

           for ( i in 1: nrow(event_timepoint_fs)){

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
         L <- length(X)                               # length of actual signal 
         kk <- nrow(k)                                #number of elements in sequence! save!
         
         fs <- input$fs 
         X <- 1/L * SynchWave::fftshift(fft(X,L))
         df <- fs/L                             #frequency resolution
         sample_index <- (-L/2):(L/2-1)         #ordered index for FFT plot
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
         beat_ioi <- results_rhythm[a,2]
         beat_fft <- results_rhythm[a,5]

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

### npvi of ugof values from ioi beat
         
         z <- c()
         b <- c()
         
         for (l in 1: length(ugof_value_beat)){
           
           #z[l] <- (ugof_value_beat[l,1] - ugof_value_beat[l+1,1])
           #b[l] <- (ugof_value_beat[l,1] + ugof_value_beat[l+1,1])/2
           
           z[l] <- (ugof_value_beat[l] - ugof_value_beat[l+1])
           b[l] <- (ugof_value_beat[l] + ugof_value_beat[l+1])/2
         } #end of for loop
         
         z <- na.omit(z)
         b <- na.omit(b)
         c <- sum(abs(z/b))
         
         npvi_ugof_ioi <- c*(100/(length(z)-1))


### cv of ugof values
         
         ugof_ioi_cv <- sd(ugof_value_beat, na.rm = TRUE)/mean(ugof_value_beat, na.rm = TRUE)
         ugof_ioi_cv_unbiased <-  (1+1/(4*(length(ugof_value_beat)-1)))*ugof_ioi_cv

### cv of duration of elements (if applicable, then to be extracted from column 4 of input data and to be written to column 19 of results later)         

        # if(ncol(data) == 4){
        #   duration_cv <- sd(data$X4, na.rm = TRUE)/mean(data$X4, na.rm = TRUE)
        #   duration_cv_unbiased <-  (1+1/(4*(length(nrow(data))-1)))*duration_cv
        # }
         
### save ugof values per sequence in list as R document 
         
         
                 
###recurrence ugof ioi -------------
         
         # output$rec_ugof_plots <- renderUI({
         #   plot_output_list <- lapply(1:length(list_of_files), function(i) {
         #     plotname <- paste("ugof_plot", i, sep="")
         #     plotlyOutput(plotname, height = 300, width = 300)
         #   }) # end lapply
         #   
         #   do.call(tagList, plot_output_list)
         # }) 
         # 
         # local({
         # 
         # eucl_dist_ugof <- (as.matrix(vegdist(ugof_value_beat, "euclidean", na.rm = TRUE)))
         # eucl_dist_ugof <- eucl_dist_ugof[1:(nrow(eucl_dist_ugof)-1),1:(nrow(eucl_dist_ugof)-1) ]
         # 
         # threshold <- mean(ugof_value_beat, na.rm = TRUE)*0.1 # as input?
         # 
         # eucl_dist_ugof[eucl_dist_ugof < threshold] <- 0
         
# transform matrix as to be able to plot it with ggplot as tile plot
 #https://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2
         
      #    levels <- 1:(nrow(eucl_dist_ugof))
      #    
      #    eucl_dist_ugof_2 <- eucl_dist_ugof %>%
      #      tibble::as_tibble() %>%
      #      rownames_to_column('Var1') %>%
      #      gather(Var2, value, -Var1) %>%
      #      mutate(
      #        Var1 = factor(Var1, levels = levels),
      #        Var2 = factor(gsub("V", "", Var2), levels = levels)
      #      )
      #    
      #    my_i <- a
      #    plotname <- paste("ugof_plot", my_i, sep="")
      #    
      #    output[[plotname]] <- renderPlotly({
      #     
      #   rec_plot_ugof <- ggplot(eucl_dist_ugof_2, aes(Var1, Var2)) +
      #      geom_tile(aes(fill = value)) +
      #      # geom_text(aes(label = round(value, 1))) +
      #      scale_fill_gradient(low = "white", high = "black")+
      #      xlab("#ugof")+
      #      ylab("#ugof")+
      #      coord_fixed(ratio=1)+
      #      ggtitle(paste("ugof ioi,File:", results_rhythm$filename[my_i]))+
      #      theme_minimal()+
      #      theme(
      #        plot.background = element_rect(fill = "white"),
      #        panel.grid = element_blank(),
      #        title = element_text(size = 6))
      #    
      #    rec_plot_ugof<- ggplotly(rec_plot_ugof)
      #    
      #    rec_plot_ugof
      #    
      #    }) #end renderPlotly
      # }) #end local
         
  ### end recurrence ugof
         results_rhythm[a,9] <<- m_ugof_beat_1 
         results_rhythm[a,10] <<- mean(min_value, na.rm = TRUE)
         silent_beat_ioi <- nrow(theotime_seq)-kk
         results_rhythm[a,11] <<- silent_beat_ioi
         
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
            
            
### calculate npvi of ugof values of fourier beat            
            z <- c()
            b <- c()
            
            for (l in 1: length(ugof_value_beat_2)){
              
              z[l] <- (ugof_value_beat_2[l] - ugof_value_beat_2[l+1])
              b[l] <- (ugof_value_beat_2[l] + ugof_value_beat_2[l+1])/2
              
            } #end of for loop
            
            z <- na.omit(z)
            b <- na.omit(b)
            c <- sum(abs(z/b))
            
            npvi_ugof_fft <- c*(100/(length(z)-1)) 
### calculate cv of ugof values of fourier beat
            
            ugof_fft_cv <- sd(ugof_value_beat, na.rm = TRUE)/mean(ugof_value_beat_2, na.rm = TRUE)
            ugof_fft_cv_unbiased <-  (1+1/(4*(length(ugof_value_beat_2)-1)))*ugof_fft_cv
           
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
            
            
            results_rhythm[a,12] <<- m_ugof_beat_2
            results_rhythm[a,13] <<- mean(min_value_2, na.rm = TRUE)
            silent_beat_fft <- nrow(theotime_seq_2)-kk
           
            results_rhythm[a,14] <<- silent_beat_fft} else {
              
              results_rhythm[a,12] <<- NA
              results_rhythm[a,13] <<- NA
              results_rhythm[a,14] <<- NA
              
            }
         
         results_rhythm[a,15] <<- npvi_ugof_ioi
         results_rhythm[a,16] <<- ugof_ioi_cv_unbiased
         results_rhythm[a,17] <<- npvi_ugof_fft
         results_rhythm[a,18] <<- ugof_fft_cv_unbiased
         
        # if(ncol(data) == 4){
        #   results_rhythm[a,19] <<- duration_cv_unbiased
        # }
        
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
      
     
      #} #end of input$all == TRUE
      
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
      averaging <- input$method
      

      results_rhythm <<- cbind(results_rhythm, fs,averaging,elements,elements_raw, filenames, savename)
      
      #if (nrow(data) == 3){
        
      colnames(results_rhythm) <<- c("index", "ioi_beat", "unbiased_cv",  "npvi", "fourier_beat", "freq_reso",
                                     "n_elements","signal_length","ugof_ioi","mean_min_dev_ioi","silent_beats_ioi",
                                     "ugof_fft","mean_min_dev_fft","silent_beats_fft","npvi_ugof_ioi","cv_ugof_ioi","npvi_ugof_fft","cv_ugof_fft", "fs","averaging", "elements","raw_element_seq",
                                     "filename", "savename")
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
# The rerun is currenlty only calculating ioi beat and ugof
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
      
      # match original sequence to theoretical time series and calculate actual deviation
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
  
#   output$warning_ugof_detail <- renderUI({
#     
#     HTML(paste("<b>Important Remarks:</b>",
#     "1) You need to calculate the standard results first, for this analysis to work! (GO Button in the Sidepanel)",
#     "2) Running this analysis is quite time intensive, as all ugofs are calculated between
#     0.1 and 100 Hz for all input files you chose. The suggestion is, to run the analysis for the
#     example data set of 10 short files, to get an idea of how long this process takes on your
#     local machine. In development this took about 5 minutes with 8GB of RAM and a processor with 
#     4 CPU cores and 1.8 Ghz.",   
#     "Are you sure you want to run this analysis?", sep ="<br/>")
#     )
#   })
#   
#   observeEvent(input$ugof_detail, {  
#   
#   maxdev_plot <- vector()
#   count <- 0
#     
# for (x in seq(from = 0.1, to= 100, by = 0.1)){
#     
#       count <- count + 1
#     
#       timestep <- 1000/x
#     
#       maxdev_plot[count] <- timestep/2
#     }
# 
#   maxdev_plot <- as.data.frame(maxdev_plot)
#   
#   maxdev_plot <- maxdev_plot %>% 
#     mutate(beat = seq(0.1,100,0.1),
#            maxdev = maxdev_plot/1000) #transform max dev from ms to seconds
#   
#   
# 
# ## modelling ugofs ----------------
# # for various rhythms as calculated with 
# 
#   m_ugof <- data.frame()
#   min_value_all <- data.frame()
#   for (k in 1:length(list_of_files)){
#     
#     if (input$fileextension == 'csv'){
#       data_ugof <- read_delim(paste(path, list_of_files[k], sep = "\\"), delim  = ",", col_names = TRUE)
#       colnames(data_ugof) <- c("X1", "X2", "X3")
#     } else if (input$fileextension == "xls"){
#       data_ugof <- read_xls(paste(path, list_of_files[k], sep = "\\"), sheet = 1, col_names = FALSE)
#       colnames(data_ugof) <- c("X1", "X2", "X3")
#     } else if (input$fileextension == "xlsx") {
#       data_ugof <- read.xlsx(paste(path, list_of_files[k], sep = "\\"), sheet = 1, colNames = FALSE)
#       colnames(data_ugof) <- c("X1", "X2", "X3")
#     } else {NULL}
#     
#     
#     data_ugof <- data_ugof[,1]
#     
#     a <- 0
#     
#     for (rhythm in seq(from = 0.1, to = 100, by = 0.1)){
#     #for (rhythm in c(1,10)){
#       
#       b <- 0
#       a <- a +1
#       maxoriginal <- max(data_ugof)
#       timesteps <- 1000/rhythm
#       count <- -1     # needs to be -1, so that it is 0 in the first loop and the counter still works
#       theotime_value <- 0
#       theotime_seq <- data.frame()
#       
#       while (theotime_value < maxoriginal){
#         b <- b+1
#         count <- count + 1
#         theotime_value <- count * timesteps/1000
#         theotime_seq[b,1] <- theotime_value  
#       }
#         x <- nrow(data_ugof)
#         min_value <- c(1:x)
#         ugof_value_beat <- c()
# 
#         for (n in 1:x){
# 
#           min_value[n] <- min(abs(as.numeric(data_ugof[n,1])- theotime_seq))
# 
#         }
# 
#         # calculate uGof
# 
#         maxdev <- timesteps/2/1000
# 
#         ugof_value_beat <- min_value/maxdev
# 
#          
#         m_ugof[a,k] <- median(ugof_value_beat[1:length(ugof_value_beat)])
#       } # end for loop rhythms
#       } # end for loop files
#   
# ## z-score calculation ---------------
#   
# #zscore: z=(x-mean)/standard deviaton
# # mean = mean of modelled distribution
# # standard deviation = standard deviation of modeled distribution
# # x = ugof value
#   
#   ugof_distribution <- gather(m_ugof, cols, value) %>% 
#     summarize_at("value", list(mean = mean, std =sd))
#   
#   
#   zscore_fun <- function(data, mean, std){
#     
#     (data-mean)/std
#     
#   }
#   
#   z_scores <-  zscore_fun(results_rhythm$ugof_ioi, ugof_distribution$mean, ugof_distribution$std)
#   z_scores_sig <- as.data.frame(z_scores)
#   
# for (score in 1:length(z_scores)){
#   if (z_scores[score] <= -1.65){
#             z_scores_sig[score, 2] <- "sig_good"
#   } else if (z_scores[score] >= 1.65){
#        z_scores_sig[score, 2] <- "sig_bad"
#          } else {z_scores_sig[score, 2] <- "non_sig"}
# }# end for loop scores
# 
#   
# # zscores need to be included in some final output or be downloadable in the
# # corresponidng tab with ugof, beat, zscore, mean distribution, std distribution
# # file and ...?
#   
# ## output -------------
#   
#     output$maxdev100 <- renderPlotly({
#     
#       # add a legend to this to distinguish between max possible, ioi and fft
#       
#     p <- ggplot()+
#       geom_jitter(data = maxdev_plot,aes(x = beat, y = maxdev),
#                   width = 0.2, alpha = 0.5, shape = 1, size = 0.5)+
#       geom_jitter(data = results_rhythm, aes(x = `ioi_beat`, y = `mean_min_dev_ioi`),
#                   color = "blue")+
#       geom_jitter(data = results_rhythm, aes(x = `fourier_beat`, y = `mean_min_dev_fft`),
#                   color = "darkgreen")+
#       ylab("Deviation in sec")+
#       xlab("Beat [Hz]")+
#       theme_minimal()+
#       ggtitle("Maximum possible deviation from 1 to 100 Hz and calculated deviations")+
#       coord_cartesian(ylim = c(0.001,10))+
#       scale_y_log10(labels = scales::label_comma(accuracy = 0.01))
#     
#     p <- ggplotly(p)
#     
#     p
#     
#   })
#   
#   output$ugof_hist <- renderPlotly({
# 
#     p <- gather(m_ugof, cols, value) %>%
#       ggplot(aes(x= value))+
#       geom_histogram(color = "white", fill = "darkblue", na.rm = TRUE)+               #change bin width here if necessary
#       aes(y=stat(count)/sum(stat(count))*100) +     # y is shown in percentages
#       xlab("ugof")+
#       ylab("Percentage [%]")+
#       theme_minimal()
# 
#     p <- ggplotly(p)
# 
#     p
# 
#   }) #end renderPlotly ugof_hist
#   
#   output$ugof_zscore <- renderPlotly({
#     
#     sig_data <- cbind(z_scores_sig, results_rhythm$ioi_beat)
#     colnames(sig_data) <- c("zscore", "significance", "ioi_beat")
#     
#     p <-  sig_data %>% 
#       ggplot(aes(x= ioi_beat, y = zscore, fill = significance ))+
#       geom_point(size = 5)+
#       xlab("Beat [Hz]")+
#       ylab("z-score")+
#       #scale_x_continuous(limits = c(0,100))+
#       theme_minimal(base_size = 16)+
#       theme(text=element_text(size=20))#+ #used to be family="serif", changed back to arial(default)
#       # scale_colour_manual(name = "Significance",
#       # labels = c("not sig",
#       #            "bad sig",
#       #            "good sig"),
#       # values = c("#D55E00","#E69F00", "#CC79A7"))+ #"#D55E00" "#0072B2"
#       # scale_fill_manual(name = "Significance",
#       #                   labels = c("not sig",
#       #                              "bad sig",
#       #                              "good sig"),
#       #                   values = c("#D55E00","#E69F00", "#CC79A7"))
#     
#     p <- ggplotly(p)
#     
#     p
#     
#   }) # end render Plotly ugof-zscore
#   
# 
#     }) # end observeEvent input$ugof_detail
  
# 04g: Download Results --------------

  ## Dataset Results----------
  datasetInput <- reactive({
    
    results_rhythm
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("rhythm_analysis_",input$method,"_", input$savename,"_fs_",input$fs,".csv", sep = "")
    },
    content = function(file){
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  ## IOIs ----------
  datasetInput_ioi <- reactive({
    
    ioi_all
    
  })
  
  output$downloadData_ioi <- downloadHandler(
    filename = function(){
      paste("iois_", input$savename,".csv", sep = "")
    },
    content = function(file){
      write.csv(datasetInput_ioi(), file, row.names = FALSE)
    }
  )

  ## Integer ratios ----
  datasetInput_ir <- reactive({
    
    
    ir_all
    
  })  
    
  output$downloadData_ir <- downloadHandler(
    filename = function(){
      paste("integer_ratios_raw", input$savename,".csv", sep = "")
    },
    content = function(file){
      write.csv(datasetInput_ir(), file, row.names = FALSE)
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
  
} #end server function