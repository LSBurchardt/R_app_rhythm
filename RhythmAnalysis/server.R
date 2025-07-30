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

# Server function ---------------------------------------------------------
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
  
  # 04: Preparations ----
  
  ## 04a: create path to save plots ----
  observe({
    req(input$savename)  # Ensure save name is available
    
    # Create folder if it doesn't exist
    save_path <<- file.path("plots_app", input$savename)
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
  })
  
  ## 04b: choosing data to analyse  -------------------
  
  # File extension, Textoutput which file extension was chosen
  pattern <- reactive({
    input$fileextension
  })
  
  output$files <- renderText({
    paste("You chose the following file extension pattern of the input data",
          pattern(),
          ".")
  })
  
  # Directory and Files,
  observe({
    if (input$goButton_1 > 0) {
      path <<- choose.dir()
      pattern <<- pattern()
      list_of_files <<- list.files(path = path, pattern = pattern)
      
      
      
      output$list_files <- renderTable({
        #list.files(path = path, pattern = pattern)
        
        list <- list.files(path = path, pattern = pattern)
        list <- as.data.frame(list)
        
        list$index <- row.names(list)
        colnames(list) <- c("file", "index")
        list
      }) # end renderTable
      
    } # end of input$go Button
  })# end observe go button
  
  ## 04c: Check if Files are read correctly -----
  
  observeEvent(input$check_button, {
    req(length(list_of_files) > 0)
    req(input$sep_input)
    
    file_path <- paste(path, list_of_files[[1]], sep = "\\")
    sep <- input$sep_input
    
    # Try reading the file; handle errors gracefully
    tryCatch({
      data_preview <- readr::read_delim(file_path, delim = sep, n_max = 6)
      
      output$file_preview <- renderPrint({
        head(data_preview)
      })
    }, error = function(e) {
      output$file_preview <- renderPrint({
        cat("Error reading file:\n", e$message)
      })
    })
  })
  
  ## 4d: additional elements ----
  
  output$element_checkboxes <- renderUI({
    # Only show elements beyond "j"
    extra_elements <-
      elementlist[elementlist %in% letters[11:26]]  # k to z
    
    checkboxGroupInput(
      inputId = "element_extra",
      label = "Select additional elements (k–z):",
      choices = extra_elements,
      selected = NULL  # or selected = extra_elements if you want preselection
    )
  })
  
  observeEvent(input$toggle_extra_elements, {
    shinyjs::toggle(id = "extra_element_ui")
  })
  
  # 05: Calculations ---------------
  results <- observe({
    if (input$goButton_2 > 0) {
      rm(results_rhythm, binarydata)
      
      #if(input$all == TRUE){
      
      results_rhythm <<-
        data.frame()      # <<- global assignment operator, needs to be used when changing as well
      ioi_all <<- list(NA)
      ir_all <<- list(NA)
      #ioi_all <<- data.frame()
      plot_list <<- list(NA)
      
      raw_deviations_bp <<- data.frame(
        filename = character(),
        obs_number = integer(),
        obs_value = numeric(),
        matched_theo = numeric(),
        raw_deviation = numeric(),
        abs_deviation = numeric(),
        best_shift = numeric(),
        ugof_value_beat = numeric(),
        stringsAsFactors = FALSE
      )
      
      for (a in 1:length(list_of_files)) {
        print(a)    # as a checkpoint, if software crashes because of a faulty file
        
        ## load data ----------
        
        #be aware: independent of your column names, they are overwritten to be X1, X2, and X3
        
        # transform any timepoint series to start at 0.1 --> maybe the start is at 47098.9 and the second at 47101.1
        
        if (input$colnames_present == "TRUE") {
          colnames <- TRUE
        } else {
          colnames <- FALSE
        }
        
        req(input$sep_input)
        sepa <- input$sep_input
        
        if (input$fileextension == 'csv') {
          data <-
            readr::read_delim(
              paste(path, list_of_files[a], sep = "\\"),
              delim  = sepa ,
              col_names = colnames
            )
          colnames(data) <- c("X1", "X2", "X3")
          #colnames(data) <- c("X1", "X2", "X3", "X4")
          data <-
            data %>%  select(X1, X2, X3) # only short fix for things with 4 columns (i.e. doreco data word level (then also add X4 for selction here) or zf shared)
        } else if (input$fileextension == "xls") {
          data <-
            read_xls(
              paste(path, list_of_files[a], sep = "\\"),
              sheet = 1,
              col_names = colnames
            )
          colnames(data) <- c("X1", "X2", "X3")
        } else if (input$fileextension == "xlsx") {
          data <-
            read.xlsx(
              paste(path, list_of_files[a], sep = "\\"),
              sheet = 1,
              colNames = colnames
            )
          
          if (nrow(data) == 3) {
            colnames(data) <- c("X1", "X2", "X3")
          } else if (nrow(data) == 2) {
            colnames(data) <- c("X1", "X2")
          } else if (nrow(data) == 1) {
            colnames(data) <- c("X1")
          }
          
        } else {
          NULL
        }
        
        if (nrow(data) <= 2) {
          results_rhythm[a, 1] <<- a
          results_rhythm[a, 2:14] <<- NA
          next
        }
        
        # make sure, every sequence, independent of the actual start point, gets a start point of 0.1
        # this is to decrease calculation times, as some calculations like theo_time start at 0
        
        # make input as question, transformation needed?
        
        #subtractor <- as.numeric(data[1,1])
        data[, 1] <- (data[, 1] + 0.1) - as.numeric(data[1, 1])
        
        
        output$loop_a <- renderText({
          paste("We finished loop", a , "of", length(list_of_files))
        })
        
        output$table_input_data <- renderTable({
          data
        }) #end renderText
        
        
        
        ### subset for element types -----------
        # did you load data with element types in column X3? if-clause to check for third column
        # if not, message is shown that all elements are used, because not subsetable for element type
        
        if ("X3" %in% colnames(data)) {
          # 1. Collect fixed checkboxes (a–j)
          elementlist <- c(
            if (isTRUE(input$element_a))
              "a"
            else
              NULL,
            if (isTRUE(input$element_b))
              "b"
            else
              NULL,
            if (isTRUE(input$element_c))
              "c"
            else
              NULL,
            if (isTRUE(input$element_d))
              "d"
            else
              NULL,
            if (isTRUE(input$element_e))
              "e"
            else
              NULL,
            if (isTRUE(input$element_f))
              "f"
            else
              NULL,
            if (isTRUE(input$element_g))
              "g"
            else
              NULL,
            if (isTRUE(input$element_h))
              "h"
            else
              NULL,
            if (isTRUE(input$element_i))
              "i"
            else
              NULL,
            if (isTRUE(input$element_j))
              "j"
            else
              NULL
          )
          
          # 2. Combine with dynamically selected extras (k–z)
          if (!is.null(input$element_extra)) {
            elementlist <- c(elementlist, input$element_extra)
          }
          
          # 3. Save the unfiltered full element sequence
          elements_seq[a] <<- str_c(data$X3, collapse = "")
          
          # 4. Filter the data for selected element types
          data <- data %>% dplyr::filter(X3 %in% elementlist)
        }
        
        # if("X3" %in% colnames(data) == TRUE) {
        #
        #  elementlist<- as.vector(NULL)
        #
        #  if (input$element_a == TRUE)
        #    elementlist <- c(elementlist, "a") else {NULL} #changed for seal pups, should be a
        #
        #  if (input$element_b == TRUE)
        #    elementlist <- c(elementlist, "b") else {NULL} #changed for seal pups, should be b
        #
        #  if(input$element_c == TRUE)
        #    elementlist <- c(elementlist, "c") else {NULL}
        #
        #  if(input$element_d == TRUE)
        #    elementlist <- c(elementlist, "d") else {NULL}
        #
        #  if(input$element_e == TRUE)
        #    elementlist <- c(elementlist, "e") else {NULL}
        #
        #  if(input$element_f == TRUE)
        #    elementlist <- c(elementlist, "f") else {NULL}
        #
        #  if(input$element_g == TRUE)
        #    elementlist <- c(elementlist, "g") else {NULL}
        #
        #  if(input$element_h == TRUE)
        #    elementlist <- c(elementlist, "h") else {NULL}
        #
        #  if(input$element_i == TRUE)
        #    elementlist <- c(elementlist, "i") else {NULL}
        #
        #  if(input$element_j == TRUE)
        #    elementlist <- c(elementlist, "j") else {NULL}
        #
        # if(input$element_k == TRUE)
        #   elementlist <- c(elementlist, "k") else {NULL}
        #
        # if(input$element_l == TRUE)
        #   elementlist <- c(elementlist, "l") else {NULL}
        #
        # if(input$element_m == TRUE)
        #   elementlist <- c(elementlist, "m") else {NULL}
        #
        # if(input$element_n == TRUE)
        #   elementlist <- c(elementlist, "n") else {NULL}
        #
        # if(input$element_o == TRUE)
        #   elementlist <- c(elementlist, "o") else {NULL}
        #
        # if(input$element_p == TRUE)
        #   elementlist <- c(elementlist, "p") else {NULL}
        #
        # if(input$element_q == TRUE)
        #   elementlist <- c(elementlist, "q") else {NULL}
        #
        # if(input$element_r == TRUE)
        #   elementlist <- c(elementlist, "r") else {NULL}
        #
        # if(input$element_s == TRUE)
        #   elementlist <- c(elementlist, "s") else {NULL}
        #
        # if(input$element_t == TRUE)
        #   elementlist <- c(elementlist, "t") else {NULL}
        #
        # if(input$element_u == TRUE)
        #   elementlist <- c(elementlist, "u") else {NULL}
        #
        # if(input$element_v == TRUE)
        #   elementlist <- c(elementlist, "v") else {NULL}
        #
        # if(input$element_w == TRUE)
        #   elementlist <- c(elementlist, "w") else {NULL}
        #
        # if(input$element_x == TRUE)
        #   elementlist <- c(elementlist, "x") else {NULL}
        #
        # if(input$element_y == TRUE)
        #   elementlist <- c(elementlist, "y") else {NULL}
        #
        # if(input$element_z == TRUE)
        #   elementlist <- c(elementlist, "z") else {NULL}
        
        # filter data for the chosen element types
        
        #elements_seq[a] <<- str_c(data$X3, collapse = "") #needs to be run before(!) filtering the data
        #otherwise the saved sequence will be the sequence that was actually analysed
        
        #data <- data %>%
        #   dplyr::filter(X3 %in% elementlist)
        
        
        # output element list to see, which elements where chosen, just a back up to see, if element list
        # works correctly
        
        output$elementlist <- renderTable({
          elementlist
          
        })
        
        #  } else {
        #
        #    data <- data
        #
        # output$element <- renderText({
        #
        #      print("You did not assign different element types. All elements will be used.")
        #
        #    })
        #
        #  } #end of else
        
        
        ## ioi calc & plot & duration (if applicable) ----------
        ioi <-
          data.frame()                 # set up empty dataframe to store ioi values in
        
        for (x in  1:nrow(data)) {
          # start of loop through rows of data to calculate iois
          
          z = x + 1
          ioi[x, 1] <- data[z, 1] - data[x, 1]
          
        }                                   #end of loop through rows of data to calculate iois
        
        colnames(ioi) <- c("X1")
        
        ioi_beat <-
          1 / get(input$method)(ioi$X1, na.rm = TRUE) #the user chooses whether to calculate ioi beat based on median or mean
        ioi_cv <-
          sd(ioi$X1, na.rm = TRUE) / mean(ioi$X1, na.rm = TRUE)
        ioi_cv_unbiased <-  (1 + 1 / (4 * (nrow(ioi) - 1))) * ioi_cv
        
        #transforming iois to degrees and radians based on ioi_beat for circular statistics
        # looping through calculated iois
        
        # Function to convert degrees to radians
        deg_to_rad <- function(degrees) {
          return(degrees * pi / 180)
        }
        reference <-
          1 / get(input$method)(ioi$X1, na.rm = TRUE) #something wrong here, reference does not seem to work
        
        for (x in 1:nrow(ioi)) {
          ioi[x, 2] <- ioi[x, 1] / reference * 360
          ioi[x, 3] <- deg_to_rad(ioi[x, 2])
          
          # Add filename and reference columns
          ioi[x, 4] <- list_of_files[a]  # filename
          ioi[x, 5] <- reference          # reference
          
        }
        
        colnames(ioi) <-
          c("ioi",
            "degree",
            "radians",
            "filename",
            "reference_beat")
        
        #add parameters to results
        results_rhythm[a, 1] <<- a
        results_rhythm[a, 2] <<- ioi_beat
        results_rhythm[a, 3] <<- ioi_cv_unbiased
        ioi_all[[a]] <<- ioi
        ### integer ratio calculations ----
        # based on ioi, which is calculated per sequence
        
        n <- nrow(ioi)
        
        # Ensure there are at least 2 intervals to calculate the ratio
        if (n > 1) {
          # Create a dataframe to store results for the current sequence
          integer_r <- expand.grid(i = 1:n, j = 1:n)
          integer_r <- integer_r[integer_r$i != integer_r$j,]
          
          # Only consider adjacent pairs (i, i+1)
          #integer_r <- data.frame(
          #   i = 1:(n-1),        # Index of the first element in the pair
          #   j = 2:n             # Index of the second element in the pair (i+1)
          # )
          
          # Calculate the ratio for all pair
          integer_r$ratio <-
            with(integer_r, ioi$ioi[i] / (ioi$ioi[i] + ioi$ioi[j]))
          integer_r$file <- list_of_files[a]
        }
        
        ir_all[[a]] <<- integer_r
        
        ### plot Beats ------------
        output$plot_beat <- renderPlotly({
          p <- ggplot(data = results_rhythm,) +
            geom_jitter(
              aes(
                x = "IOI",
                y = ioi_beat,
                color = filename
              ),
              width = 0.1,
              alpha = 0.8
            ) +
            geom_jitter(aes(
              x = "Fourier",
              y = fourier_beat,
              color = filename
            ),
            width = 0.1,
            ) +
            ylab("Beat [Hz]") +
            xlab("Beat Parameter") +
            theme_minimal() +
            ggtitle("Beat Results in Hertz") +
            ylim(0, input$fs / 2)
          
          saveRDS(p, file.path(save_path, "plot_ioi_beat.rds"))
          
          p <- ggplotly(p)
          
          p
          
        })
        
        ### plot ugof ----------------
        output$plot_ugof <- renderPlotly({
          p <- ggplot(data = results_rhythm) +
            geom_jitter(
              aes(
                x = "IOI ugof",
                y = ugof_ioi,
                color = filename
              ),
              width = 0.1,
              alpha = 0.8
            ) +
            geom_jitter(
              aes(
                x = "Fourier ugof",
                y = ugof_fft,
                color = filename
              ),
              width = 0.1,
              alpha = 0.8
            ) +
            geom_jitter(
              aes(
                x = "Coefficient of Variation",
                y = unbiased_cv,
                color = filename
              ),
              width = 0.1,
              alpha = 0.8
            ) +
            ylab("Value") +
            xlab("Parameter") +
            theme_minimal() +
            ggtitle("Goodness of Fit and Coefficient of Variation") +
            ylim(0, 1)
          
          saveRDS(p,
                  file.path(save_path, "plot_beat_precision_cv.rds"))
          
          p <- ggplotly(p)
          
          p
          
        })
        ### plot Variability Parameter ----------
        output$plot_var <- renderPlotly({
          p <- ggplot(data = results_rhythm) +
            geom_jitter(
              aes(
                x = "npvi",
                y = npvi,
                color = filename
              ),
              width = 0.1,
              alpha = 0.8
            ) +
            ylab("Value") +
            xlab("") +
            theme_minimal() +
            ggtitle("normalized Pairwise Variability Index") +
            ylim(0, max(results_rhythm$npvi))
          
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
          hist_data <- ggplot_build(ggplot(ioi_all, aes(x = ioi)) +
                                      geom_histogram(bins = 30, na.rm = TRUE))$data[[1]]
          
          # Compute percentage
          hist_data <- hist_data %>%
            mutate(percentage = round(count / sum(count) * 100, digits = 2))
          
          # Plot with precomputed values
          p <- ggplot(hist_data, aes(x = xmin, y = percentage)) +
            geom_bar(
              stat = "identity",
              aes(),
              #text = paste0("Percentage: ", round(percentage, 2), "%")),
              width = diff(hist_data$x)[1],
              # Ensures correct bar width
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
            ggplot(aes(x = ratio)) +
            geom_density() +
            coord_cartesian(xlim = c(0, 1)) +
            theme_minimal() +
            xlab("Integer Ratios") +
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
            geom_histogram(fill = "darkblue", color = "white") +
            coord_polar(start = 0) +
            scale_y_continuous(limits = c(0, NA)) +  # Adjust radial limits
            labs(x = "Radians") +
            ggtitle(" Polar Plot: IOIs in Reference to IOI Beat") +
            theme_minimal()
          
          saveRDS(rose_plot,
                  file.path(save_path, "plot_ioi_rose.rds"))
          
          rose_plot
          
        })
        
        ## npvi calculations (ioi_seq) -----------
        
        ioi_seq <- drop_na(ioi)
        z <- c()
        b <- c()
        
        for (l in 1:nrow(ioi_seq)) {
          z[l] <- (ioi_seq[l, 1] - ioi_seq[l + 1, 1])
          b[l] <- (ioi_seq[l, 1] + ioi_seq[l + 1, 1]) / 2
          
        } #end of for loop
        
        z <- na.omit(z)
        b <- na.omit(b)
        c <- sum(abs(z / b))
        
        npvi <- c * (100 / (length(z) - 1))
        
        results_rhythm[a, 4] <<- npvi
        
        ## end npvi
        
        ## fourier calculations -----------
        
        ### binary -----------
        
        #transform time labels to binary sequence for Fourier analysis
        
        event_timepoint <- vector(length = nrow(data))
        event_timepoint <- data [, 1]
        
        event_timepoint_fs <-
          round(event_timepoint * input$fs) # index of events with fs chosen in interface
        
        event_timepoint_fs <- as.data.frame(event_timepoint_fs)
        
        binarydata <<-
          data.frame(matrix(
            ncol = 1,
            nrow = max(event_timepoint_fs),
            0
          ))
        colnames(binarydata) <<- c('timeseries')
        
        for (l in 1:max(event_timepoint_fs)) {
          for (i in 1:nrow(event_timepoint_fs)) {
            if (l == event_timepoint_fs[i, 1]) {
              binarydata[l, 1] <<- 1
              
            }         # end of if
            
          }          # end of for loop i
          
        }           #end of for loop l
        
        ### end binary
        
        ### fourier calc ----------
        
        #https://statisticsglobe.com/find-index-positions-non-zero-values-matrix-r
        
        k <-
          which(binarydata != 0, arr.ind = T)   # find outputs all values unequal zero -> finds all 1's, saves indices in k
        X <-
          binarydata$timeseries[min(k[, 1]):max(k[, 1])]             # X in binary data cut to start and end with 1
        L <-
          length(X)                               # length of actual signal
        kk <-
          nrow(k)                                #number of elements in sequence! save!
        
        fs <- input$fs
        X <- 1 / L * SynchWave::fftshift(fft(X, L))
        df <-
          fs / L                             #frequency resolution
        sample_index <-
          (-L / 2):(L / 2 - 1)         #ordered index for FFT plot
        f <- sample_index * df
        
        index_0 <-
          which.min(abs(f - 0))      #index where f is zero to select only positive fs
        X <-
          X[(index_0 - 1):length(X)]   # select only amplitudes for positive fs
        f <-
          f[(index_0 - 1):length(f)]   # select only positive fs
        
        # https://stackoverflow.com/questions/14968954/return-index-from-a-vector-of-the-value-closest-to-a-given-element
        # https://www.oreilly.com/library/view/the-r-book/9780470510247/ch002-sec020.html
        
        
        # find peaks in fft signal
        
        peaks <-
          findpeaks(abs(X), npeaks = 11, sortstr = TRUE) #sortstr = TRUE so that it is sorted decreasing as need and as done in matlab
        peaks <-
          peaks %>%      # column 1: amplitude/peak height, column 2: index, column 3 and 4 start and end of peak, delted, because we do not need it
          as.data.frame() %>%
          select(V1, V2)
        
        colnames(peaks) <- c("amplitude", "index")
        
        # we know amplitude and index but not yet the corresponding frequency
        # we now need to couple index with corresponding frequency from f
        
        # f at indexes in peaks$index
        peak_freq <- f[peaks$index]
        # add frequency to the amplitude dataframe
        peaks[, "freq"] <- peak_freq
        
        #Account for shift in zero-bin component
        
        if (peaks[1, 3] != 0) {
          peaks$freq <- peaks$freq - peaks$freq[1]
          
        }
        
        #save data
        
        results_rhythm[a, 5] <<- peaks$freq[2] # fourier beat
        results_rhythm[a, 6] <<-
          df            # frequency resolution
        results_rhythm[a, 7] <<-
          kk            # number of elements
        results_rhythm[a, 8] <<- L             # length of signal
        
        ### end fourier calc
        ## end fourier
        
        ## beat precision -------------
        # former: ugof - universal-goodness-of-fit
        
        rm(maxoriginal, timesteps, theotime_value, theotime_seq)
        data_ugof <-
          data$X1             # taking start times of elements for onset analysis
        beat_ioi <- results_rhythm[a, 2]
        beat_fft <- results_rhythm[a, 5]
        
        timesteps <- 1000 / round(beat_ioi, digits = 1)
        
        ### phase shift -----
        
        # function find_best_phase_shift.R is sourced in "global.R"
        
        beat_period <- timesteps / 1000  # in seconds
        #step_size <- input$step_size     # from UI
        step_size <- 0.01
        
        best_phase_shift <-
          find_best_phase_shift(data_ugof, beat_period, step_size)
        rmsd_data <- best_phase_shift$rmsd_data
        rmsd_data$file <- list_of_files[a]
        
        all_rmsd_data[[a]] <<- rmsd_data
        
        theotime_seq <- best_phase_shift$theotime_seq
        output$phasePlot <-
          renderPlot({
            best_phase_shift$rmsd_plot
          })
        
        
        ### plot phase shift contrours----
        
        combined_data <<- bind_rows(all_rmsd_data)
        
        phase_plot_all <-
          ggplot(combined_data, aes(
            x = phase_shift,
            y = rmsd,
            color = file
          )) +
          geom_line(linewidth = 0.5) +
          labs(title = "Phase Shift Optimization for All Files",
               x = "Phase Shift (s)",
               y = "RMS Deviation") +
          theme_minimal()
        
        phase_plot_all <- ggplotly(phase_plot_all)
        
        output$phasePlot <-
          renderPlotly({
            phase_plot_all
          })
        
        ### raw beat precision values ioi-----
        
        for (n in 1:length(data_ugof)) {
          # Calculate max possible deviation for normalization
          maxdev <- timesteps / 2 / 1000
          
          # Raw differences to all theoretical time points
          raw_differences <- data_ugof[n] - theotime_seq[, 1]
          
          # Find index of closest theoretical point
          closest_idx <- which.min(abs(raw_differences))
          
          # Extract closest theoretical time and corresponding raw/abs deviation
          matched_theo <- theotime_seq[closest_idx, 1]
          raw_dev <- raw_differences[closest_idx]
          abs_dev <- abs(raw_dev)
          ugof <- abs_dev / maxdev  # normalized deviation
          
          # Append one row to output dataframe (enforce scalar entries)
          raw_deviations_bp <<- rbind(
            raw_deviations_bp,
            data.frame(
              filename = as.character(list_of_files[a]),
              obs_number = n,
              obs_value = data_ugof[n],
              matched_theo = matched_theo,
              raw_deviation = raw_dev,
              abs_deviation = abs_dev,
              best_shift = as.numeric(best_phase_shift$best_shift),
              ugof_value_beat = ugof,
              stringsAsFactors = FALSE
            )
          )
        }
        
        # plot output, histogram around 0 to see deviations
        
        output$rawDeviationHist <- renderPlotly({
          req(raw_deviations_bp)  # ensure the data exists
          
          p <- ggplot(raw_deviations_bp, aes(x = raw_deviation)) +
            geom_histogram(
              binwidth = 0.01,
              fill = "steelblue",
              color = "white"
            ) +
            geom_vline(
              xintercept = 0,
              color = "red",
              linetype = "dashed",
              linewidth = 1
            ) +
            labs(title = "Histogram of Raw Deviations",
                 x = "Deviation (s)",
                 y = "Frequency") +
            theme_minimal()
          
          ggplotly(p)
        })
        
        
        
        ### bp ioi ---------
        
        # data_ugof <- data$X1
        # beat_ioi <- results_rhythm[a,2]
        # beat_fft <- results_rhythm[a,5]
        #
        # # calculate goodness-of-fit for IOI analysis and Fourier analysis
        #
        # maxoriginal <- max(data_ugof)
        # timesteps <- 1000/round(beat_ioi, digits = 1)
        # count <- 0
        # theotime_value <- 0
        # theotime_seq <- data.frame()
        #
        # while (theotime_value < maxoriginal){
        #
        #   count <- count + 1;
        #   theotime_value <- count * timesteps /1000;
        #   theotime_seq[count,1] <- theotime_value;
        #
        # }
        #
        # # match original sequence to theoretical timeseries and calculate actual deviation
        # x <- length(data_ugof)
        # min_value <- c(1:x)
        # ugof_value_beat <- c()
        #
        # for (n in 1:x){
        #
        #   min_value[n] <- min(abs(data_ugof[n]- theotime_seq))
        #
        # }
        #
        # # calculate uGof
        #
        # maxdev <- timesteps/2/1000;
        #
        # ugof_value_beat <- min_value/maxdev;
        
        #m_ugof_beat_1 <- median(ugof_value_beat[2:length(ugof_value_beat)])
        
        m_ugof_beat_1 <- median(raw_deviations_bp$ugof_value_beat)
        
        
        ### npvi of ugof values from ioi beat
        
        
        ugof_value_beat <- raw_deviations_bp$ugof_value_beat
        z <- c()
        b <- c()
        
        for (l in 1:length(ugof_value_beat)) {
          #z[l] <- (ugof_value_beat[l,1] - ugof_value_beat[l+1,1])
          #b[l] <- (ugof_value_beat[l,1] + ugof_value_beat[l+1,1])/2
          
          z[l] <- (ugof_value_beat[l] - ugof_value_beat[l + 1])
          b[l] <- (ugof_value_beat[l] + ugof_value_beat[l + 1]) / 2
        } #end of for loop
        
        z <- na.omit(z)
        b <- na.omit(b)
        c <- sum(abs(z / b))
        
        npvi_ugof_ioi <- c * (100 / (length(z) - 1))
        
        
        ### cv of ugof values
        
        ugof_ioi_cv <-
          sd(ugof_value_beat, na.rm = TRUE) / mean(ugof_value_beat, na.rm = TRUE)
        ugof_ioi_cv_unbiased <-
          (1 + 1 / (4 * (length(
            ugof_value_beat
          ) - 1))) * ugof_ioi_cv
        
        ### cv of duration of elements (if applicable, then to be extracted from column 4 of input data and to be written to column 19 of results later)
        
        # if(ncol(data) == 4){
        #   duration_cv <- sd(data$X4, na.rm = TRUE)/mean(data$X4, na.rm = TRUE)
        #   duration_cv_unbiased <-  (1+1/(4*(length(nrow(data))-1)))*duration_cv
        # }
        
        
        
        ### end recurrence ugof
        results_rhythm[a, 9] <<- m_ugof_beat_1
        results_rhythm[a, 10] <<-
          pmin(m_ugof_beat_1, 1 - m_ugof_beat_1)
        results_rhythm[a, 11] <<- mean(min_value, na.rm = TRUE)
        silent_beat_ioi <- nrow(theotime_seq) - kk
        results_rhythm[a, 12] <<- silent_beat_ioi
        
        ### ugof Fourier --------------
        
        if (is.na(beat_fft) == FALSE) {
          timesteps_2 <- 1000 / round(beat_fft, digits = 1)
          count_2 <- 0
          theotime_value_2 <- 0
          theotime_seq_2 <- data.frame()
          
          while (theotime_value_2 < maxoriginal) {
            count_2 <- count_2 + 1
            
            theotime_value_2 <- count_2 * timesteps_2 / 1000
            
            theotime_seq_2[count_2, 1] <- theotime_value_2
            
          }
          
          # match original sequence to theoretical timeseries and calculate actual deviation
          
          x_2 <- length(data_ugof)
          min_value_2 <- c(1:x)
          ugof_value_beat_2 <- c()
          
          for (n in 1:x) {
            min_value_2[n] <- min(abs(data_ugof[n] - theotime_seq_2))
          }
          
          # calculate ugof for Fourier beat
          
          maxdev_2 <- timesteps_2 / 2 / 1000
          
          
          ugof_value_beat_2 <- min_value_2 / maxdev_2
          
          
          m_ugof_beat_2 <-
            median(ugof_value_beat_2[2:length(ugof_value_beat_2)])
          
          
          ### calculate npvi of ugof values of fourier beat
          z <- c()
          b <- c()
          
          for (l in 1:length(ugof_value_beat_2)) {
            z[l] <- (ugof_value_beat_2[l] - ugof_value_beat_2[l + 1])
            b[l] <-
              (ugof_value_beat_2[l] + ugof_value_beat_2[l + 1]) / 2
            
          } #end of for loop
          
          z <- na.omit(z)
          b <- na.omit(b)
          c <- sum(abs(z / b))
          
          npvi_ugof_fft <- c * (100 / (length(z) - 1))
          ### calculate cv of ugof values of fourier beat
          
          ugof_fft_cv <-
            sd(ugof_value_beat, na.rm = TRUE) / mean(ugof_value_beat_2, na.rm = TRUE)
          ugof_fft_cv_unbiased <-
            (1 + 1 / (4 * (length(
              ugof_value_beat_2
            ) - 1))) * ugof_fft_cv
          
          ### recurrence ugof fft -------------
          
          output$rec_ugof_fft_plots <- renderUI({
            plot_output_list <- lapply(1:length(list_of_files), function(i) {
              plotname <- paste("ugof_fft_plot", i, sep = "")
              plotlyOutput(plotname, height = 300, width = 300)
            }) # end lapply
            
            do.call(tagList, plot_output_list)
          })
          
          local({
            eucl_dist_ugof <-
              (as.matrix(
                vegdist(ugof_value_beat_2, "euclidean", na.rm = TRUE)
              ))
            eucl_dist_ugof <-
              eucl_dist_ugof[1:(nrow(eucl_dist_ugof) - 1), 1:(nrow(eucl_dist_ugof) -
                                                                1)]
            
            threshold <-
              mean(ugof_value_beat, na.rm = TRUE) * 0.1 # as input?
            
            eucl_dist_ugof[eucl_dist_ugof < threshold] <- 0
            
            # transform matrix as to be able to plot it with ggplot as tile plot
            #https://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2
            
            levels <- 1:(nrow(eucl_dist_ugof))
            
            eucl_dist_ugof_2 <- eucl_dist_ugof %>%
              tibble::as_tibble() %>%
              rownames_to_column('Var1') %>%
              gather(Var2, value,-Var1) %>%
              mutate(
                Var1 = factor(Var1, levels = levels),
                Var2 = factor(gsub("V", "", Var2), levels = levels)
              )
            
            my_i <- a
            plotname <- paste("ugof_fft_plot", my_i, sep = "")
            
            output[[plotname]] <- renderPlotly({
              rec_plot_ugof <- ggplot(eucl_dist_ugof_2, aes(Var1, Var2)) +
                geom_tile(aes(fill = value)) +
                # geom_text(aes(label = round(value, 1))) +
                scale_fill_gradient(low = "white", high = "black") +
                xlab("#ugof") +
                ylab("#ugof") +
                coord_fixed(ratio = 1) +
                ggtitle(paste("ugof fft,File:", results_rhythm$filename[my_i])) +
                theme_minimal() +
                theme(
                  plot.background = element_rect(fill = "white"),
                  panel.grid = element_blank(),
                  title = element_text(size = 6)
                )
              
              rec_plot_ugof <- ggplotly(rec_plot_ugof)
              
              rec_plot_ugof
              
            }) # end renderPlotly
          }) # end local
          ### end recurrence plot fft ugof
          
          
          results_rhythm[a, 13] <<- m_ugof_beat_2
          results_rhythm[a, 14] <<-
            mean(min_value_2, na.rm = TRUE)
          silent_beat_fft <- nrow(theotime_seq_2) - kk
          
          results_rhythm[a, 15] <<- silent_beat_fft
        } else {
          results_rhythm[a, 13] <<- NA
          results_rhythm[a, 14] <<- NA
          results_rhythm[a, 15] <<- NA
          
        }
        
        results_rhythm[a, 16] <<- npvi_ugof_ioi
        results_rhythm[a, 17] <<- ugof_ioi_cv_unbiased
        results_rhythm[a, 18] <<- npvi_ugof_fft
        results_rhythm[a, 19] <<- ugof_fft_cv_unbiased
        
        # if(ncol(data) == 4){
        #   results_rhythm[a,19] <<- duration_cv_unbiased
        # }
        
        ## end ugof
        
        ## recurrence plot -------
        
        
        ## loop version for recurrence plots
        #https://stackoverflow.com/questions/22840892/r-shiny-loop-to-display-multiple-plots
        
        output$plots <- renderUI({
          plot_output_list <- lapply(1:length(list_of_files), function(i) {
            plotname <- paste("plot", i, sep = "")
            plotlyOutput(plotname, height = 300, width = 300)
          }) # end lapply
          
          do.call(tagList, plot_output_list)
        })
        
        local({
          ioi_seq <-
            data.frame()                 # set up empty dataframe to store ioi values in
          
          
          for (x in  1:nrow(data)) {
            # start of loop through rows of data to calculate iois
            
            z = x + 1
            ioi_seq[x, 1] <- data[z, 1] - data[x, 1]
            
          }
          
          ##  recurrence matrix
          
          # euclidian distance matrix
          
          eucl_dist <-
            (as.matrix(vegdist(ioi_seq, "euclidean", na.rm = TRUE)))
          eucl_dist <-
            eucl_dist[1:(nrow(eucl_dist) - 1), 1:(nrow(eucl_dist) - 1)]
          
          threshold <-
            mean(ioi_seq$X1, na.rm = TRUE) * 0.1 # as input?
          
          eucl_dist[eucl_dist < threshold] <- 0
          
          # transform matrix as to be able to plot it with ggplot as tile plot
          #https://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2
          
          levels <- 1:(nrow(eucl_dist))
          
          eucl_dist_2 <- eucl_dist %>%
            tibble::as_tibble() %>%
            rownames_to_column('Var1') %>%
            gather(Var2, value,-Var1) %>%
            mutate(
              Var1 = factor(Var1, levels = levels),
              Var2 = factor(gsub("V", "", Var2), levels = levels)
            )
          
          ## recurrence plot
          
          my_i <- a
          plotname <- paste("plot", my_i, sep = "")
          
          output[[plotname]] <- renderPlotly({
            rec_plot <- ggplot(eucl_dist_2, aes(Var1, Var2)) +
              geom_tile(aes(fill = value)) +
              # geom_text(aes(label = round(value, 1))) +
              scale_fill_gradient(low = "white", high = "black") +
              xlab("#IOI") +
              ylab("#IOI") +
              coord_fixed(ratio = 1) +
              ggtitle(paste("IOI,File:", results_rhythm$filename[my_i])) +
              theme_minimal() +
              theme(
                plot.background = element_rect(fill = "white"),
                panel.grid = element_blank(),
                title = element_text(size = 6)
              )
            
            rec_plot <- ggplotly(rec_plot)
            
            rec_plot
            
          }) # end renderPlotly
          
        }) # end local
        
        ## end if recurrence plot
        
      
      
    } # end if input$goButton_2
      
      #} #end of input$all == TRUE
      
      
      # 04 c: phase shift plot -----
      
      
      
      # 04c: Standard results details ----------
      
      filenames <- as.data.frame(list_of_files)
      
      elements <- str_c(elementlist, collapse = "")
      elements_raw <- elements_seq
      
      # if("X3" %in% colnames(data) == FALSE) {
      # 
      # elements <- "all"
      # elements_raw <- NA
      # 
      # } else if ("X3" %in% colnames(data) == TRUE) {
      # 
      # elements_raw <- elements_seq} else {
      # 
      # elements <- str_c(elementlist, collapse = " ")
      # elements_raw <- elements_seq
      # 
      # }
      
      fs <- input$fs
      savename <- input$savename
      averaging <- input$method
      
      
      results_rhythm <<-
        cbind(results_rhythm,
              fs,
              averaging,
              elements,
              elements_raw,
              filenames,
              savename)
      
      #if (nrow(data) == 3){
      
      colnames(results_rhythm) <<-
        c(
          "index",
          "ioi_beat",
          "unbiased_cv",
          "npvi",
          "fourier_beat",
          "freq_reso",
          "n_elements",
          "signal_length",
          "ugof_ioi",
          "ugof_sym_ioi",
          "mean_min_dev_ioi",
          "silent_beats_ioi",
          "ugof_fft",
          "mean_min_dev_fft",
          "silent_beats_fft",
          "npvi_ugof_ioi",
          "cv_ugof_ioi",
          "npvi_ugof_fft",
          "cv_ugof_fft",
          "fs",
          "averaging",
          "elements",
          "raw_element_seq",
          "filename",
          "savename"
        )
      #}
    } #end for loop through list_of_files
          
    })  #end of observer results
  
  
  output$table_ioi <- renderTable({
    results_rhythm
    
  })
  
  output$calc <- renderText({
    paste("Once the results are done, they will appeare here. The results are:")
  })
  
  #04d: Reset Button -------------
  
  observeEvent(input$resetAll, {
    reset("fs")
    reset("savename")
    reset("goButton_1")
    reset("table_ioi")
    
    rm(results_rhythm)
    
    output$table_ioi <- renderTable({
      results_rhythm
      
    }) #end renderTable
  }) # end resetAll
  
  
 # }) #end observeEvent reset



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

output$downloadData_ioi <- downloadHandler(
  filename = function() {
    paste0("iois_", input$savename, ".csv")
  },
  content = function(file) {
    write.csv(ioi_all, file, row.names = FALSE)
  }
)

## integer ratios ----

output$downloadData_ir <- downloadHandler(
  filename = function(){
    paste("integer_ratios_raw", input$savename,".csv", sep = "")
  },
  content = function(file){
    write.csv(ir_all, file, row.names = FALSE)
  }
)

## raw deviations - beat precision -----

output$downloadData_rawdev <- downloadHandler(
  filename = function() {
    paste0("raw_deviations_", input$savename, ".csv")
  },
  content = function(file) {
    write.csv(raw_deviations_bp, file, row.names = FALSE)
  }
)



# Help Tab ----

output$info <- renderUI({
  HTML(paste(
    'For more information, see <a href="manual.pdf" target="_blank">this PDF Manual</a>.',
    "<b>Important Remarks:</b>",
    "1) The input range for the phase shift is 0.001 seconds to 10 seconds.",
    "2) Once the calculations are finished, go to any of the other tabs ('Simple Parameters' or 'Rhythm Analysis') to see the results and to be able to download the results tables.",
    "3) The plots (all but recurrence plots) are saved in the plot folder automatically.",
    sep = "<br/>"
  ))
})


} #end server function
