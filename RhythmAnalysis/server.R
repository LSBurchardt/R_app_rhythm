# Preamble -----
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# With this app you can analyse the temporal structure or rhythm of a time series
# i.e. animal vocalizations, human speech, movement patterns, heart beats
#
# author: Dr. Lara S. Burchardt
# github: LSBurchardt//R_app_rhythm
#
# Rhythm analysis is performed using ioi analysis
# See for example: Burchardt & Knörnschild, 2020, Comparison of methods for rhythm analysis of complex animals’ acoustic signals
# https://doi.org/10.1371/journal.pcbi.1007755
#
# Furthermore different variability parameters and beat precision (universal goodness-of-fit) value is calculated
# See: Burchardt, Briefer, Knörnschild, 2021, Novel ideas to further expand the applicability of rhythm analysis
# https://doi.org/10.1002/ece3.8417

#server.R

# 00: load packages ---------

#defined in global.R

# 01: load functions ---------

#defined in global.R

# Server function ---------------------------------------------------------

server <- function(input, output) {
  # 02: preparations ----
  ## 02a: author  ----
  
  output$authors <- renderText({
    paste("written by Dr. Lara S. Burchardt")
  })
  
  ## 02b: plot folder for saving ----
  
  observe({
    req(input$savename)  # Ensure save name is available
    # Define and create the save path
    save_path <- file.path("plots_app", input$savename)
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
  })
  
  ## 02c: choose analysis folder -----
  
  # all csv files from the chosen folder are taken into account
  
  roots <-
    c(appdir = normalizePath(getwd()))  # start in app directory
  
  shinyDirChoose(input, "dir", roots = roots)  #platform independent folder choosing
  
  dir_path <- reactive({
    req(input$dir)
    normalizePath(parseDirPath(roots, input$dir), winslash = "/")
  })
  
  # write list of files output to access all csv files in the chosen directory throughout the app
  list_of_files <- reactive({
    path <- dir_path()
    if (dir.exists(path)) {
      list.files(
        path = path,
        pattern = "\\.csv$",
        # match files ending in .csv
        ignore.case = TRUE,
        # match .csv or .CSV
        full.names = FALSE         # show just file names
      )
    } else {
      character(0)
    }
  })
  
  output$list_files <- renderTable({
    files <- list_of_files()
    if (length(files) == 0) {
      return(data.frame(file = "No CSV files found", index = NA))
    }
    data.frame(file = files, index = seq_along(files))
  })
  
  ## 02d: Check File Read -----
  # does the read in work correctly? where the right settings chosen (seperator, column names)?
  # this reads in the first few lines of the first file in list of files
  
  observeEvent(input$check_button, {
    req(input$savename)  # Make sure a save name is provided
    req(length(list_of_files()) > 0)
    req(input$sep_input)
    
    file_path <- file.path(dir_path(), list_of_files()[[1]])
    sep <- input$sep_input
    
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
  
  ## 2e: additional elements ----
  # 10 elements (a-j) are always presented, if more are needed, 14 extra elements (k-z) can be chosen
  
  observeEvent(input$toggle_extra_elements, {
    toggle("extra_element_ui")
  })
  
  output$element_checkboxes <- renderUI({
    checkboxGroupInput(
      "element_extra_list",
      label = "Select additional elements:",
      choices = letters[11:26],
      # k–z
      selected = NULL
    )
  })
  
  # 03: Calculations ---------------
  # rhythm analysis begins here
  # different parameters are calculated and results saved to be plotted and downloadable
  
  results <- observeEvent(input$goButton, {
    results_rhythm <<-
      data.frame()      # <<- global assignment operator, needs to be used when changing as well
    ioi_all <<- list(NA)
    ir_all <<- list(NA)
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
    
    for (a in 1:length(list_of_files())) {
      print(a)    # as a checkpoint, if software crashes because of a faulty file
      
      ## 3a: load data ----
      
      if (input$colnames_present == "TRUE") {
        colnames <- TRUE
      } else {
        colnames <- FALSE
      }
      
      req(input$sep_input)
      sepa <- input$sep_input
      
      data <- readr::read_delim(
        file.path(dir_path(), list_of_files()[a]),
        delim  = sepa,
        col_names = colnames
      )
      
      colnames(data) <- c("X1", "X2", "X3")
      data <- data %>% select(X1, X2, X3)
      
      # if file is too short, i.e. only 1 or 2 elements,the loop is skipped and results for this file saved as NA
      if (nrow(data) <= 2) {
        results_rhythm[a, 1] <<- a
        results_rhythm[a, 2:14] <<- NA
        next
      }
      
      # make sure, every sequence, independent of the actual start point, gets a start point of 0.1 seconds
      # this is to decrease calculation times, as some calculations like theo_time start at 0
      
      data[, 1] <- (data[, 1] + 0.1) - as.numeric(data[1, 1])
      
      output$table_input_data <- renderTable({
        data
      }) #end renderText
      
      ## 3b: subset for element types ----
      
      if ("X3" %in% colnames(data)) {
        # Collect fixed checkboxes (a–j)
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
        
        # Combine with dynamically selected extras (k–z)
        if (!is.null(input$element_extra_list)) {
          elementlist <- c(elementlist, input$element_extra_list)
        }
        
        # Save unfiltered full element sequence
        elements_seq[a] <<- str_c(data$X3, collapse = "")
        
        # Filter data for selected element types
        data <- data %>% dplyr::filter(X3 %in% elementlist)
      } else {
        # if no X3 with element types provided --> NA
        elements_seq[a] <<- NA_character_
      }

      
      ## 03c: ioi and cv calculations ----------
      
      ioi <-
        data.frame()                 # set up empty dataframe to store ioi values in, per loop 
      
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
      
      for (x in 1:nrow(ioi)) {
        ioi[x, 2] <- list_of_files()[a]  # filename
        ioi[x, 3] <- ioi_beat    # frequency in Hz
      }
      
      colnames(ioi) <-
        c("ioi",
          "filename",
          "reference_beat")
      
      ioi_all[[a]] <<- ioi
      
      ## 03d:  integer ratio calculations ----
      # based on ioi, which is calculated per sequence
      
      ioi_vec <- ioi$ioi  
      
      integer_r <- compute_integer_ratios(
        ioi_vec = ioi_vec,
        method = input$ratio_method,
        n_pairs = input$n_random_pairs,
        seed = input$seed
      )
      
      integer_r$file <-
        list_of_files()[a]  # Add file name to integer ratios
      ir_all[[a]] <<-
        integer_r           # combining all integer ratios, calculated per file in a list
      

      ## 03d: npvi calculations (ioi_seq) -----------
      
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
      
      ## end npvi
      
      ## 03e: phase shift -------------
      # finidng the phase shift of the theoretical beat sequences, with the least deviations of theoretical from actual element onsets
      # function find_best_phase_shift.R is sourced in "global.R"
      
      # removing relevant data from previous loops, to avoid errors
      
      rm(maxoriginal, timesteps, theotime_value, theotime_seq)
      data_bp <-
        data$X1             # taking start times of elements for onset analysis
      
      # timesteps for theoretical time sequence based on ioi_beat 
      timesteps <- 1000 / round(ioi_beat, digits = 2)
      
      beat_period <- timesteps / 1000  # in seconds
      step_size <- input$step_size     # from UI
      
      best_phase_shift <-
        find_best_phase_shift(data_bp, beat_period, step_size)
      rmsd_data <- best_phase_shift$rmsd_data
      rmsd_data$file <- list_of_files()[a]
      
      all_rmsd_data[[a]] <<- rmsd_data
      
      theotime_seq <- best_phase_shift$theotime_seq
      
      ## 03f: raw deviations ----
      
      for (n in 1:length(data_bp)) {
        # Calculate max possible deviation for normalization
        maxdev <- timesteps / 2 / 1000
        
        # Raw differences to all theoretical time points
        raw_differences <- data_bp[n] - theotime_seq[, 1]
        
        # Find index of closest theoretical point
        closest_idx <- which.min(abs(raw_differences))
        
        # Extract closest theoretical time and corresponding raw/abs deviation
        matched_theo <- theotime_seq[closest_idx, 1]
        raw_dev <- raw_differences[closest_idx]
        abs_dev <- abs(raw_dev)
        ugof <- abs_dev / maxdev  # normalized deviation
        
        # Append one row to output dataframe 
        raw_deviations_bp <<- rbind(
          raw_deviations_bp,
          data.frame(
            filename = as.character(list_of_files()[a]),
            obs_number = n,
            obs_value = data_bp[n],
            matched_theo = matched_theo,
            raw_deviation = raw_dev,
            abs_deviation = abs_dev,
            best_shift = as.numeric(best_phase_shift$best_shift),
            ugof_value_beat = ugof,
            stringsAsFactors = FALSE
          )
        )
      }
      
      ## 3g: beat precision -----
      
      ### sequence summary ----
      # beat precision median across a sequence based on raw beat precision values
      # calculated after phase shift
      
      m_ugof_beat <- median(raw_deviations_bp$ugof_value_beat)
      
      
      ### npvi of bp ----
      
      # how different are the bps in a sequence? we calcualte the npvi over the sequence of 
      # individual beat precisio values
      
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
      
      
      ### cv of beat precision values -----
      # we also calcualte the coefficient of variation over the sequence of bps 
      # for the same reason, to show overall variability of bps
      
      ugof_ioi_cv <-
        sd(ugof_value_beat, na.rm = TRUE) / mean(ugof_value_beat, na.rm = TRUE)
      ugof_ioi_cv_unbiased <-
        (1 + 1 / (4 * (length(
          ugof_value_beat
        ) - 1))) * ugof_ioi_cv
  
      
# this ends the calculations, now some results are plotted to be direct output in the app
      
      # 04: Plots -----
      
      ## 04a: Scatter Beats ------------
      
      output$plot_beat <- renderPlotly({
        p <- ggplot(data = results_rhythm, ) +
          geom_jitter(
            aes(
              x = "IOI",
              y = ioi_beat,
              color = filename
            ),
            width = 0.1,
            alpha = 0.8
          ) +
          ylab("Beat [Hz]") +
          xlab("Beat Parameter") +
          theme_minimal() +
          ggtitle("Beat Results in Hertz")
        
        #saveRDS(p, file.path(save_path, "plot_ioi_beat.rds"))
        
        p <- ggplotly(p)
        
        p
        
      })
      
      ## 04b: Scatter Beat Precision and CV ----------------
      
      output$plot_ugof <- renderPlotly({
        p <- ggplot(data = results_rhythm) +
          geom_jitter(
            aes(
              x = "Beat Precision IOI",
              y = ugof_ioi,
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
        
        #saveRDS(p,
        #        file.path(save_path, "plot_beat_precision_cv.rds"))
        
        p <- ggplotly(p)
        
        p
        
      })
      
      ## 04c: Scatter NPVI ----------
      
      output$plot_var <- renderPlotly({
        p <- ggplot(data = results_rhythm) +
          geom_jitter(aes(
            x = "npvi",
            y = npvi,
            color = filename
          ),
          width = 0.1,
          alpha = 0.8) +
          ylab("Value") +
          xlab("") +
          theme_minimal() +
          ggtitle("normalized Pairwise Variability Index") +
          ylim(0, max(results_rhythm$npvi))
        
        #saveRDS(p, file.path(save_path, "plot_npvi.rds"))
        
        p <- ggplotly(p)
        
        p
        
      })
      
      ## 04d: Histogram raw IOIs --------------------
      
      output$plot_ioi_all <- renderPlotly({
        # we append all lists in ioi_all, as they are all the same format, for plotting and saving
        ioi_all <<- do.call(rbind, ioi_all)
        # Compute histogram data separately
        hist_data <- ggplot_build(ggplot(ioi_all, aes(x = ioi)) +
                                    geom_histogram(bins = 30, na.rm = TRUE))$data[[1]]
        
        # Compute percentage
        hist_data <- hist_data %>%
          mutate(percentage = round(count / sum(count) * 100, digits = 2))
        
        # Plot with pre-computed values
        p <- ggplot(hist_data, aes(x = xmin, y = percentage)) +
          geom_bar(
            stat = "identity",
            aes(),
            width = diff(hist_data$x)[1],
            # Ensures correct bar width
            color = "white",
            fill = "darkblue"
          ) +
          xlab("IOI [sec]") +
          ylab("Percentage [%]") +
          ggtitle("Histogram of IOIs")+
          theme_minimal()
        
        #saveRDS(p, file.path(save_path, "plot_hist_ioi.rds"))
        
        p <- ggplotly(p)
        
        p
        
      })
      ## 04e: integer ratio plot ------
      
      output$plot_ir_all <- renderPlotly({
        ir_all <<- do.call(rbind, ir_all)
        
        p <- ir_all %>%
          ggplot(aes(x = ratio)) +
          geom_density() +
          geom_density(aes(color = adjacent)) +
          coord_cartesian(xlim = c(0, 1)) +
          theme_minimal() +
          xlab("Integer Ratios") +
          ylab(" Density")+
          ggtitle("Density Plots of Integer Ratios for All Ratios, Adjacent Pairs and Non-adjacent Pairs")
        
        #saveRDS(p, file.path(save_path, "plot_integer_ratio.rds"))
        
        p_plotly <- ggplotly(p)
        
        p_plotly
        
      })
      
      
      ## 04f: phase shift contours----
      
      combined_data <<- bind_rows(all_rmsd_data)

      phase_plot_all <-
        ggplot(combined_data, aes(x = phase_shift,
                                  y = rmsd,
                                  color = file)) +
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
      
      ## 04g: Histogram raw deviations -----
      
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
      
# 05: Recurrence Plots -----
# since extra calculations are necessary for these plots and they are not part
# of the standard result, this gets a new heading 
      
      ## loop version for recurrence plots
      #https://stackoverflow.com/questions/22840892/r-shiny-loop-to-display-multiple-plots
      
      output$plots <- renderUI({
        plot_output_list <- lapply(1:length(list_of_files()), function(i) {
          plotname <- paste("plot", i, sep = "")
          plotlyOutput(plotname, height = 300, width = 300)
        }) # end lapply
        
        do.call(tagList, plot_output_list)
      })
      
      local({
        
        ioi_seq <- data.frame(ioi = ioi$ioi)
        colnames(ioi_seq) <- c("ioi")
        
        # for (x in  1:nrow(data)) {
        #   # start of loop through rows of data to calculate iois
        #   
        #   z = x + 1
        #   ioi_seq[x, 1] <- data[z, 1] - data[x, 1]
        #   
        # }
        
        ##  recurrence matrix
        
        # euclidian distance matrix
        
        eucl_dist <-
          (as.matrix(vegdist(ioi_seq, "euclidean", na.rm = TRUE)))
        eucl_dist <-
          eucl_dist[1:(nrow(eucl_dist) - 1), 1:(nrow(eucl_dist) - 1)]
        
        threshold <-
          #mean(ioi_seq$X1, na.rm = TRUE) * 0.1 # as input?
          mean(ioi_seq$ioi, na.rm = TRUE)* 0.1
        
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
        plotname <- paste("plot", my_i, sep = "")
        
        output[[plotname]] <- renderPlotly({
          rec_plot <- ggplot(eucl_dist_2, aes(Var1, Var2)) +
            geom_tile(aes(fill = value)) +
            # geom_text(aes(label = round(value, 1))) +
            scale_fill_gradient(low = "white", high = "black") +
            xlab("#IOI") +
            ylab("#IOI") +
            coord_fixed(ratio = 1) +
            ggtitle(paste("File:", results_rhythm$filename[my_i])) +
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
      
      ## end of recurrence plot
      
      # 06: Results -------

      ## 06a: collecting all numeric results of loop run a ---------
      
      results_rhythm[a, 1] <<- a
      results_rhythm[a, 2] <<- ioi_beat
      results_rhythm[a, 3] <<- ioi_cv_unbiased
      results_rhythm[a, 4] <<- npvi
      results_rhythm[a, 5] <<- m_ugof_beat
      silent_beat_ioi <- nrow(theotime_seq) - nrow(data)
      results_rhythm[a, 6] <<- silent_beat_ioi
      results_rhythm[a, 7] <<- npvi_ugof_ioi
      results_rhythm[a, 8] <<- ugof_ioi_cv_unbiased
      
    } # end of loop through files
    
    ## 06b: meta data results ----
    
    filenames <- as.data.frame(list_of_files())
    
    elements <- str_c(elementlist, collapse = "")
    elements_raw <- elements_seq
    
    savename <- input$savename
    averaging <- input$method
    
    # combine numeric results with method input, filenames etc.
    results_rhythm <<-
      cbind(results_rhythm,
            #fs,
            averaging,
            elements,
            elements_raw,
            filenames,
            savename)
    
    # write column names to complete results table
    colnames(results_rhythm) <<-
      c(
        "index",
        "ioi_beat",
        "unbiased_cv",
        "npvi",
        "ugof_ioi",
        "silent_beats_ioi",
        "npvi_ugof_ioi",
        "cv_ugof_ioi",
        "averaging",
        "elements",
        "raw_element_seq",
        "filename",
        "savename"
      )
    
  })  #end of observeEvent (Go Button)
  

  ## 06c: render results table -----
  
  output$table_ioi <- renderTable({
    results_rhythm
    
  })
  
  # 07: Downloads --------------
  
  ## 07a: Rhythm Analysis Results----------
  datasetInput <- reactive({
    results_rhythm
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("rhythm_analysis_",
            input$method,
            "_step_size_",
            input$step_size,
            "_",
            input$savename,
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  ## 07b: raw IOIs ----------
  
  output$downloadData_ioi <- downloadHandler(
    filename = function() {
      paste0("iois_", input$savename, ".csv")
    },
    content = function(file) {
      write.csv(ioi_all, file, row.names = FALSE)
    }
  )
  
  ## 07c: integer ratios ----
  
  output$downloadData_ir <- downloadHandler(
    filename = function() {
      paste("integer_ratios_raw", input$savename, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ir_all, file, row.names = FALSE)
    }
  )
  
  ## 07d: raw deviations - beat precision -----
  
  output$downloadData_rawdev <- downloadHandler(
    filename = function() {
      paste0(
        "raw_deviations_",
        input$method,
        "_step_size_",
        input$step_size,
        "_",
        input$savename,
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      write.csv(raw_deviations_bp, file, row.names = FALSE)
    }
  )
  
  # 08: Help Tab ----
  
  output$info <- renderUI({
    HTML(
      paste(
        'For more information, see <a href="manual.pdf" target="_blank">this PDF Manual</a>.',
        "<b>Important Remarks:</b>",
        "1) The input range for the phase shift is 0.001 seconds to 10 seconds.",
        "2) Once the calculations are finished, go to any of the other tabs ('Simple Parameters' or 'Rhythm Analysis') to see the results and to be able to download the results tables.",
        "3) The plots (all but recurrence plots) are saved in the plot folder automatically.",
        sep = "<br/>"
      )
    )
  })
  
} #end server function
