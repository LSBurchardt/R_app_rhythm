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

#ui.R

# 00: load packages -- ---------------------------------------------------------

#defined in global.R

# 02: global variables and dataframes

#defined in "global.R"

# 03: define UI --------------------------------------------------------------
ui <- fluidPage(
  fluidRow(
    column(9,
           # Application title
           titlePanel(title="Rhythm Analysis for Timeseries" 
           ),
           offset = 3),
    fluidRow( 
      column(2, 
             # Sidebar with input for analysis options and sampling rate for fourier analysis
             
             checkboxInput("all", "Run all analysis.", value = TRUE),
             
             checkboxInput("rec_plot", "Produce Recurrence Plots", value = TRUE),
             
             numericInput("fs", "Sampling Rate (FS) for Fourier Analysis",
                          min= 10, max= 1000, value=200),
             
             selectInput("method", "Which method to use for ioi beat:",
                         choices = c("mean", "median")),
         
             selectInput("fileextension", "Choose file extension of input data:",
                         choices = c("csv", "xls", "xlsx")),

             actionButton("goButton_1","Choose input folder"),
             
             img(src="blank_space.png", width = "100%"),
             
             textInput("savename", "ID for saving (i.e. test_species)", value = ""),
             
             actionButton("goButton_2","GO"),

             img(src="blank_space.png", width = "100%"),
             
             actionButton("resetAll", "Reset Results Table"),
             
             img(src="blank_space.png", width = "100%"),
             
             downloadButton("downloadData", "Download Results"),
             
             #downloadButton("downloadPlot", "Download Recurrence Plots"),
             
             #tags$iframe(style="height:4px; width:1%; scrolling=yes", 
            #           src="manual.pdf"),
             offset = 1
             ),
      # Show plots of the data
      column(9, 
             #add_busy_bar(color = "red", height = "8px"),
             add_busy_spinner(spin = "fading-circle"),
             textOutput("authors"),  #added to mention authors
             
             tabsetPanel(type = "tabs",
                         tabPanel("Data",
                                  column(8,
                                  textOutput("files"),
                                  textOutput("files_out"),
                                  tableOutput("list_files"),
                                  #tableOutput("table_input_data"),
                                  textOutput("loop_a")
                                  ),
                                  column(4,
                                  #checkboxInput("element_all", "Use all elements.", value = TRUE),
                                  checkboxInput("element_a", "Use element a.", value = TRUE),
                                  checkboxInput("element_b", "Use element b.", value = TRUE),
                                  checkboxInput("element_c", "Use element c.", value = TRUE),
                                  checkboxInput("element_d", "Use element d.", value = TRUE),
                                  checkboxInput("element_e", "Use element e.", value = TRUE),
                                  checkboxInput("element_f", "Use element f.", value = TRUE),
                                  textOutput("element"),
                                  tableOutput("elementlist")
                                  )
                                  #plotlyOutput("plot_timeseries", inline = TRUE)
                         ),
                         tabPanel("Results", 
                                  
                                  textOutput("calc"),
                                  tableOutput("table_ioi"),
                                  plotlyOutput("plot_beat"),
                                  plotlyOutput("plot_ugof"),
                                  plotlyOutput("plot_var"),
                                  plotlyOutput("plot_ioi_all")
                                  
                                  #textOutput("data_test")
                                  ##
                         ),
                         tabPanel("Recurrence Plots",
                                   
                                  column(4,
                                  uiOutput("plots")
                                  ),
                                  column(4,
                                  uiOutput("rec_ugof_plots")
                                  ),
                                  column(4,
                                  uiOutput("rec_ugof_fft_plots"))
                                  ##
                                  ),
                         tabPanel("Re-run analysis on Section",
                                  numericInput("file_nr", "File Nr. to rerun analysis",
                                               min= 1, max= 1000, value=1),
                                  numericInput("start", "Start IOI for analysis",
                                               min= 1, max= 1000, value=1),
                                  numericInput("end", "End IOI for analysis",
                                               min= 1, max= 1000, value=50),
                                  actionButton("rerun_ioi", "Rerun on Section"),
                                  
                                  img(src="blank_space.png", width = "100%"),
                                  
                                  tableOutput("table_rerun"),
                                  
                                  downloadButton("download_rerun_Data", "Download Rerun Results")
                                 ),
                         tabPanel("Beat precision details",
                                  
                                  textOutput("warning_ugof_detail"),
                                
                                  actionButton("ugof_detail", "Calculate now"),
                                  
                                  plotlyOutput("maxdev100"),
                                  
                                  plotlyOutput("ugof_hist")
                                  ##
                         ),
                         tabPanel("Help",
                                  ##
                                  )
                        ),
            
               )
          )
     )
)


# 04: Server function --------------------------------------------------------------

#defined in server.R