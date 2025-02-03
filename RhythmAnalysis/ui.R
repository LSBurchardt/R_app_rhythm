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
           titlePanel(title="RANTO - Rhythm ANalysis TOol for Timeseries" 
           ),
           offset = 3),
    fluidRow( 
      column(2, 
             # Sidebar with input for analysis options and sampling rate for fourier analysis
             
             #checkboxInput("all", "Run all analysis.", value = TRUE),
             
             #checkboxInput("rec_plot", "Produce Recurrence Plots", value = TRUE),
             
             numericInput("fs", "Sampling Rate (FS) for Fourier Analysis",
                          min= 10, max= 1000, value=200),
             
             selectInput("method", "Which method to use for ioi beat:",
                         choices = c("mean", "median")),
         
             selectInput("fileextension", "Choose file extension of input data:",
                         choices = c("csv", "xls", "xlsx")),
             
             selectInput("colnames_present", "Datasheets have column names:",
                         choices = c("FALSE","TRUE")),
             
             actionButton("goButton_1","Choose input folder"),
             
             img(src="blank_space.png", width = "100%"),
             
             textInput("savename", "ID for saving (i.e. test_species)", value = ""),
             
             actionButton("goButton_2","GO"),

             img(src="blank_space.png", width = "100%"),
             
             actionButton("resetAll", "Reset Results Table"),
             
             img(src="blank_space.png", width = "100%"),
             
             downloadButton("downloadData", "Download Results"),
             
             img(src="blank_space.png", width = "100%"),
             
             downloadButton("downloadData_ioi", "Download combined raw IOIs"),
             
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
                                  checkboxInput("element_a", "Use element a.", value = TRUE),
                                  checkboxInput("element_b", "Use element b.", value = TRUE),
                                  checkboxInput("element_c", "Use element c.", value = TRUE),
                                  checkboxInput("element_d", "Use element d.", value = TRUE),
                                  checkboxInput("element_e", "Use element e.", value = TRUE),
                                  checkboxInput("element_f", "Use element f.", value = TRUE),
                                  checkboxInput("element_g", "Use element g.", value = TRUE),
                                  checkboxInput("element_h", "Use element h.", value = TRUE),
                                  checkboxInput("element_i", "Use element i.", value = TRUE),
                                  checkboxInput("element_j", "Use element j.", value = TRUE),
                                  checkboxInput("element_k", "Use element k.", value = TRUE),
                                  checkboxInput("element_l", "Use element l.", value = TRUE),
                                  checkboxInput("element_m", "Use element m.", value = TRUE),
                                  checkboxInput("element_n", "Use element n.", value = TRUE),
                                  checkboxInput("element_n", "Use element o.", value = TRUE),
                                  checkboxInput("element_n", "Use element p.", value = TRUE),
                                  checkboxInput("element_n", "Use element q.", value = TRUE),
                                  checkboxInput("element_n", "Use element r.", value = TRUE),
                                  checkboxInput("element_n", "Use element s.", value = TRUE),
                                  checkboxInput("element_n", "Use element t.", value = TRUE),
                                  checkboxInput("element_n", "Use element u.", value = TRUE),
                                  checkboxInput("element_n", "Use element v.", value = TRUE),
                                  checkboxInput("element_n", "Use element w.", value = TRUE),
                                  checkboxInput("element_n", "Use element x.", value = TRUE),
                                  checkboxInput("element_n", "Use element y.", value = TRUE),
                                  checkboxInput("element_n", "Use element z.", value = TRUE),
                                  textOutput("element"),
                                  tableOutput("elementlist")
                                  )
                         ),
                         tabPanel("Results", 
                                  
                                  textOutput("calc"),
                                  tableOutput("table_ioi"),
                                  plotlyOutput("plot_beat"),
                                  plotlyOutput("plot_ugof"),
                                  plotlyOutput("plot_var"),
                                  plotlyOutput("plot_ioi_all"),
                                  plotOutput("plot_rose")
                                  ##
                         ),
                         tabPanel("Recurrence Plots",
                                   
                                  column(4,
                                  uiOutput("plots")
                                  ),
                                  #column(4,
                                  #uiOutput("rec_ugof_plots")
                                  #),
                                  #column(4,
                                  #uiOutput("rec_ugof_fft_plots"))
                                  ##
                                  ),
                         # tabPanel("Re-run analysis on Section",
                         #          numericInput("file_nr", "File Nr. to rerun analysis",
                         #                       min= 1, max= 1000, value=1),
                         #          numericInput("start", "Start IOI for analysis",
                         #                       min= 1, max= 1000, value=1),
                         #          numericInput("end", "End IOI for analysis",
                         #                       min= 1, max= 1000, value=50),
                         #          actionButton("rerun_ioi", "Rerun on Section"),
                         #          
                         #          img(src="blank_space.png", width = "100%"),
                         #          
                         #          tableOutput("table_rerun"),
                         #          
                         #          downloadButton("download_rerun_Data", "Download Rerun Results")
                         #         ),
                         # tabPanel("Beat precision details",
                         #          
                         #          uiOutput("warning_ugof_detail"),
                         #        
                         #          actionButton("ugof_detail", "Calculate now"),
                         #          
                         #          plotlyOutput("maxdev100"),
                         #          
                         #          plotlyOutput("ugof_hist"),
                         #          
                         #          plotlyOutput("ugof_zscore")
                         #          ##
                         # ),
                         #tabPanel("Help",
                                  ##
                                  #)
                        ),
            
               )
          )
     )
)


# 04: Server function --------------------------------------------------------------

#defined in server.R