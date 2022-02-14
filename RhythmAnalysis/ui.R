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
      column(3, 
             # Sidebar with input for analysis options and sampling rate for fourier analysis
             
             checkboxInput("all", "Run all analysis.", value = TRUE),
             checkboxInput("rec_plot", "Produce Recurrence Plots", value = TRUE),
             numericInput("fs", "Sampling Rate for Fourier Analysis",
                          min= 10, max= 1000, value=20),
             img(src="blank_space.png", width = "100%"),
             
             selectInput("fileextension", "Choose file extension of input data:",
                         choices = c("csv", "xls")),
             # selectInput("fileextension_output", "Choose file extension for output data:",
             #             choices = c("csv", "xlsx")),
             
             textInput("savename", "ID for saving (i.e. test_species)", value = ""),
             
             img(src="blank_space.png", width = "100%"),
             
             #actionButton("dir", "Choose Directory"),
             actionButton("goButton_1","Choose folder"),
            
             #textOutput("session"),
             #shinyDirButton("dir", label = "Chose directory", title = "Chose Directory", multiple = TRUE),
            
             img(src="blank_space.png", width = "100%"),
             
             actionButton("goButton_2","GO"),
             #submitButton("Run Analysis"),
             
             img(src="blank_space.png", width = "100%"),
             
             actionButton("resetAll", "Reset all"),
             
             img(src="blank_space.png", width = "100%"),
             
             downloadButton("downloadData", "Download Results"),
            
             
             #tags$iframe(style="height:4px; width:1%; scrolling=yes", 
            #           src="manual.pdf"),
             offset = 1
             ),
      # Show plots of the data
      column(8, 
             #add_busy_bar(color = "red", height = "8px"),
             add_busy_spinner(spin = "fading-circle"),
             textOutput("authors"),  #added to mention authors
             
             tabsetPanel(type = "tabs",
                         tabPanel("Data",
                                  textOutput("files"),
                                  textOutput("files_out"),
                                  tableOutput("list_files"),
                                  #tableOutput("table_input_data"),
                                  textOutput("loop_a"),
                                  #plotlyOutput("plot_timeseries", inline = TRUE)
                         ),
                         tabPanel("Results", 
                                  
                                  textOutput("calc"),
                                  tableOutput("table_ioi"),
                                  plotlyOutput("plot_ioi_beat"),
                                  plotlyOutput("plot_ioi_all")
                                  #textOutput("data_test")
                                  ##
                         ),
                         tabPanel("Recurrence Plots",
                                  # numericInput("file_nr", "File Nr. to rerun analysis",
                                  #              min= 1, max= 1000, value=1),
                                  # numericInput("start", "Start IOI for analysis",
                                  #              min= 1, max= 1000, value=1),
                                  # numericInput("end", "End IOI for analysis",
                                  #              min= 1, max= 1000, value=50),
                                  uiOutput("plots")
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