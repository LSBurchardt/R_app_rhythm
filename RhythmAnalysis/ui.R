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


ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .center-title { text-align: center; margin-top: 20px; margin-bottom: 10px; }
      .center-buttons { display: flex; justify-content: center; gap: 10px; flex-wrap: wrap; margin-bottom: 20px; }
      .spaced-column { padding: 15px; background-color: #f9f9f9; border-radius: 8px; border: 1px solid #ddd; margin-bottom: 20px; }
      .tab-content-wrapper { padding: 20px; }
      .input-section-title { font-weight: bold; margin-bottom: 10px; }
      .go-button-bottom { display: flex; justify-content: center; margin-top: 20px; }
    "))
  ),
  
  # Centered Title
  div(class = "center-title",
      titlePanel("RANTO - Rhythm ANalysis TOol for Timeseries")
  ),
  
  # actionLink("toggle_extra_elements", "Show more elements"),
  # 
  # hidden(
  #   div(id = "extra_element_ui",
  #       uiOutput("element_checkboxes")
  #   )
  # ),
  tags$head(
    tags$script(HTML("
    $(document).ready(function() {
      $('#toggle_extra_elements').on('click', function() {
        $('#extra_element_ui').toggle();
      });
    });
  "))
  ),
  
  # spinner indicating the app is busy
  add_busy_spinner(spin = "fading-circle", color = "#666", position = "top-right", height = "40px", width = "40px"),
  
  # Centered Top Action Buttons
  div(class = "center-buttons",
      #actionButton("goButton_2", "GO"),
      #actionButton("resetAll", "Reset Results Table"),
      downloadButton("downloadData", "Download Results"),
      downloadButton("downloadData_ioi", "Download combined raw IOIs"),
      downloadButton("downloadData_ir", "Download raw integer ratios"),
      downloadButton("downloadData_rawdev", "Download Raw Deviations")
  ),
  
  # Full Width Tabs
  tabsetPanel(
    type = "tabs",
    
    tabPanel("Input and Data",
             div(class = "tab-content-wrapper",
                 fluidRow(
                   # Column 1 – Methods
                   column(4,
                          div(class = "spaced-column",
                              div(class = "input-section-title", "Analysis Settings"),
                              selectInput("method", "Method for ioi beat:", choices = c("mean", "median")),
                              numericInput("step_size", "Phase Shift Step Size (s)", value = 0.01, min = 0.001, max = 10, step = 0.001),
                              selectInput("ratio_method", "Integer ratio method",
                                          choices = c("All pairs" = "all", "Random pairs" = "random"),
                                          selected = "all"),
                              numericInput("n_random_pairs", "Number of random pairs (if applicable)",
                                           value = 20, min = 20),
                              numericInput("seed", "Random seed (for reproducibility)", value = 123, min = 1),
                              numericInput("fs", "Sampling Rate (FS) for Fourier Analysis", min = 10, max = 1000, value = 20)
                          )
                   ),
                   
                   # Column 2 – File input
                   column(4,
                          div(class = "spaced-column",
                              div(class = "input-section-title", "Input Data"),
                              #selectInput("fileextension", "File extension:", choices = c("csv", "xls", "xlsx")),
                              selectInput("colnames_present", "Column names present?", choices = c("FALSE", "TRUE")),
                              actionButton("goButton_1", "Choose input folder"),
                              textInput("sep_input", "Separator", value = ","),
                              actionButton("check_button", "Check"),
                              verbatimTextOutput("file_preview"),
                              textInput("savename", "ID for saving (e.g., test_species)", value = "")
                          )
                   ),
                   
                   # Column 3 – Dynamic elements and file list
                   column(4,
                          div(class = "spaced-column",
                              div(class = "input-section-title", "Selected Files and Elements"),
                              
                              textOutput("files"),
                              textOutput("files_out"),
                              tableOutput("list_files"),
                              textOutput("loop_a"),
                              
                              div(class = "input-section-title", "Choose elements a–j"),
                              checkboxInput("element_a", "Use element a", TRUE),
                              checkboxInput("element_b", "Use element b", TRUE),
                              checkboxInput("element_c", "Use element c", TRUE),
                              checkboxInput("element_d", "Use element d", TRUE),
                              checkboxInput("element_e", "Use element e", TRUE),
                              checkboxInput("element_f", "Use element f", TRUE),
                              checkboxInput("element_g", "Use element g", TRUE),
                              checkboxInput("element_h", "Use element h", TRUE),
                              checkboxInput("element_i", "Use element i", TRUE),
                              checkboxInput("element_j", "Use element j", TRUE),
                              
                              
                             
                              tags$div(style = "margin-top: 10px; margin-bottom: 5px;",
                                       tags$strong("Need more elements?"),
                                       tags$p("Click below to select elements beyond j.")
                              ),
                              actionLink("toggle_extra_elements", "Show more elements"),
                              
                              # Hidden container for extra checkboxes (K–Z)
                              tags$div(
                                id = "extra_element_ui",
                                style = "display: none;",
                                uiOutput("element_checkboxes")
                              ),
                              
                              # Include toggle JavaScript
                              tags$head(
                                tags$script(HTML("
      $(document).ready(function() {
        $('#toggle_extra_elements').on('click', function() {
          $('#extra_element_ui').toggle();
        });
      });
    "))
                              )
                              ),
                              
                              #tags$p("Need more elements? Please see the Manual (page xx) for details.")
                          )
                 ),
                 
                 # Duplicated GO Button at bottom
                 div(class = "go-button-bottom",
                     actionButton("goButton_2", "GO")  # You can use the same ID or a different one
                 )
             )
    ),
    
    tabPanel("Simple Parameters",
             plotlyOutput("plot_ioi_all"),
             plotlyOutput("plot_ir_all"),
             plotlyOutput("phasePlot"),
             plotlyOutput("rawDeviationHist")
    ),
    
    tabPanel("Rhythm Analysis Results",
             textOutput("calc"),
             tableOutput("table_ioi"),
             plotlyOutput("plot_beat"),
             plotlyOutput("plot_ugof"),
             plotlyOutput("plot_var")
    ),
    
    tabPanel("Recurrence Plots",
             column(4, uiOutput("plots"))
    ),
    
    tabPanel("Help",
             uiOutput("info")
    )
  )
)
# 03: define UI --------------------------------------------------------------
# ui <- fluidPage(
#   fluidRow(
#     column(9,
#            # Application title
#            titlePanel(title="RANTO - Rhythm ANalysis TOol for Timeseries" 
#            ),
#            offset = 3),
#     fluidRow( 
#       column(2, 
#              # Sidebar with input for analysis options and sampling rate for fourier analysis
#              
#              #checkboxInput("all", "Run all analysis.", value = TRUE),
#              
#              actionButton("goButton_2","GO"),
# 
#              img(src="blank_space.png", width = "100%"),
#              
#              actionButton("resetAll", "Reset Results Table"),
#              
#              img(src="blank_space.png", width = "100%"),
#              
#              downloadButton("downloadData", "Download Results"),
#              
#              img(src="blank_space.png", width = "100%"),
#              
#              downloadButton("downloadData_ioi", "Download combined raw IOIs"),
#              
#              img(src="blank_space.png", width = "100%"),
#              
#              downloadButton("downloadData_ir", "Download raw integer ratios"),
#              
#              img(src="blank_space.png", width = "100%"),
#              
#              downloadButton("downloadData_rawdev", "Download Raw Deviations"),
#              
#              #downloadButton("downloadPlot", "Download Recurrence Plots"),
#              
#              #tags$iframe(style="height:4px; width:1%; scrolling=yes", 
#              #           src="manual.pdf"),
#              offset = 2
#              ),
#       # Show plots of the data
#       column(9, 
#              #add_busy_bar(color = "red", height = "8px"),
#              add_busy_spinner(spin = "fading-circle"),
#              textOutput("authors"),  #added to mention authors
#              tabsetPanel(type = "tabs",
#                         tabPanel("Input and Data",
#                                  
#                                  numericInput("fs", "Sampling Rate (FS) for Fourier Analysis",
#                                               min= 10, max= 1000, value=20),
#                                  
#                                  selectInput("method", "Which method to use for ioi beat:",
#                                              choices = c("mean", "median")),
#                                  numericInput("step_size", 
#                                               label = "Phase Shift Step Size (in seconds)", 
#                                               value = 0.01, 
#                                               min = 0.001, 
#                                               max = 10, 
#                                               step = 0.001),
#                                  selectInput("fileextension", "Choose file extension of input data:",
#                                              choices = c("csv", "xls", "xlsx")),
#                                  
#                                  selectInput("colnames_present", "Datasheets have column names:",
#                                              choices = c("FALSE","TRUE")),
#                                  
#                                  actionButton("goButton_1","Choose input folder"),
#                                  
#                                  img(src="blank_space.png", width = "100%"),
#                                  
#                                  textInput("savename", "ID for saving (i.e. test_species)", value = ""), 
#                                  
#                                 textInput("sep_input", "Separator", value = ","),
#                                 
#                                 actionButton("check_button", "Check"),
#                                 
#                                 verbatimTextOutput("file_preview")
#                             ),
#                          tabPanel("Data",
#                                   column(8,
#                                   textOutput("files"),
#                                   textOutput("files_out"),
#                                   tableOutput("list_files"),
#                                   #tableOutput("table_input_data"),
#                                   textOutput("loop_a")
#                                   ),
#                                   column(4,
#                                   checkboxInput("element_a", "Use element a.", value = TRUE),
#                                   checkboxInput("element_b", "Use element b.", value = TRUE),
#                                   checkboxInput("element_c", "Use element c.", value = TRUE),
#                                   checkboxInput("element_d", "Use element d.", value = TRUE),
#                                   checkboxInput("element_e", "Use element e.", value = TRUE),
#                                   checkboxInput("element_f", "Use element f.", value = TRUE),
#                                   checkboxInput("element_g", "Use element g.", value = TRUE),
#                                   checkboxInput("element_h", "Use element h.", value = TRUE),
#                                   checkboxInput("element_i", "Use element i.", value = TRUE),
#                                   checkboxInput("element_j", "Use element j.", value = TRUE),
#                                   checkboxInput("element_k", "Use element k.", value = TRUE),
#                                   checkboxInput("element_l", "Use element l.", value = TRUE),
#                                   checkboxInput("element_m", "Use element m.", value = TRUE),
#                                   checkboxInput("element_n", "Use element n.", value = TRUE),
#                                   checkboxInput("element_n", "Use element o.", value = TRUE),
#                                   checkboxInput("element_n", "Use element p.", value = TRUE),
#                                   checkboxInput("element_n", "Use element q.", value = TRUE),
#                                   checkboxInput("element_n", "Use element r.", value = TRUE),
#                                   checkboxInput("element_n", "Use element s.", value = TRUE),
#                                   checkboxInput("element_n", "Use element t.", value = TRUE),
#                                   checkboxInput("element_n", "Use element u.", value = TRUE),
#                                   checkboxInput("element_n", "Use element v.", value = TRUE),
#                                   checkboxInput("element_n", "Use element w.", value = TRUE),
#                                   checkboxInput("element_n", "Use element x.", value = TRUE),
#                                   checkboxInput("element_n", "Use element y.", value = TRUE),
#                                   checkboxInput("element_n", "Use element z.", value = TRUE),
#                                   textOutput("element"),
#                                   tableOutput("elementlist")
#                                   )
#                          ),
#                          tabPanel("Simple Parameters",
#                                   plotlyOutput("plot_ioi_all"),
#                                   plotlyOutput("plot_ir_all"),
#                                   #plotOutput("plot_rose"),
#                                   plotlyOutput("phasePlot"),
#                                   plotlyOutput("rawDeviationHist"),
#                                   ),
#                         
#                          tabPanel("Rhythm Analysis Results", 
#                                   
#                                   textOutput("calc"),
#                                   tableOutput("table_ioi"),
#                                   plotlyOutput("plot_beat"),
#                                   plotlyOutput("plot_ugof"),
#                                   plotlyOutput("plot_var"),
#                                   
#                                   ##
#                          ), tabPanel("Recurrence Plots",
#                                      
#                                      column(4,
#                                             uiOutput("plots")
#                                      ),
#                                      #column(4,
#                                      #uiOutput("rec_ugof_plots")
#                                      #),
#                                      #column(4,
#                                      #uiOutput("rec_ugof_fft_plots"))
#                                      ##
#                          ),
#                          #tabPanel("Beat Precision Analysis",
#                         #          )
#                          
#                          # tabPanel("Re-run analysis on Section",
#                          #          numericInput("file_nr", "File Nr. to rerun analysis",
#                          #                       min= 1, max= 1000, value=1),
#                          #          numericInput("start", "Start IOI for analysis",
#                          #                       min= 1, max= 1000, value=1),
#                          #          numericInput("end", "End IOI for analysis",
#                          #                       min= 1, max= 1000, value=50),
#                          #          actionButton("rerun_ioi", "Rerun on Section"),
#                          #          
#                          #          img(src="blank_space.png", width = "100%"),
#                          #          
#                          #          tableOutput("table_rerun"),
#                          #          
#                          #          downloadButton("download_rerun_Data", "Download Rerun Results")
#                          #         ),
#                          # tabPanel("Beat precision details",
#                          #          
#                          #          uiOutput("warning_ugof_detail"),
#                          #        
#                          #          actionButton("ugof_detail", "Calculate now"),
#                          #          
#                          #          plotlyOutput("maxdev100"),
#                          #          
#                          #          plotlyOutput("ugof_hist"),
#                          #          
#                          #          plotlyOutput("ugof_zscore")
#                          #          ##
#                          # ),
#                          tabPanel("Help",
#                                     uiOutput("info")##
#                                   )
#                         ),
#             
#                )
#           )
#      )
# )


# 04: Server function --------------------------------------------------------------

#defined in server.R