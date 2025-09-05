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
###

# ui.R

# 00: load packages -- ---------------------------------------------------------
# defined in global.R

# 01: global variables and dataframes  ------
# defined in "global.R"

# 02: User Interface ----

ui <- fluidPage(
  
  useShinyjs(),
  
  tags$head(
    tags$style(
      HTML(
        "
        .center-title { text-align: center; margin-top: 20px; margin-bottom: 10px; }
        .center-buttons { display: flex; justify-content: center; gap: 10px; flex-wrap: wrap; margin-bottom: 20px; }
        .spaced-column { padding: 15px; background-color: #f9f9f9; border-radius: 8px; border: 1px solid #ddd; margin-bottom: 20px; }
        .tab-content-wrapper { padding: 20px; }
        .input-section-title { font-weight: bold; margin-bottom: 10px; }
        .go-button-bottom { display: flex; justify-content: center; margin-top: 20px; }
        hr { border: none; border-top: 4px solid #000; margin: 100px 0; }
        "
      )
    ),
    tags$script(
      HTML(
        "
        $(document).ready(function() {
          $('#toggle_extra_elements').on('click', function() {
            $('#extra_element_ui').toggle();
          });
        });
        "
      )
    )
  ),
  
  # Centered Title
  div(
    class = "center-title",
    titlePanel("RANTO - Rhythm ANalysis TOol for Timeseries")
  ),
  
  # spinner indicating the app is busy
  add_busy_spinner(
    spin = "fading-circle",
    color = "#666",
    position = "top-right",
    height = "40px",
    width = "40px"
  ),
  
  # Centered Top Action Buttons
  div(
    class = "center-buttons",
    downloadButton("downloadData", "Download Rhythm Analysis Results"),
    downloadButton("downloadData_ioi", "Download Combined IOIs"),
    downloadButton("downloadData_ir", "Download Integer Ratios"),
    downloadButton("downloadData_rawdev", "Download Raw Deviations")
  ),
  
  # Full Width Tabs
  tabsetPanel(
    type = "tabs",
    
    tabPanel(
      "Input and Data",
      div(
        class = "tab-content-wrapper",
        fluidRow(
          
          # Column 1 – Methods
          column(
            4,
            div(
              class = "spaced-column",
              div(class = "input-section-title", "Analysis Settings"),
              selectInput("method", "Method for ioi beat:", choices = c("median", "mean")),
              numericInput(
                "step_size",
                "Phase Shift Step Size (s)",
                value = 0.01,
                min = 0.001,
                max = 10,
                step = 0.001
              ),
              selectInput(
                "ratio_method",
                "Integer ratio method",
                choices = c("All pairs" = "all", "Random pairs" = "random"),
                selected = "all"
              ),
              conditionalPanel(
              condition = "input.ratio_method == 'random'",
              numericInput(
                "n_random_pairs",
                "Number of random pairs",
                value = 20,
                min = 20
              ),
              numericInput(
                "seed",
                "Random seed (for reproducibility)",
                value = 123,
                min = 1
              )
              ) # end conditional panel
              # numericInput("fs", "Sampling Rate (FS) for Fourier Analysis", min = 10, max = 1000, value = 20)
            )
          ),
          
          # Column 2 – File input
          column(
            4,
            div(
              class = "spaced-column",
              div(class = "input-section-title", "Input Data"),
              # selectInput("fileextension", "File extension:", choices = c("csv", "xls", "xlsx")),
              selectInput(
                "colnames_present",
                "Column names present?",
                choices = c("FALSE", "TRUE")
              ),
              shinyDirButton("dir", "Select folder", "Please select a folder"),
              # actionButton("goButton_1", "Choose input folder"),
              textInput("sep_input", "Separator", value = ","),
              actionButton("check_button", "Check"),
              verbatimTextOutput("file_preview"),
              textInput("savename", "ID for saving (e.g., test_species)", value = "default01")
            )
          ),
          
          # Column 3 – Dynamic elements and file list
          column(
            4,
            div(
              class = "spaced-column",
              div(class = "input-section-title", "Selected Files and Elements"),
              
              textOutput("files"),
              textOutput("files_out"),
              tableOutput("list_files"),
              
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
          )
        ),
        
        # GO Button at bottom to start calculations
        div(
          class = "go-button-bottom",
          actionButton("goButton", "GO")  # You can use the same ID or a different one
        )
      )
    ),
    
    tabPanel(
      "Analysis Results",
      plotlyOutput("plot_ioi_all"),
      tags$hr(),
      plotlyOutput("plot_ir_all"),
      tags$hr(),
      tableOutput("table_ioi"),
      tags$hr(),
      plotlyOutput("rawDeviationHist"),
      tags$hr(),
      plotlyOutput("plot_beat"),
      tags$hr(),
      plotlyOutput("plot_ugof"),
      tags$hr(),
      plotlyOutput("plot_var"),
      tags$hr(),
      plotlyOutput("phasePlot")
    ),
    
    tabPanel(
      "Recurrence Plots",
      column(4, uiOutput("plots"))
    ),
    
    tabPanel(
      "Help",
      uiOutput("info")
    )
  )
)
