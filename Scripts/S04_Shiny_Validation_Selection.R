# ################################################################################
# Title: Seleting a subset of BirdNet classifications to mannually validate in an R Shiny App
# BirdNet R workflow, Script 4 
# Author: Will Harrod
# Date Created: 2026-10-27
################################################################################

################################################################################
# 1) Prep ######################################################################
################################################################################

# Clear environments
rm(list = ls())

# Load Packages 
library(tidyverse)
library(fs)
library(seewave)
library(tuneR)
library(shiny)
library(bslib)
library(audio)

# Set the input audio working directory
default_audio_in_dir <- "/media/will/Timbermill/Bird_Anuran_ARUs"

# Name of the classifier output summary file
classifier_info_file <- "combined_selectiontable.txt"

# Set the output audio working directory
default_audio_out_dir <- "/home/will/NCSU/Data/BirdNet_Validations/MinConf_025_Shiny"

# Name of the validation data output 
default_valid_file <- "birdnet_validations.csv"

################################################################################
# 2) Shiny UI ##################################################################
################################################################################

# UI call
ui <- page_fluid(
  
  # Theme
  theme = bs_theme(
    version = 5, 
    bootswatch = "flatly"
  ),
  
  # Add a little space at the top
  layout_columns(row_heights = 1),
  
  # ----------------------------------------------------------------------------
  # File Path ------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Start the file path row
  fluidRow(
    
    # Audio File input Directory
    column(4,
           # Custom color pallete and boarder style
           tags$head(tags$style(HTML("#audio_dir {border: 1px solid #1f3242;}"))),
           # Begin audio file directory entry
           textInput(
             inputId = "audio_dir",
             label = "Input full audio file directory",
             value = default_audio_in_dir, 
             width = "100%",
             placeholder = "Enter the directory containing your full audio file imputs"
           )), # End Audio file directory entry UI
    
    #  Audio File output Directory
    column(4,
           # Custom color pallete and boarder style
           tags$head(tags$style(HTML("#audio_dir {border: 1px solid #1f3242;}"))),
           # Begin audio file directory entry
           textInput(
             inputId = "audio_dir",
             label = "Output short audio file directory",
             value = default_audio_in_dir, 
             width = "100%",
             placeholder = "Enter the directory that will store your clipped output audio files"
           )), # End Audio file directory entry UI
    
    # Validation csv file name
    column(4,
           h5("Validation file name", style = "margin-bottom: 10px;"), 
           # Custom color pallete
           tags$head(tags$style(HTML("#valid_file {border: 1px solid #1f3242;}"))),
           # Begin validation file entry
           textInput(
             inputId = "valid_file",
             label = NULL,
             value = default_valid_file, 
             width = "100%",
             placeholder = "Enter the name of your validation .csv output file"
           )) # End validation csv entry UI
    
  ), # End file path selection
  
  #-----------------------------------------------------------------------------
  # Button to read in new files -------------------------------------------------
  # ----------------------------------------------------------------------------
  # Button UI
  fluidRow(
    column(12, class = "mt-2",
           actionButton(
             "read_files", "Read classifier output from a new path", 
             icon = icon("redo"),
             width = "100%",
             class = "btn-primary"
           ),
           
           # Listen to the hidden Audio Element
           tags$div(id = "audio_container")
           
    )), # End new file file button display

  # ----------------------------------------------------------------------------
  # Species list drop downs -----------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Start the species selection row
  fluidRow(
    
    # Choose a species to view
    column(6, class = "mt-2",
           # Custom color pallete, font, and boarder style 
           tags$head(tags$style(HTML("#species_view + .selectize-control .selectize-input {border: 1px solid #1f3242;}"))),
           # Begin species selection
           selectInput(
             "species_view", 
             label = "Select a Species to View",
             choices = NULL,
             selected = NULL,
             width = "100%"
           )), # End species choice'
    
    # Dropdown to list species that should be excluded
      column(6, class = "mt-2",
             # Custom color pallete, font, and boarder style 
             tags$head(tags$style(HTML("#species_drop + .selectize-control .selectize-input {border: 1px solid #1f3242;}"))),
             # Begin species selection
             selectizeInput(
               inputId = "species_drop", 
               label = "List species that you do not wish to include in the validation",
               choices = NULL,
               selected = NULL,
               multiple = TRUE,
               width = "100%"
             ) # End other sound selection
      )), # End other sounds
  
  # ----------------------------------------------------------------------------
  # Selection Sliders -----------------------------------------------------------
  # ----------------------------------------------------------------------------
  fluidRow(
 
  # Number of files to validate ------------------------------------------------
  column(12,
       sliderInput(
         inputId = "n_valid",
         label = "Select a number of samples to validate for each species",
         step = 10,
         value = 100,
         min = 10,
         max = 1000,
         width = "100%"
       )  
  )), # End vocalization Count Slider
  
  # Confidence bins ------------------------------------------------------------
  
  # Confidence bin location 1 ---------------------------------------------------
  fluidRow(
  #Bin 1
  column(6,
         sliderInput(
           inputId = "bin1_loc",
           label = "Assign the first confidence score bin",
           value = 0.5,
           min = 0,
           max = 1,
           width = "100%"
         )  
  ),
  # Bin 2
  column(6,
         sliderInput(
           inputId = "bin2_loc",
           label = "Assign the second confidence score bin",
           value = 0.5,
           min = 0,
           max = 1,
           width = "100%"
         )) 
  ), # End Confidence bin location Selection
  
  # Confidence score weightss --------------------------------------------------
  fluidRow(
  column(4,
         sliderInput(
           inputId = "bin1_wgt",
           label = "Weight the first confidence score bin",
           value = 0.33,
           min = 0,
           max = 1,
           width = "100%"
         )),
  column(4,
         sliderInput(
           inputId = "bin2_wgt",
           label = "Weight the second confidence score bin",
           value = 0.33,
           min = 0,
           max = 1,
           width = "100%"
         )),
  column(4,
         sliderInput(
           inputId = "bin3_wgt",
           label = "Weight the third confidence score bin",
           value = 0.34,
           min = 0,
           max = 1,
           width = "100%"
         )),
  ), # End toggleable selection sliders
  
  # ----------------------------------------------------------------------------
  # Histograms ------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Start the histogram display row
  fluidRow(
    
    # Plot the full confidence scores
    column(6, class = "mt-1",
           plotOutput("full_conf_hist", height = "180px")
      ), # End full confidence score histogram
    
    ## Plot the potential selection of confidence scores
    column(6, class = "mt-1",
           plotOutput("slct_conf_hist", height = "180px")
     ), # End selected confidence score histogram
  
    ), # End display row
  
  # ----------------------------------------------------------------------------
  # Button to run the code and extract  short audio files ----------------------
  # ----------------------------------------------------------------------------
  
  # Script button UI
  fluidRow(
    column(12, class = "mt-2",
           actionButton(
             "extract_audio", "Extract short audio files to validate", 
             icon = icon("list-alt"),
             width = "100%",
             class = "btn-primary"
           ),
           
           # Listen to the hidden Audio Element
           tags$div(id = "audio_container")
           
    )), # End validation extraction button
  
) # End UI


################################################################################
# 3) Shiny Server ##############################################################
################################################################################

# Call the shiny server
server <- function(input, output, session) {
  
  # ----------------------------------------------------------------------------
  # Setup ----------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Initialize reactive values (empty until a file is loaded)
  
  
  # ----------------------------------------------------------------------------
  #  Read in new audio files when the button is pressed ------------------------
  # ----------------------------------------------------------------------------
  observeEvent(input$read_files, {
    req(input$audio_dir, input$valid_file)        
       
            
  }) # End reading in the aaudio files
  
  # ----------------------------------------------------------------------------
  # Update figures and sliders when a new species is selected ------------------
  # ----------------------------------------------------------------------------
  observeEvent(input$species_view, {
    req(input$species_view)        
    
    # Render the selected valdiation confidence Histogram ------------------------
    
    # Render the full confidence Histogram ---------------------------------------
    
  }) # End species selection
  
  
  # ----------------------------------------------------------------------------
  # Remove species from this list that you do not wish to validate--------------
  # ----------------------------------------------------------------------------
  observeEvent(c(input$species_drop, input$species_drop), {
    req(input$species_view, input$species_drop)        
    
    
  }) # End species removal
  
  # ----------------------------------------------------------------------------
  
  
  # ----------------------------------------------------------------------------
  
  
  
  # ----------------------------------------------------------------------------
  # Re render the plot when almost anything changes---------------------
  # ----------------------------------------------------------------------------
  observeEvent(c(input$species_drop, input$species_drop, 
                 input$n_valid, input$bin1_loc, input$bin2_loc, 
                 input$bin1_wgt, input$bin2_wgt, input$bin1_wgt), {
    req(input$species_view, input$species_drop)        
    
    
    # Re render the selected valdiation confidence Histogram ---------------------
    
  })
  
  
  # ----------------------------------------------------------------------------
  # Run the validation selection script ----------------------------------------
  # ----------------------------------------------------------------------------
  observeEvent(input$species_view, {
    req(input$species_view)
    
  }) # End validation generation button
  
} # End server 

################################################################################
# 4) Run the app ###############################################################
################################################################################
shinyApp(ui, server)