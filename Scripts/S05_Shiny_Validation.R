################################################################################
# Title: Validating BirdNET classifications with an R Shiny app
# BirdNet R workflow, Script 5 of 10
# Author: Will Harrod
# Date Created: 2025-12-23
################################################################################

# Note: Once the file paths in this script have been modified to your device, press:
# Control/Command + Shift + Enter to run the Shiny App

################################################################################
# 1) Prep ######################################################################
################################################################################

# Load Packages 
library(tidyverse)
library(fs)
library(seewave)
library(tuneR)
library(shiny)
library(bslib)
library(audio)

# Clear environments
rm(list = ls())

# File Paths (Change these for your device) ------------------------------------

# Set the audio working directory
default_audio_dir <- "/home/will/NCSU/Data/BirdNet_Validations/MinConf_025_Shiny"

# Name of the validation data 
default_valid_file <- "birdnet_validations.csv"

# DELETE ######################################
audio_dir <-  default_audio_dir
valid_file <- default_valid_file
#############################################

# Combine file directory and name
file_path <- path(default_audio_dir, default_valid_file)

# Set working directory to the script's location
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Create the www folder if it doesn't exist
if (!dir.exists("www")) {
  dir.create("www")
}

# Objects ----------------------------------------------------------------------

# Define how often you want to autosave 
save_rate <- 1

# Confidence probability for the logistic regression
log_reg_prob <- 0.95

# Size of logistic regression stimulated data
sim_dat_size <- 1000

# Options for the validation status
valid_status_choices <- c("Validate New Audio Files", "Check Previously Validated Audio Files")

# Other sounds to add that aren't currently in the classifier output
# These are for training new classifiers
add_sp <- c(
  "Turbine_Woosh",
  "Turbine_Mechanical",
  "Turbine_Boom",
  "Rain",
  "Southern Leopard Frog"
)

# Palette for your pie chart
pie_pal <- c(
  "Not Validated" = "gray75",
  "True Positive" = "blue3",
  "False Positive" = "brown4"
)

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
    
    # Audio File Directory
    column(6,
           # Custom color pallete and boarder style
           tags$head(tags$style(HTML("#audio_dir {border: 1px solid #1f3242;}"))),
           # Begin audio file directory entry
           textInput(
             inputId = "audio_dir",
             label = NULL,
             value = default_audio_dir, 
             width = "100%",
             placeholder = "Enter the directory containing your audio files"
          )), # End Audio file directory entry UI
    
    # Validation csv file name
    column(6,
           # Custom color pallete
           tags$head(tags$style(HTML("#valid_file {border: 1px solid #1f3242;}"))),
           # Begin validation file entry
           textInput(
             inputId = "valid_file",
             label = NULL,
             value = default_valid_file, 
             width = "100%",
             placeholder = "Enter the name of your validation .csv file"
          )) # End validation csv entry UI
    
  ), # End file path selection

  # ----------------------------------------------------------------------------
  # Selection  -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Start the species selection row
  fluidRow(
    
    # Choose a species
    column(6,
           # Custom color pallete, font, and boarder style 
           tags$head(tags$style(HTML("#species + .selectize-control .selectize-input {border: 1px solid #1f3242;}"))),
           h5("Select a Species", style = "margin-bottom: 10px;"), 
           # Begin species selection
           selectInput(
             "species", 
             label = NULL,
             choices = NULL,
             selected = NULL,
             width = "100%"
           )), # End species choice
    
    # Select whether or not to only see rows that are now validated 
    column(6,
           # Custom color pallete, font, and boarder style 
           tags$head(tags$style(HTML("#validation_status + .selectize-control .selectize-input {border: 1px solid #1f3242;}"))),
           h5("Validation Status", style = "margin-bottom: 10px;"), 
           # Begin valdiation type selection
           selectInput(
             "validation_status", 
             label =  NULL,
             choices = valid_status_choices[1],
             selected = "Validate New Audio Files",
             width = "100%"
           )), # End validation choice
    
    # Hidden vocalization selection     
    div(style = "display: none;",
        selectInput(
          "voc", 
          "Vocalization Number",
          choices = NULL,
          selected = NULL,
          width = "100%"
        ))  # End vocalization choice
    
  ), # End selection
  
  # ----------------------------------------------------------------------------
  # Display --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Start the display row
  fluidRow(
    # Plot the vocalization spectrogram
    column(8, class = "mt-1",
           plotOutput("spectro_plot", height = "380px")
    ), # End spectro plot
    
    # Plot the progress
    column(4, class = "mt-1",
           fluidRow(
             card_body(padding = 0,
                       plotOutput("logit_reg", height = "180px")
             )), # End logistic regression  
           fluidRow(
             card_body(padding = 0,
                       plotOutput("progress_plot", height = "180px")
             )) # End progress plot
    ) # End right display plots
  ), # End display row
  
  # Play the sound
  fluidRow(
    column(12, class = "mt-2",
           actionButton(
             "play", "Play Sound Again", 
             icon = icon("volume-up"),
             width = "100%",
             class = "btn-primary"
           ),
           
           # Listen to the hidden Audio Element
           tags$div(id = "audio_container")
           
    )), # End display
  
  # ----------------------------------------------------------------------------
  # Validate -------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Start the validation row
  fluidRow(
    
    # True positive option
    column(6, class = "mt-1",
           actionButton("true_pos", "Yes, True Positive?",
                        icon = icon("thumbs-up"),
                        width = "100%",
                        class = "btn-success"
           )), # End true positive option
    
    # False Positive Option
    column(6, class = "mt-1",
           actionButton("false_pos", "No, False Positive?",
                        icon("thumbs-down"),
                        width = "100%",
                        class = "btn-success"
           )) # end false positive option
    
  ), # End validation section
  
  # ----------------------------------------------------------------------------
  # Other Sounds in the recording ----------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Dropdown to list other sounds
  fluidRow(
    column(12, class = "mt-2",
           # Custom color pallete, font, and boarder style 
           tags$head(tags$style(HTML("#other_species + .selectize-control .selectize-input {border: 1px solid #1f3242;}"))),
           # Begin species selection
           selectizeInput(
             inputId = "other_species", 
             label = NULL,
             choices = NULL,
             selected = NULL,
             multiple = TRUE,
             width = "100%",
             options = list(maxItems = 5,
                            placeholder = "Select other species present in the recording",
                            plugins = list("remove_button"))
           ) # End other sound selection
    )), # End other sounds
  
  # ----------------------------------------------------------------------------
  # Navigation -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Start the navegation section
  fluidRow(
    
    # Previous option
    column(6, class = "mt-2",
           actionButton("prev_voc", "Previous",
                        icon = icon("chevron-left"),
                        width = "100%",
                        class = "btn-secondary"
           )), # End next option
    
    # Next Option
    column(6, class = "mt-2",
           actionButton("next_voc", "Next",
                        icon = icon("chevron-right"),
                        width = "100%",
                        class = "btn-secondary"                      
           )) # end next option
    
  ), # End navigation
  
  # ----------------------------------------------------------------------------
  # Download -------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Download button
  fluidRow(
    column(12, class = "mt-2",
           actionButton("download", "Save",
                        icon("download"),
                        width = "100%",
                        class = "btn-primary"
           ) # End download button
    )) # End download 
  
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
  valid_ssion  <- reactiveVal(NULL)
  file_path_ssion <- reactiveVal(NULL)
  sp_ls_ssion   <- reactiveVal(character(0))
  save_counter <- reactiveVal(0)
  
  # Autosave Function
  perform_auto_save <- function() {
    new_count <- save_counter() + 1
    if (new_count >= save_rate) {
      write.csv(valid_ssion(), file_path_ssion(), row.names = FALSE)
      # showNotification("Auto-save complete", type = "message")
      save_counter(0)
    } else {
      save_counter(new_count)
    }
  }
  
  # ----------------------------------------------------------------------------
  # Read in and clean the data -------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Trigger reading in a new file when the path is updated
  observeEvent(c(input$audio_dir, input$valid_file), {
    req(input$audio_dir, input$valid_file)

    # List the audio directory and validation file
    audio_dir <- input$audio_dir
    valid_file <- input$valid_file

    # Combine file directory and name
    file_path <- path(audio_dir, valid_file)
    req(file_exists(file_path))

    # Clean the validation data
    birds_to_valid <- read_csv(
      file = file_path, 
      show_col_types = FALSE,
      col_types = cols(
        True.Positive = col_double(),
        Other.Sp1 = col_character(),
        Other.Sp2 = col_character(),
        Other.Sp3 = col_character(),
        Other.Sp4 = col_character(),
        Other.Sp5 = col_character())
      )|>
      arrange(Species, -Confidence) |>
      group_by(Species) |>
      mutate(Voc.ID = 1:n()) |>
      ungroup() |>
      mutate(Confidence = round(Confidence, 2),
             Confidence = ifelse(Confidence == 1, 0.99, Confidence),
             True.Positive = as.numeric(True.Positive),
             across(starts_with("Other.Sp"), as.character),
             # Optional: Rename file paths (Comment Out)
             New.File.Path = str_replace(New.File.Path, "/media/will/Timbermill/", "/home/will/NCSU/Data/")
      )

    # Get a list of species for the dropdown
    sp_ls <- sort(c(unique(birds_to_valid$Species), add_sp))
    
    # Store everything the rest of the app needs
    valid_ssion(birds_to_valid)
    file_path_ssion(file_path)
    sp_ls_ssion(sp_ls)
    
    # Fill the species selection dropdowns
    updateSelectInput(session, "species", choices = sp_ls, selected = sp_ls[1])
    updateSelectizeInput(session, "other_species",
                         choices = setdiff(c(sp_ls, add_sp), sp_ls[1]),
                         selected = character(0))
    updateSelectInput(session, "validation_status", selected = "Validate New Audio Files")

  }) # End reading in the audio path
  
  # ----------------------------------------------------------------------------
  # Species selection ----------------------------------------------------------
  # ----------------------------------------------------------------------------
 
  # Select a species and filter by validation status
  selected_species <- reactive({
    
    # Required inputs
    req(input$species, input$validation_status, valid_ssion())
    
    # Filter to a species
    data <- valid_ssion() |> filter(Species == input$species)
    
    # Previously validated rows, or the ones still to do
    if(input$validation_status == "Check Previously Validated Audio Files"){
      filter(data, !is.na(True.Positive))
    } else {
      filter(data, is.na(True.Positive))
    }
    
  }) # End validation status selection
  
  # Select the vocalization
  selected_vocalization <- reactive({
    
    # Required inputs
    req(input$voc, selected_species())
    
    # Select vocalization
    selected_species() |> filter(Voc.ID == input$voc)
  }) # End vocalization selection
  
  
  # ----------------------------------------------------------------------------
  # Validation type selection --------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Choices for the validation type: "Previously Validated" only if this species has some
  status_choices <- function() {
    n_validated <- valid_ssion() |> 
      filter(Species == input$species, !is.na(True.Positive)) |> 
      nrow()
    if(n_validated > 0) valid_status_choices else valid_status_choices[1]
  }
  
  # Only offer previously validated when this species has at least one validated recording
  observe({
    req(input$species, valid_ssion())
    
    # Count validated recordings for this species
    n_validated <- valid_ssion() |> 
      filter(Species == input$species, !is.na(True.Positive)) |> 
      nrow()
    
    # Build the list of choices
    choices <- c("Validate New Audio Files")
    if(n_validated > 0){
      choices <- c(choices, "Check Previously Validated Audio Files")
    }
    
    # Keep the current selection if it's still available, otherwise fall back
    current <- isolate(input$validation_status)
    updateSelectInput(
      session = session, 
      "validation_status",
      choices = choices,
      selected = ifelse(!is.null(current) && current %in% choices, current, "Validate New Audio Files")
      )
    
  }) # End validation status choices
  
  
  # ----------------------------------------------------------------------------
  # Update vocalization list ---------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Update the vocalization list whenever species or validation status changes
  observeEvent(c(input$species, input$validation_status), {
    
    # Get the IDs from the filtered reactive
    new_voc_list <- selected_species() |> pull(Voc.ID)
    
    # Let the user know if there is nothing to show
    if(length(new_voc_list) == 0){
      showNotification("No recordings match this selection for this species.",
                       type = "warning")
    }
    
    # Update the hidden selector
    updateSelectInput(session = session,
                      inputId = "voc",
                      choices = new_voc_list,
                      # Select the first available ID automatically
                      selected = head(new_voc_list, 1))
    
  }) # End vocalization list
  
  # Update the total number of vocalizations
  total_voc <- reactive({
    req(input$species, valid_ssion())
    valid_ssion() |> filter(Species == input$species) |> pull(Voc.ID) |> max()
  }) # End total vocalization reactivity
  
  # Update the ID of the vocalization
  voc_id <- reactive({
    selected_vocalization() |> pull(Voc.ID)
    
  })# End vocalization ID reactivity
  
  # Move to the next ID in the CURRENT list after a validation
  advance_after_validation <- function() {
    
    # Define the current vocalization selection
    ids <- selected_species() |> pull(Voc.ID)
    current <- as.numeric(input$voc)
    
    # Nothing left in this selection
    if(length(ids) == 0){
      showNotification("No recordings left to validate for this sppecies.", type = "message")
      return(invisible(NULL))
    }
    
    # Next ID after the current one
    next_voc <- ids[ids > current][1]
    
    # Last one in the list and it just left the list (unvalidated mode)
    if(is.na(next_voc) && !(current %in% ids)){
      next_voc <- tail(ids[ids < current], 1)
    }
    
    if(length(next_voc) == 1 && !is.na(next_voc)){
      updateSelectInput(
        session = session, 
        inputId = "voc", 
        selected = next_voc
        )
    }
    
  } # End advance function
  
  # ----------------------------------------------------------------------------
  # Update validation type -----------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Species changed: update the choices and reset to Unvalidated
  observeEvent(input$species, {
    req(input$species, valid_ssion())
    updateSelectInput(session, "validation_status",
                      choices = status_choices(),
                      selected = "Validate New Audio Files")
  }) # End validation type reset
  
  # Validations changed: refresh the choices but keep the current selection
  observeEvent(valid_ssion(), {
    req(input$species)
    choices <- status_choices()
    current <- input$validation_status
    updateSelectInput(session, "validation_status",
                      choices = choices,
                      selected = ifelse(current %in% choices, current, choices[1])) 
  }) # End validation type refresh
  
  # ----------------------------------------------------------------------------
  # Select other species--------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Don't offer the species being validated as an "other" species
  observeEvent(input$species, {
    # Require species
    req(input$species)
    # Update the other species selector 
    updateSelectizeInput(
      session = session, 
      inputId = "other_species",
      choices = setdiff(c(sp_ls_ssion(), add_sp), input$species)
      )

  }) # End other species list
  
  # ----------------------------------------------------------------------------
  # Display --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Spectrogram ----
  
  # Spectrogram plot render
  output$spectro_plot <- renderPlot({
    
    # Wait until exactly one snippet is selected
    vocal <- selected_vocalization()
    req(nrow(vocal) == 1)
    
    # Define and check file
    wav_file <- vocal$New.File.Path
    if(is.na(wav_file) || !file.exists(wav_file)){
      plot.new()
      text(0.5, 0.5, paste("Audio file not found:\n", wav_file), cex = 1.2)
      return(invisible(NULL))
    }
    
    # Read and Save
    wav <- readWave(wav_file)
    
    # Define the confidence score
    conf <- vocal$Confidence
    
    # Define the plot 
    plt_id <- vocal$Plot
    
    # Check if the species has previously been seen at the plot
    prev_plt_voc_cnt <- selected_species() |> 
      filter(Plot == plt_id & True.Positive == 1) |> 
      nrow()
    
    # Make a title based on whether or not that species has been previously seen
    prev_plt_ttl <- ifelse(
      test = prev_plt_voc_cnt >= 1,
      yes = paste0("| Previously Detected at ", plt_id),
      no = paste0("| Not Previously Detected at ", plt_id)
    )
    
    # Define how you previously labeled the recording
    pos_status <- vocal$True.Positive
    post_status_ttl <- ifelse(
      test = is.na(pos_status),
      yes = "",
      no = paste0("| Previously Validated: ", ifelse(pos_status == 1, "True", "False"))
    )
    
    # Create the spectrogram
    spectro(wav = wav,
            f = wav@samp.rate,
            scale = FALSE,
            dB = "max0",
            palette = spectro.colors,
            main = paste("Confidence: ", conf, prev_plt_ttl, post_status_ttl),
            cexlab = 1.3,
            cexaxis = 1.3
    ) # End spectrogram creation 
    
    # Add lines to show the 3 seconds that BirdNet actually analyzed
    abline(v = 1, col = "gray20", lwd = 2, lty = 2)
    abline(v = 4, col = "gray20", lwd = 2, lty = 2)
    
  }) # End spectrogram plot render
  
  # Logistic regression ----
  
  # Render the logistic regression
  output$logit_reg <- renderPlot({
    
    # Ensure species is selected
    req(input$species)
    
    # Validated rows for this species
    validated <- valid_ssion() |> 
      filter(Species == input$species & !is.na(True.Positive))
    
    # Need at least one true AND one false positive to fit the model
    if(n_distinct(validated$True.Positive) < 2){
      return(
        # Blank plot if the model can't be fit yet
        ggplot() +
          annotate("text", x = 0, y = 0, 
                   label = "Validate at least one true and one\nfalse positive to see the confidence curve",
                   size = 5) +
          theme_void()
      )
    }
    
    # Model how confidence scores relate to true positive probability
    logit_reg_mod <- glm(True.Positive ~ Confidence,
                         data = validated,
                         family = binomial(link = "logit"))
    
    B0 <- coef(logit_reg_mod)[["(Intercept)"]]
    B1 <- coef(logit_reg_mod)[["Confidence"]]
    
    # Simulate predicted values 
    logit_reg_pred <- tibble(
      Confidence = seq(from = 0, to = 1, length.out = sim_dat_size)
    ) |> 
      mutate(Pred.Val = plogis(B0 + B1*Confidence))
    
    # Solve for the confidence where probability equals our acceptable minimum
    threshold <- (qlogis(log_reg_prob) - B0)/B1
    
    # Backup if the threshold can't be calculated or falls outside 0-1
    threshold_label <- if(is.na(threshold) || B1 <= 0 || threshold > 1) {
      "Not reached"
    } else {
      round(max(threshold, 0), 2)
    }
    
    # Plot the logistic regression
    logit_reg_pred |> 
      ggplot() +
      geom_line(aes(x = Confidence, y = Pred.Val)) +
      geom_hline(yintercept = log_reg_prob, linetype = "dashed") +
      labs(
        title = paste0(log_reg_prob*100, "% True Positive Threshold: ", threshold_label),
        y = "True Positive Probability") +
      scale_y_continuous(limits = c(0, 1)) +
      theme_classic()
    
  }) # End logistic regression render
  
  # }) # End histogram of confidence scores
  
  # Pie chart of proportion validated 
  output$progress_plot <- renderPlot({
    
    # Ensure species is selected
    req(input$species) 
    
    # Count how many snippets are in each validation status
    progress_dat <- valid_ssion() |> 
      filter(Species == input$species) |>
      mutate(True.Positive = case_when(is.na(True.Positive) ~ "Not Validated",
                                       True.Positive == 1 ~ "True Positive",
                                       True.Positive == 0 ~ "False Positive")) |> 
      mutate(True.Positive = factor(True.Positive, 
                                    levels = c("Not Validated", 
                                               "True Positive", 
                                               "False Positive"))) |> 
      count(True.Positive, .drop = FALSE)
    
    # Plot progress
    progress_dat |> 
      ggplot(aes(x = "", y = n, fill = True.Positive, col = True.Positive)) +
      geom_col(width = 1) +
      # Put the count in the middle of each slice (blank if the count is 0)
      geom_text(aes(label = ifelse(n > 0, n, "")),
                position = position_stack(vjust = 0.5),
                color = "black", size = 5) +
      scale_fill_manual(values = pie_pal) +
      scale_color_manual(values = pie_pal) +
      coord_polar("y", start = 0) +
      labs(title = "Validation Progress") +
      guides(fill = guide_legend(ncol = 1, nrow = 3)) +
      theme_void() +
      theme(plot.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            legend.position = "bottom",
            legend.title = element_blank()) 
    
  }) # End pie chart render
  
  # Audio render from the play button or changing vocalizations
  observeEvent(c(input$play, input$voc, input$species), {
    
    # Define and check file
    vocal <- selected_vocalization()
    req(nrow(vocal) == 1)
    wav_file <- vocal$New.File.Path
    if(is.na(wav_file) || !file.exists(wav_file)) return(NULL)
    
    # Read and Save
    wav <- readWave(wav_file)
    writeWave(wav, "www/temp_audio.wav")
    
    # Use a cache-buster to track updates to the file
    timestamp <- as.numeric(Sys.time())
    audio_src <- paste0("temp_audio.wav?v=", timestamp)
    
    # Remove the previous player before adding a new one
    removeUI(selector = "audio#vocal_player")
    
    # Insert the new player
    insertUI(
      selector = "#audio_container",
      where = "beforeEnd",
      ui = tags$audio(
        id = "vocal_player", 
        src = audio_src,
        type = "audio/wav",
        autoplay = TRUE,      
        style = "display:none;" 
      )) # End Audio player UI
    
  }, ignoreInit = TRUE) # End audio render 
  
  # ----------------------------------------------------------------------------
  # Validation -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Name of the species 
  output$species_name <- renderText(paste0("Was this sound made by: ", input$species, "?"))
  
  # True positive 
  observeEvent(input$true_pos, {
    
    req(input$voc)
    
    # Add a true positive 
    current_validation <- valid_ssion() |> 
      mutate(True.Positive = case_when(Species == input$species & Voc.ID == as.numeric(input$voc) ~ 1,
                                       TRUE ~ as.numeric(True.Positive)))
    
    # Update the data 
    valid_ssion(current_validation)
    
    # Trigger autosave 
    perform_auto_save()
    
    # Advance to the next vocalization in the current list
    advance_after_validation()
    
  }) # End true positive button
  
  # False Positive
  observeEvent(input$false_pos, {
    
    req(input$voc)
    
    # Add a false positive 
    current_validation <- valid_ssion() |> 
      mutate(True.Positive = case_when(Species == input$species & Voc.ID == as.numeric(input$voc) ~ 0,
                                       TRUE ~ as.numeric(True.Positive)))
    
    # Update the data 
    valid_ssion(current_validation)
    
    # Trigger autosave 
    perform_auto_save()
    
    # Advance to the next vocalization in the current list
    advance_after_validation()
    
  }) # End false positive button
  
  # ----------------------------------------------------------------------------
  # Assign Other species in the recording to the correct rows ------------------
  # ----------------------------------------------------------------------------

  # Notice that other species have been selected
  observeEvent(input$other_species, {
    
    req(input$voc)
    
    # Pull out the current validation
    current_validation <- valid_ssion()
    
    # List of other species selected (up to 5, padded with NA)
    other_sp_ls <- head(input$other_species, 5)
    other_sp_ls <- c(other_sp_ls, rep(NA_character_, 5 - length(other_sp_ls)))
    
    # Only the row being viewed
    row <- current_validation$Species == input$species & 
      current_validation$Voc.ID == as.numeric(input$voc)
    
    # Skip if there's no matching row, or the box already matches what's saved
    if(!any(row)) return(NULL)
    saved <- sapply(1:5, function(i) as.character(current_validation[[paste0("Other.Sp", i)]][row][1]))
    if(identical(saved, other_sp_ls)) return(NULL)
    
    # Assign those to the other species columns
    for(i in 1:5) {
      current_validation[[paste0("Other.Sp", i)]][row] <- other_sp_ls[i]
    }
    
    # Update the data
    valid_ssion(current_validation)
    
  }, ignoreNULL = FALSE) # End other species assignment
  
  # Save other species, then empty the selection whenever the vocalization changes
  observeEvent(c(input$species, input$voc), {
    
    vocal <- selected_vocalization()
    req(nrow(vocal) == 1)
    
    # Saved other species for this recording, without the NAs
    saved <- as.character(unlist(vocal[1, paste0("Other.Sp", 1:5)]))
    saved <- saved[!is.na(saved)]
    
    updateSelectizeInput(session, "other_species", selected = saved)
    
  }) # End other species reload
    
  # ----------------------------------------------------------------------------
  # Navigation -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Previous
  observeEvent(input$prev_voc, {
    
    req(input$voc)
    
    # Closest earlier ID in the current list
    ids <- selected_species() |> pull(Voc.ID)
    previous_voc <- tail(ids[ids < as.numeric(input$voc)], 1)
    
    if(length(previous_voc) > 0){
      updateSelectInput(session,
                        inputId = "voc",
                        selected = previous_voc)
    } 
  }) # End previous button
  
  # Next 
  observeEvent(input$next_voc, {
    
    req(input$voc)
    
    # Closest later ID in the current list
    ids <- selected_species() |> pull(Voc.ID)
    next_voc <- ids[ids > as.numeric(input$voc)][1]
    
    if(!is.na(next_voc)){
      updateSelectInput(session,
                        inputId = "voc",
                        selected = next_voc)
    } 
  }) # End next button
  
  # ----------------------------------------------------------------------------
  # Download -------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Manual Download Button
  observeEvent(input$download, {
    
    # Save the .csv
    write.csv(valid_ssion(), file_path_ssion(), row.names = FALSE)
    # Notify user of the save
    showNotification("Progress Saved", type = "message")
    
  }
  )# End manual download 
  
} # End server 

# 5) Run the app ###############################################################
shinyApp(ui, server)