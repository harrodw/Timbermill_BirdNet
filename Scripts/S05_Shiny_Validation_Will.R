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
library(shinythemes)

# File Paths (Change these for your device) ------------------------------------

# Set the audio working directory
audi_dir <- "/media/will/Timbermill/Bird_Anuran_ARUs"

# Name of the directory that holds your validation data 
validations_dir <- "/media/will/Timbermill/BirdNet_Validations/MinConf_025_Shiny"

# Name of the validation data 
validations_file <- "birdnet_validations.csv"

# (Comment Out) View the validation csv to make sure everything is working
read_csv(file_path) |> filter(!is.na(True.Positive)) |> count(Species, True.Positive) |> print(n = Inf)

# Objects ----------------------------------------------------------------------

# Combine file directory and name
file_path <- path(validations_dir, validations_file)

# Define how often you want to autosave 
save_rate <- 5

# set a minimum confidence threshold
min_conf <- 0.1

# Confidence probability for the logistic regression
log_reg_prob <- 0.95

# Size of logistic regression simulationed data
sim_dat_size <- 1000

# Set working directory to the script's location
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Create the www folder if it doesn't exist
if (!dir.exists("www")) {
  dir.create("www")
}

# 2) Data Preparation ##########################################################

# Add the birds data
birds_to_valid_raw <- read_csv(file_path) 
# glimpse(birds_to_valid_raw)

# Get a list of species for the dropdown
species_list <- sort(unique(birds_to_valid_raw$Species))
# species_list

# Length of the species list 
n_species <- length(species_list)
# n_species

# Number of validations per species 
n_valid <- nrow(birds_to_valid_raw)/n_species
# n_valid

# Clean the validation data 
birds_to_valid <- birds_to_valid_raw |> 
  arrange(Species, -Confidence) |> 
  group_by(Species) |>
  mutate(Voc.ID = 1:n()) |> 
  ungroup() |> 
  mutate(Confidence = round(Confidence, 2))
# glimpse(birds_to_valid)

# Define the default bird 
default_bird <- birds_to_valid |> 
  arrange(Species) |> 
  slice_head(n = 1) |> 
  pull(Species)
# default_bird

# Starting snippets to validate
default_valid_ids <- birds_to_valid |> 
  filter(Species == default_bird) |> 
  pull(Voc.ID)
# default_valid_ids

# Palette for the pie chart
pie_pal <- c(
  "Not Validated" = "gray75",
  "True Positive" = "blue3",
  "False Positive" = "brown4"
)

# Palette for the histogram
hist_pal <- c(
  "True Positive" = "blue3",
  "False Positive" = "brown4"
)

# 3) Shiny UI ##################################################################

# UI call
ui <- page_fluid(
  
  # Theme
  theme = shinytheme("flatly"),
  # div(titlePanel("BirdNet Validation"), style = "text-align: center;"),
  
  # ----------------------------------------------------------------------------
  # Selection  -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Start the first row
  fluidRow(
    
    # Choose a species
    column(6,
           # Custom font for species selection
           h5("Select a Species", style = "margin-bottom: 10px;"), 
           # Begin species selection
           selectInput("species", NULL,
                       choices = species_list,
                       selected = default_bird,
                       width = "100%"
           )), # End species choice
    
    # Select whether or not to only see rows that are now validated 
    column(6,
           h5("Vocalizations Type", style = "margin-bottom: 10px;"), 
           # Begin valdiation type selection
           selectInput("unval_only", NULL,
                       choices = c("All Vocalizations", "Unvalidated Vocalizations"),
                       selected = "Show all Vocalizations",
                       width = "100%"
           )), # End validation choice
    
    # Hidden vocalization selection     
    div(style = "display: none;",
        selectInput("voc", "Vocalization Number",
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
           card(
             card_body(padding = 0,
           plotOutput("logit_reg", height = "180px")
           )), # End results  
           card(
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
  
  # Start the third row
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
  
  # Toggle for other sounds
  fluidRow(
    column(12, class = "mt-2",
           # Begin species selection
             selectizeInput(
               inputId = "other_species", 
              label = NULL,
              choices = species_list,
              selected = NULL,
              multiple = TRUE,
              width = "100%",
              options = list(maxItems = 5,
                            placeholder = "Select other species present in the recording",
                            plugins = list("remove_button"))
           ) # End other sound selection
    )), # End other sound
  
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

# 4) Shiny Server ##############################################################

# Call the shiny server
server <- function(input, output, session) {
  
  # ----------------------------------------------------------------------------
  # Setup ----------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Initialize reactive values
  validations <- reactiveVal(birds_to_valid)
  save_counter <- reactiveVal(0)
  
  # Autosave Function
  perform_auto_save <- function() {
    new_count <- save_counter() + 1
    if (new_count >= save_rate) {
      write.csv(validations(), file_path, row.names = FALSE)
      showNotification("Auto-save complete", type = "message")
      save_counter(0)
    } else {
      save_counter(new_count)
    }
  }
  
  # ----------------------------------------------------------------------------
  # Selection ------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Select a species and whether or not to display all rows 
  selected_species <- reactive({
    
    # Required inputs
    req(input$species, validations())
    
    # Filter to a species
    data <- validations() |> filter(Species == input$species)
    
    # Check if the user wants to filter out validated rows
    if (input$unval_only == "Unvalidated Vocalizations") {
      data <- data |> filter(is.na(True.Positive))
    }
    # Return the selection
    return(data)
  })
  
  # Select the vocalization
  selected_vocalization <- reactive({
    
    # Required inputs
    req(input$voc, selected_species())
    
    # Select vocalization
    selected_species() |> filter(Voc.ID == input$voc)
  }) # End vocalization selection
  
  # Update the number of vocalizations whenever species OR filter changes
  observeEvent(c(input$species, input$unval_only), {
    
    # Get the IDs from the filtered reactive we just made
    new_voc_list <- selected_species() |> pull(Voc.ID)
    
    # Update the hidden selector
    updateSelectInput(session = session,
                      inputId = "voc",
                      choices = new_voc_list,
                      # Select the first available unvalidated ID automatically
                      selected = head(new_voc_list, 1))
    
  }) # End vocalization list
  
  # Update the total number of vocalizations
  total_voc <- reactive({
    validations() |> filter(Species == input$species) |> pull(Voc.ID) |> max()
  }) # End total vocalization reactivity

  
  # Update the ID of the vocalization
  voc_id <- reactive({
    selected_vocalization() |> pull(Voc.ID)
  })# End vocalization ID reactivity
  
  # ----------------------------------------------------------------------------
  # Display --------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  # Spectrogram ----
  
  # Spectrogram plot render
  output$spectro_plot <- renderPlot({
    
    # Define and check file
    wav_file <- selected_vocalization() |> pull(New.File.Path)
    if(!file.exists(wav_file)) return(NULL)
    
    # Read and Save
    wav <- readWave(wav_file)
    
    # Define the confidence score
    conf <- selected_vocalization() |> pull(Confidence)
    
    # Create the spectrogram
    spectro(wav = wav,
            f = wav@samp.rate,
            scale = FALSE,
            dB = "max0",
            palette = spectro.colors,
            main = paste("Confidence:", conf, "| Vocalization:", voc_id(), "out of", total_voc()),
            cexlab = 1.3,
            cexaxis = 1.3
    ) # End spectrogram creation 
    
    # Add lines to show the 3 seconds that BirdNet actually analyzed
    abline(v = 1, col = "gray20", lwd = 2.2, lty = 2)
    abline(v = 4, col = "gray20", lwd = 2.2, lty = 2)
    
  }) # End spectrogram plot render
  
  # Logistic regression ----
  
  # Render the logistic regression
  output$logit_reg <- renderPlot({
    
    # Ensure species is selected
    req(input$species)
    
    # Validated rows for this species
    validated <- validations() |> 
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
    
    # Solve for the confidence where probability equals our acceptable
    threshold_label <- logit_reg_pred |> 
      mutate(Pred.Val.Round = signif(Pred.Val, 2)) |> 
      filter(Pred.Val.Round == log_reg_prob) |> 
      slice_head(n = 1) |> 
      pull(Pred.Val.Round)
    
  # Histogram ----
  
  # # # Histograms of true and false positives
  # output$conf_hist <- renderPlot({
  # 
  #   # Ensure species is selected
  #   req(input$species)
  # 
  #   # Pull out only the validated rows
  #   valid_dat <- validations() |>
  #     filter(Species == input$species & !is.na(True.Positive)) |>
  #     select(True.Positive, Confidence) |>
  #     mutate(True.Positive = case_when(True.Positive == 1 ~ "True Positive",
  #                                      True.Positive == 0 ~ "False Positive")) |>
  #     mutate(True.Positive = factor(True.Positive,
  #                                   levels = c("True Positive", "False Positive"))) |>
  #     droplevels()
  # 
  #   # Find the highest confidence score for a false positive
  #   false_pos <- valid_dat |>
  #   filter(True.Positive == 0)
  # 
  #   # set line to zero if there are no false positives yet
  #   max_false <- ifelse(length(false_pos) > 0, max(false_pos$Confidence), 0.01)
  # 
  #   # Historgram of confidence scores color-coded by true positive status
  #   valid_dat |>
  #     ggplot(aes(x = Confidence,
  #                # colour = True.Positive,
  #                fill = True.Positive)) +
  #     geom_histogram(alpha = 0.25) +
  #     # geom_vline(xintercept = max_false) +
  #     scale_fill_manual(values = hist_pal) +
  #     # scale_color_manual(values = hist_pal) +
  #     labs(title = "Histogram of Confidence Scores",
  #          x = "Confidence",
  #          y = "Frequency") +
  #     scale_x_continuous(limits = c(0, 1)) +
  #     guides(fill = guide_legend(ncol = 1, nrow = 2)) +
  #     theme_classic() +
  #     theme(plot.title = element_text(size = 14),
  #           legend.text = element_text(size = 12),
  #           axis.title = element_text(size = 12),
  #           axis.text.x = element_text(size = 12),
  #           legend.position = "bottom",
  #           legend.title = element_blank())
   
  
  # }) # End histogram of confidence scores

    # Pie chart of proportion validated 
    output$progress_plot <- renderPlot({
      
      # Ensure species is selected
      req(input$species) 
      
      # Count how many snippets are in each validation status
      progress_dat <- validations() |> 
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
    wav_file <- selected_vocalization() |> pull(New.File.Path)
    if(!file.exists(wav_file)) return(NULL)
    
    # Read and Save
    wav <- readWave(wav_file)
    writeWave(wav, "www/temp_audio.wav")
    
    # Use a cache-buster to track updates to the file
    timestamp <- as.numeric(Sys.time())
    audio_src <- paste0("temp_audio.wav?v=", timestamp)
    
    # Remove the previous player before adding a new one
    removeUI(selector = "audio#vocal_player")
    
    # 5. Insert the new player
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
  # Select other species--------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Select a species and whether or not to display all rows
  selected_species <- reactive({

    # Required inputs
    req(input$species, validations())

    # Filter to a species
    data <- validations() |> filter(Species == input$species)

    # Check if the user wants to filter out validated rows
    if (input$unval_only == "Unvalidated Vocalizations") {
      data <- data |> filter(is.na(True.Positive))
    }
    # Return the selection
    return(data)
  })

  # Select the vocalization
  selected_vocalization <- reactive({

    # Required inputs
    req(input$voc, selected_species())

    # Select vocalization
    selected_species() |> filter(Voc.ID == input$voc)
  }) # End vocalization selection

  # ----------------------------------------------------------------------------
  # validation -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Name of the species 
  output$species_name <- renderText(paste0("Was this sound made by: ", input$species, "?"))
  
  # True positive 
  observeEvent(input$true_pos, {
    
    # Get the current state of the data 
    current_validation <- validations()
    
    # Add a true positive 
    current_validation <- current_validation |> 
      mutate(True.Positive = case_when(Species == input$species & Voc.ID == as.numeric(input$voc) ~ 1,
                                       TRUE ~ as.numeric(True.Positive)))
    
    # Update the data 
    validations(current_validation)
    
    # Trigger autosave 
    perform_auto_save()
    
    # Define the next vocalization selection
    next_voc <- as.numeric(input$voc) + 1
    
    # Advance to the next vocalization
    if(next_voc <= total_voc()){
      updateSelectInput(session,
                        inputId = "voc",
                        selected = next_voc)
      
    }
    
  }) # End true positive button
  
  # False Positive
  observeEvent(input$false_pos, {
    
    # Get the current state of the data 
    current_validation <- validations()
    
    # Add a false positive 
    current_validation <- current_validation |> 
      mutate(True.Positive = case_when(Species == input$species & Voc.ID == as.numeric(input$voc) ~ 0,
                                       TRUE ~ as.numeric(True.Positive)))
    
    # Update the data 
    validations(current_validation)
    
    # Trigger autosave 
    perform_auto_save()
    
    # Define the next vocalization selection
    next_voc <- as.numeric(input$voc) + 1
    
    # Advance to the next vocalization
    if(next_voc <= total_voc()){
      updateSelectInput(session,
                        inputId = "voc",
                        selected = next_voc)
    } 
    
    # Update the save counter 
    new_count <- save_counter() + 1
    
    # Check if autosave is needed
    if (new_count >= save_rate) {
      
      # Overwrite the local CSV with the current reactive data
      write.csv(validations(), file_path, row.names = FALSE)
      
      # Notify the user so they know a save happened
      showNotification("Auto-save complete", type = "message")
      
      # Reset counter
      save_counter(0)
    } else {
      save_counter(new_count)
    }
    
  }) # End true positive but
  
  # ----------------------------------------------------------------------------
  # Navigation -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Previous
  observeEvent(input$prev_voc, {
    
    # Define the previous vocalization selection
    previous_voc <- as.numeric(input$voc) - 1
    
    # Make sure that it can't skip lower than 1 
    if(previous_voc >= 1){
      updateSelectInput(session,
                        inputId = "voc",
                        selected = previous_voc)
    } 
  }) # End previous button
  
  # Next 
  observeEvent(input$next_voc, {
    
    # Define the next vocalization selection
    next_voc <- as.numeric(input$voc) + 1
    
    # Make sure that it can't go higher than the maximum number of vocalizations 
    if(next_voc <= total_voc()){
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
    write.csv(validations(), file_path, row.names = FALSE)
    # Notify user of the save
    showNotification("Progress Saved", type = "message")
    
  }
  )# End manual download 
  
} # End server 

# 5) Run the app ###############################################################
shinyApp(ui, server)

