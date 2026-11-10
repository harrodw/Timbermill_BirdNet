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

# Clear environments
rm(list = ls())

# ############# Comment out ############################
# # Check the csv to make sure everything is saved
# csv_path <- "/home/will/NCSU/Data/BirdNet_Validations/MinConf_025_Shiny/birdnet_validations.csv"
# bn_valid <- read_csv(csv_path)
# bn_valid |> count(Species, True.Positive) |> print(n = Inf)
# bn_valid |> 
#   select(Species, starts_with("Other.Sp")) |>
#   distinct() |> 
#   print(n = Inf)
# #####################################################

# File Paths (Change these for your device) ------------------------------------

# Set the audio working directory
default_audio_dir <- "/home/will/NCSU/Data/BirdNet_Validations/MinConf_025_Shiny"

# Name of the validation data 
default_valid_file <- "birdnet_validations.csv"

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

# Set to TRUE when deploying: hides the path boxes and always uses the default folder and file
hosted <- FALSE

# Confidence probability for the logistic regression
log_reg_prob <- 0.95

# Size of logistic regression stimulated data
sim_dat_size <- 1000

# Options for the validation status
valid_status_choices <- c("Validate all Audio Files", "Check Previously Validated Audio Files")

# Other sounds to add that aren't currently in the classifier output
# These are for training new classifiers
add_sp <- c(
  "Turbine Woosh",
  "Turbine Mechanical",
  "Turbine Boom",
  "Rain",
  "Southern Leopard Frog",
  "Background"
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
  
  #-----------------------------------------------------------------------------
  # Button to select new files -------------------------------------------------
  # ----------------------------------------------------------------------------
  # Button UI
  fluidRow(
    column(12, class = "mt-2",
           actionButton(
             "read_files", "Read validation files from a new path", 
             icon = icon("redo"),
             width = "100%",
             class = "btn-primary"
           )
           
    )), # End new file file button display
  
  # ----------------------------------------------------------------------------
  # File Path ------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Start the file path row (hidden when hosted)
  if(hosted) NULL else fluidRow(
    
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
             selected = "Validate all Audio Files",
             width = "100%"
           )), # End validation choice
    
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
  # Other info in the audio file ----------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Dropdown to list other sounds
  fluidRow(
    column(6, class = "mt-2",
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
    ), # End other sounds
    
    # Dropdown to list the sound type
    column(6, class = "mt-2",
           # Custom color pallete, font, and boarder style 
           tags$head(tags$style(HTML("#voc_type + .selectize-control .selectize-input {border: 1px solid #1f3242;}"))),
           # Begin species selection
           selectizeInput(
             inputId = "voc_type", 
             label = NULL,
             choices = c("Song", "Call", "Other"),
             selected = "Song",
             multiple = TRUE,
             width = "100%",
             options = list(maxItems = 5,
                            placeholder = "What types of sound are in this recording?",
                            plugins = list("remove_button"))
           ) # End sound type selection
    )), # End sound type
  
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
                        icon = icon("thumbs-down"),
                        width = "100%",
                        class = "btn-danger"
           )) # end false positive option
    
  ), # End validation section
  
  
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
                        icon = icon("download"),
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
  valid_ssion <- reactiveVal(NULL)
  file_path_ssion <- reactiveVal(NULL)
  current_rec <- reactiveVal(NULL)
  sp_ls_ssion <- reactiveVal(character(0))
  save_counter <- reactiveVal(0)
  load_counter <- reactiveVal(0)
  
  # Per-session audio file so several users don't overwrite each other's clip
  audio_file <- paste0("temp_audio_", session$token, ".wav")
  audio_path <- file.path("www", audio_file)
  
  # Who is validating (session$user is only set by some hosting platforms)
  validator <- function() {
    u <- session$user
    if(is.null(u) || is.na(u)) Sys.info()[["user"]] else u
  }
  
  # Save the validations: write to a temp file, then rename, so a crash 
  # mid-write can't corrupt the real file
  save_validations <- function() {
    dat <- valid_ssion()
    pth <- file_path_ssion()
    if(is.null(dat) || is.null(pth)) return(invisible(FALSE))
    tmp <- paste0(pth, ".tmp")
    write.csv(dat, tmp, row.names = FALSE)
    file.rename(tmp, pth)
  }
  
  # Autosave function
  perform_auto_save <- function() {
    new_count <- save_counter() + 1
    if (new_count >= save_rate) {
      save_validations()
      # showNotification("Auto-save complete", type = "message")
      save_counter(0)
    } else {
      save_counter(new_count)
    }
  }
  
  # Save anything unsaved and clean up the audio file when the browser closes
  session$onSessionEnded(function() {
    isolate(save_validations())
    unlink(audio_path)
  })
  
  # ----------------------------------------------------------------------------
  # Read in and clean the data -------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Trigger reading in a new file when the button is pressed
  observeEvent(c(input$read_files), {
    
    # List the audio directory and validation file
    audio_dir  <- if(hosted) default_audio_dir else input$audio_dir
    valid_file <- if(hosted) default_valid_file else input$valid_file
    req(audio_dir, valid_file)
    
    # Combine file directory and name
    file_path <- path(audio_dir, valid_file)
    if(!file_exists(file_path)){
      showNotification(paste("Validation file not found:", file_path), type = "error")
      return(NULL)
    }
    
    # Keep an untouched copy of the original file (made once, never overwritten)
    backup_path <- path(path_dir(file_path), 
                        paste0(path_ext_remove(path_file(file_path)), "_ORIGINAL_BACKUP.csv"))
    if(!file_exists(backup_path)) file_copy(file_path, backup_path)
    
    # Set column types only for columns that exist in this file
    hdr <- names(read_csv(file_path, n_max = 0, show_col_types = FALSE))
    chr_cols <- intersect(c(paste0("Other.Sp", 1:5), "Call.Type", "Validated.By", "Validated.Time"), hdr)
    col_spec <- c(setNames(rep(list(col_character()), length(chr_cols)), chr_cols),
                  list(True.Positive = col_double()))
    
    # Clean the validation data
    birds_to_valid <- read_csv(
      file = file_path, 
      show_col_types = FALSE,
      col_types = do.call(cols, col_spec)
    ) |>
      # New.File.Name breaks ties so Voc.ID is the same every time the file is loaded
      arrange(Species, -Confidence, New.File.Name) |>
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
    
    # Make sure the columns the app writes to exist
    for(col in c(paste0("Other.Sp", 1:5), "Call.Type", "Validated.By", "Validated.Time")){
      if(!col %in% names(birds_to_valid)) birds_to_valid[[col]] <- NA_character_
    }
    
    # Species in the classifier output (extra sounds only go in the "other" list)
    sp_ls <- sort(unique(birds_to_valid$Species))
    
    # Store everything the rest of the app needs
    valid_ssion(birds_to_valid)
    file_path_ssion(file_path)
    sp_ls_ssion(sp_ls)
    
    # Redefine the list of other species 
    new_other_choices <- sort(c(sp_ls, add_sp))
    
    # Fill the species selection dropdowns
    updateSelectInput(session, "species", choices = sp_ls, selected = sp_ls[1])
    updateSelectizeInput(session, "other_species",
                         choices = setdiff(new_other_choices, sp_ls[1]),
                         selected = character(0))
    updateSelectInput(session, "validation_status", selected = "Validate all Audio Files")
    
    # Tell the vocalization list a new file was loaded 
    load_counter(load_counter() + 1)
    
  }) # End reading in the audio path
  
  # ----------------------------------------------------------------------------
  # Species selection ----------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Select a species and filter by validation status
  selected_species <- reactive({
    
    # Required inputs
    req(input$species, input$validation_status, valid_ssion())
    
    # Filter to a species. Previously validated rows, or the ones still to do
    if(input$validation_status == "Check Previously Validated Audio Files"){
      data <- valid_ssion() |> filter(Species == input$species & !is.na(True.Positive))
    } else {
      data <- valid_ssion() |> filter(Species == input$species)
    }
    
  }) # End validation status selection
  
  # The selected vocalization is stored on the server, together with the species it 
  # belongs to. (It used to live in a hidden input, which made a round trip to the 
  # browser and back; during a species switch that round trip left the OLD ID in place, 
  # so the app briefly showed, and loaded, the wrong recording before the right one.)
  voc_sel <- reactiveVal(NULL)    # list(species = ..., id = ...)
  
  # ID of the selected vocalization. Ignores a selection that belongs to another species
  current_voc <- reactive({
    sel <- voc_sel()
    req(sel, input$species, identical(sel$species, input$species))
    sel$id
  })
  
  # Select a vocalization (of the species currently chosen)
  select_voc <- function(id) voc_sel(list(species = input$species, id = id))
  
  # Select the vocalization
  selected_vocalization <- reactive({
    
    # Required inputs
    req(current_voc(), selected_species())
    
    # Select vocalization
    selected_species() |> filter(Voc.ID == current_voc())
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
  
  # Species changed: update the choices and reset to Validate All
  observeEvent(input$species, {
    req(input$species, valid_ssion())
    updateSelectInput(session, "validation_status",
                      choices = status_choices(),
                      selected = "Validate all Audio Files")
  }) # End validation type reset
  
  # Validations changed: refresh the choices but keep the current selection
  observeEvent(valid_ssion(), {
    req(input$species)
    choices <- status_choices()
    current <- input$validation_status
    updateSelectInput(session, "validation_status",
                      choices = choices,
                      selected = if(isTRUE(current %in% choices)) current else choices[1]) 
  }) # End validation type refresh
  
  # ----------------------------------------------------------------------------
  # Update vocalization list ---------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Update the vocalization list whenever species, validation status, or the file changes
  observeEvent(c(input$species, input$validation_status, load_counter()), {
    
    # Skip while the species box still holds a species from a previous file
    req(input$species, input$species %in% sp_ls_ssion())
    
    # Get the IDs from the filtered reactive
    new_voc_list <- selected_species() |> pull(Voc.ID)
    
    # Let the user know if there is nothing to show
    if(length(new_voc_list) == 0){
      showNotification("No recordings match this selection for this species.",
                       type = "warning")
    }
    
    # Start at the first recording that hasn't been validated yet (or the first one)
    unval_ids <- selected_species() |> filter(is.na(True.Positive)) |> pull(Voc.ID)
    start_id <- if(length(unval_ids) > 0) unval_ids[1] else head(new_voc_list, 1)
    
    # Select the starting recording (nothing if the list is empty)
    if(length(start_id) == 1) select_voc(start_id) else voc_sel(NULL)
    
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
    current <- as.numeric(current_voc())
    
    # Nothing left in this selection
    if(length(ids) == 0){
      showNotification("No recordings left in this selection for this species.", type = "message")
      return(invisible(NULL))
    }
    
    # Next ID after the current one
    next_voc <- ids[ids > current][1]
    
    # Last one in the list and it just left the list (validate-all mode)
    if(is.na(next_voc) && !(current %in% ids)){
      next_voc <- tail(ids[ids < current], 1)
    }
    
    if(length(next_voc) == 1 && !is.na(next_voc)){
      select_voc(next_voc)
    }
    
  } # End advance function
  
  # ----------------------------------------------------------------------------
  # Display --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # The recording selected in the app (only changes when that row actually changes)
  observe({
    sp_dat <- selected_species()
    vocal <- sp_dat |> filter(Voc.ID == current_voc())
    if(nrow(vocal) == 1) current_rec(vocal)
  })
  
  # The recording on display. (No debounce needed now: the selection can no longer be 
  # briefly out of date when the species changes)
  rec_d <- reactive(current_rec())
  
  # The file on display (changes only when the file path changes; used for the audio)
  current_path <- reactiveVal(NULL)
  observe({
    rec <- rec_d()
    req(rec)
    current_path(rec$New.File.Path)
  })
  
  # Read the waveform once per file. The last file read is remembered, so re-labeling
  # the same recording redraws the plot without reading the file again
  last_wav <- list(path = NULL, wav = NULL)
  current_wav <- reactive({
    wav_file <- rec_d()$New.File.Path
    req(wav_file)
    if(is.na(wav_file) || !file.exists(wav_file)) return(NULL)
    if(!identical(wav_file, last_wav$path)){
      last_wav <<- list(path = wav_file, wav = readWave(wav_file))
    }
    last_wav$wav
  })
  
  # Spectrogram plot render
  output$spectro_plot <- renderPlot({
    
    # The recording on display and its waveform
    vocal <- rec_d()
    req(vocal)
    wav <- current_wav()
    
    # If the file can't be found, say so on the plot
    if(is.null(wav)){
      return(
        ggplot() +
          annotate("text", x = 0, y = 0, size = 5,
                   label = paste("Audio file not found:\n", vocal$New.File.Path)) +
          theme_void()
      )
    }
    
    # Define the confidence score
    conf <- vocal$Confidence
    
    # Define the plot 
    plt_id <- vocal$Plot
    
    # Date 
    date <- ymd(vocal$Date)
    
    # Check if the species has been confirmed at the plot. isolate() so this is 
    # worked out when the recording changes, not after every validation
    prev_plt_voc_cnt <- isolate(valid_ssion()) |> 
      filter(Species == vocal$Species, Plot == plt_id, True.Positive == 1) |> 
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
      no = paste0("| Validated: ", ifelse(pos_status == 1, "True", "False"))
    )
    
    # Create the spectrogram
    spectro(
      wav = wav,
      f = wav@samp.rate,
      scale = FALSE,
      dB = "max0",
      palette = function(n) hcl.colors(n, "inferno"),
      collevels = seq(-75, -10, 1),
      main = paste("Confidence: ", conf, " | ", date, prev_plt_ttl, post_status_ttl),
      cexlab = 1.3,
      cexaxis = 1.3
    )  # End spectrogram creation
    
    # Add lines to show the 3 seconds that BirdNet actually analyzed
    abline(v = 1, col = "gray80", lwd = 2, lty = 2)
    abline(v = 4, col = "gray80", lwd = 2, lty = 2)
    
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
      geom_line(aes(x = Confidence, y = Pred.Val),  col = pie_pal[2], linewidth  = 1.5, alpha = 0.6) +
      geom_hline(yintercept = log_reg_prob, linetype = "dashed") +
      labs(
        title = paste0(log_reg_prob*100, "% True Positive Threshold: ", threshold_label),
        y = "p(True Positive)") +
      scale_y_continuous(limits = c(0, 1)) +
      theme_classic() +
      theme(plot.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            legend.position = "none",
            legend.title = element_blank()) 
    
  }) # End logistic regression render
  
  # Pie chart of proportion validated 
  output$progress_plot <- renderPlot({
    
    # Ensure species is selected
    req(input$species, valid_ssion()) 
    
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
            legend.position = "right",
            legend.title = element_blank()) 
    
  }) # End pie chart render
  
  # Audio render from the play button or a new file on display
  observeEvent(c(input$play, current_path()), {
    
    # Define and check file
    wav_file <- current_path()
    req(wav_file)
    if(is.na(wav_file) || !file.exists(wav_file)) return(NULL)
    
    # Move the audio file
    file.copy(wav_file, audio_path, overwrite = TRUE)
    
    # Use a cache-buster to track updates to the file
    timestamp <- as.numeric(Sys.time())
    audio_src <- paste0(audio_file, "?v=", timestamp)
    
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
  
  # Record a true (1) or false (0) positive for the recording being viewed and record the other species list
  record_validation <- function(value) {
    
    req(current_voc())
    
    # Pull out the current validation
    current_validation <- valid_ssion()
    
    # Only the row being validated
    row <- current_validation$Species == input$species & current_validation$Voc.ID == as.numeric(current_voc())
    req(any(row))
    
    # Record the validation, call type, who did it, and when
    current_validation$True.Positive[row] <- value
    current_validation$Call.Type[row] <- if(value == 1 && length(input$voc_type) > 0) {
      paste(input$voc_type, collapse = "; ")
    } else {
      NA_character_
    }
    current_validation$Validated.By[row] <- validator()
    current_validation$Validated.Time[row] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    # List of other species selected (up to 5, padded with NA)
    other_sp_ls <- head(input$other_species, 5)
    other_sp_ls <- c(other_sp_ls, rep(NA_character_, 5 - length(other_sp_ls)))
    
    # Assign those to the other species columns
    for(i in 1:5) {
      current_validation[[paste0("Other.Sp", i)]][row] <- other_sp_ls[i]
    }
    
    # Update the data 
    valid_ssion(current_validation)
    
    # Trigger autosave 
    perform_auto_save()
    
    # Advance to the next vocalization in the current list
    advance_after_validation()
    
  } # End record validation
  
  
  # Show the saved other species and call type for the recording being viewed
  observeEvent(c(input$species, voc_sel()), {
    
    req(input$species)
    vocal <- selected_vocalization()
    req(nrow(vocal) == 1)
    
    # Other species saved for this recording (empty if none)
    saved <- as.character(unlist(vocal[1, paste0("Other.Sp", 1:5)]))
    saved <- saved[!is.na(saved)]
    
    # Choices (without the species being validated) and the saved selection together
    updateSelectizeInput(session, "other_species",
                         choices = sort(setdiff(c(sp_ls_ssion(), add_sp), input$species)),
                         selected = saved)
    
    # Call type saved for this recording (default "Song" if not set yet)
    ct <- vocal$Call.Type[1]
    updateSelectizeInput(session, "voc_type",
                         selected = if(is.na(ct)) "Song" else strsplit(ct, "; ")[[1]])
    
  }) # End other species and call type reload
  
  # ------------------------------------------------------------------------------
  # Trigger the true or false positives ------------------------------------------  
  # ------------------------------------------------------------------------------
  
  # True positive 
  observeEvent(input$true_pos, {
    record_validation(1)
  }) # End true positive button
  
  # False Positive
  observeEvent(input$false_pos, {
    record_validation(0)
  }) # End false positive button
  
  # ----------------------------------------------------------------------------
  # Navigation -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  
  # Previous
  observeEvent(input$prev_voc, {
    
    req(current_voc())
    
    # Closest earlier ID in the current list
    ids <- selected_species() |> pull(Voc.ID)
    previous_voc <- tail(ids[ids < as.numeric(current_voc())], 1)
    
    if(length(previous_voc) > 0){
      select_voc(previous_voc)
    } 
  }) # End previous button
  
  # Next 
  observeEvent(input$next_voc, {
    
    req(current_voc())
    
    # Closest later ID in the current list
    ids <- selected_species() |> pull(Voc.ID)
    next_voc <- ids[ids > as.numeric(current_voc())][1]
    
    if(!is.na(next_voc)){
      select_voc(next_voc)
    } 
  }) # End next button
  
  # ----------------------------------------------------------------------------
  # Download -------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Manual Download Button
  observeEvent(input$download, {
    
    req(valid_ssion())
    
    # Save the .csv
    save_validations()
    
    # Notify user of the save
    showNotification("Progress Saved", type = "message")
    
  }) # End manual download 
  
} # End server 

################################################################################
# 4) Run the app ###############################################################
################################################################################

shinyApp(ui, server)