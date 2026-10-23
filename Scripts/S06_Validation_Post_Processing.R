# ################################################################################
# Title: BirdNET post-validation processing
# BirdNet R workflow, Script 5 
# Author: Will Harrod
# Date Created: 2026-05-29
################################################################################

################################################################################
# 1) Prep ######################################################################
################################################################################

# 1.1) Add packages and data ---------------------------------------------------

# Clear environments
rm(list = ls())

# Install packages 
# install.packages("tidyverse", "fs")

# Load Packages
library(tidyverse)
library(fs)
library(duckdb)

# Set Seed
set.seed(27606)

# 1.2) File directories (Change these for your device) -------------------------

# Directory where the BirdNet validations are housed
bn_dir <- "/media/will/Timbermill/BirdNet_Validations_NoConf"

# Directory where the validation summaries are held
dat_dir <-  "Data"

# 1.3) Subset the audio summaries ----------------------------------------------

# Read in the full birdnet output 
file_path <- "/media/will/Timbermill/BirdNet_Output_NoConf/BirdNET_SelectionTable.txt"

# Create an in-memory DuckDB connection
con <- dbConnect(duckdb())

# Register the file directly without loading it into RAM
bn_db <- tbl(con, sprintf("read_csv_auto('%s', delim='\t')", file_path))

# Sample 10% of the file
bn_sample <- bn_db %>%
  filter(random() <= 0.1) %>%
  collect()

# Close the connection when done
dbDisconnect(con, shutdown = TRUE)

# View the subsampled file
glimpse(bn_sample)

# (Optional) Pick 5 species
sp_valid <- c(
  "Prairie Warbler",
  "Field Sparrow",
  "Ovenbird",
  "Brown-headed Nuthatch",
  "Hooded Warbler"
)


# Separate out the audio file paths 
bn_cleaned <- bn_sample |> 
  filter(`Common Name` %in% sp_valid) |> # Optional 
  rename(
    File.Path = `Begin Path`,
    Offset = `File Offset (s)`,
    Start.Sec.Raw = `Begin Time (s)`,
    End.Sec.Raw = `End Time (s)`,
    Species = `Common Name`
  ) |> 
  mutate(Start.Sec = Offset,
         End.Sec = Offset + (End.Sec.Raw - Start.Sec.Raw)) |> 
  select(Species, Confidence, Start.Sec, End.Sec, File.Path) |> 
  mutate(Audio.File = basename(File.Path),
         Audio.File = str_replace(Audio.File, "-", "_"),
         Species = str_replace_all(Species, " ", "_"),
         Species = str_replace_all(Species, "-", "_"),
         Species = str_replace_all(Species, "'", "_")
         )
# View
glimpse(bn_cleaned) 

# View the top few rows
bn_cleaned |> 
  select(Species, Audio.File, Start.Sec) |>
  arrange(Audio.File, Start.Sec) |> 
  print(n = 30)

# View the different plots
bn_cleaned |> 
  mutate(ARU.ID = str_extract(Audio.File, "SMM2_\\d{2}")) |> 
  distinct(ARU.ID)

# Save the data 
write_csv(bn_cleaned, path(dat_dir, "birdnet_subsetted_data.csv"))

################################################################################
# 2) Create and export validation summaries ####################################
################################################################################

# List of species to include
sp_mod <- read_tsv(path(dat_dir, "possible_birds.txt"), col_names = FALSE) |> 
  rename(Species = X1) |> 
  mutate(Species = str_remove(Species, ".*_"),
         Species = str_replace_all(Species, " ", "_"),
         Species = str_replace_all(Species, "-", "_"),
         Species = str_replace_all(Species, "'", "")) |> 
  pull(Species)
sp_mod

# List the species folders
sp_dir_ls <- dir_ls(bn_dir)
sp_dir_ls

# List of species
sp_ls_full <- basename(sp_dir_ls)
sp_ls_full

# Filter to only those in the classifier
sp_ls <- tibble(Species = sp_ls_full,
                Validation.Folder = sp_dir_ls) |> 
  filter(Species %in% sp_mod)
sp_ls

# List the number of species
n_sp <- nrow(sp_ls)
n_sp

# Crate a storage object for the validations
bn_validation_dat <- tibble()

# Loop over each species and summarize the validations
for(i in 1:n_sp){
i <-  1
  # Create object for positive and negative validations
  pos_class_tbl <- tibble()
  neg_class_tbl <- tibble()
  
  # Define a species
  sp <- sp_ls$Species[i]
  
  # Define the file path 
  sp_dir <- sp_ls$Validation.Folder[i]
  
  # Check that there are positive classifications
  if(dir_exists(path(sp_dir, "Positive"))){
    # List all the positive classifications
    pos_class_full <- dir_ls(path(sp_dir, "Positive"))
    pos_class <- basename(pos_class_full)
    pos_conf <- as.numeric(str_extract(pos_class, "^\\d+(\\.\\d+)?"))
    
    
     # Assign parts of this to the data frame
     pos_class_tbl <-  tibble(
       Species = sp,
       Audio.File = pos_class,
       Confidence = pos_conf,
       True.Positive = TRUE     
     ) |> 
       mutate(
         Confidence = case_when(Confidence == 10 ~ 1, 
                                     TRUE ~ Confidence))
  }
  
  # Check that there are Negative classifications
  if(dir_exists(path(sp_dir, "Negative"))){
    # List all the Negative classifications
    neg_class_full <- dir_ls(path(sp_dir, "Negative"))
    neg_class <- basename(neg_class_full)
    neg_conf <- as.numeric(str_extract(neg_class, "^\\d+(\\.\\d+)?"))
    
    # Assign parts of this to the data frame
    neg_class_tbl <-  tibble(
      Species = sp,
      Audio.File = neg_class,
      Confidence = neg_conf,
      True.Positive = FALSE     
    ) |> 
      mutate(Confidence = case_when(Confidence == 10 ~ 1, 
                                    TRUE ~ Confidence))
  }
  
  # Combine everything
  bn_validation_dat <- bn_validation_dat |> 
    bind_rows(
      pos_class_tbl,
      neg_class_tbl
    )
  
  # Message
  message("Finished combinging validation data for ", sp, " species ", i, " out of ", n_sp)
}

# View
glimpse(bn_validation_dat)

# How many species are positive or negative?
bn_validation_dat |> 
  count(Species, True.Positive) |> 
  print(n = Inf)


# Plot
bn_validation_dat |> 
  ggplot(aes(x = Confidence, fill = True.Positive)) +
  geom_histogram() +
  facet_wrap(~Species)

# Clean the validation summaries to restore the original file names
bn_validation_prejoin <- bn_validation_dat |> 
  mutate(
    Audio.File = str_remove_all(Audio.File, "^\\d+(\\.\\d+)?_"),
    Audio.File = str_remove_all(Audio.File, "_00\\d{1}"),
    Start.Sec = str_extract(Audio.File, "S\\d+"),
    Start.Sec = as.numeric(str_remove(Start.Sec, "S")),
    End.Sec = str_extract(Audio.File, "E\\d+"),
    End.Sec = as.numeric(str_remove(End.Sec, "E"))
         ) |> 
  mutate(Audio.File = str_remove_all(Audio.File, "_[A-Z]{1}\\d+"))
# View
glimpse(bn_validation_prejoin)
bn_validation_prejoin |>
  select(Audio.File, Start.Sec, End.Sec) |> 
  print(n = 30)

# Save 
write_csv(bn_validation_prejoin, path(dat_dir, "birdnet_validation_summaries.csv"))

################################################################################
# 3) Merge with validation metadata ############################################
################################################################################

# Read in the metadata
valid_meta_dat <- read_csv(path(bn_dir, "birdnet_validations.csv")) 
# View
glimpse(valid_meta_dat)

# View the validation summaries
glimpse(bn_validation_prejoin)

# Combine
bn_valid_meta <- valid_meta_dat |> 
  select(-Confidence, -True.Positive, -Other.Species) |> 
  left_join(bn_validation_prejoin, 
            by = c("Species", "Audio.File", "Start.Sec", "End.Sec"))
# View
glimpse(bn_valid_meta)

# All there?
bn_valid_meta |> 
  select(Species, Audio.File, Confidence, True.Positive) |> 
  filter(is.na(True.Positive))

# Not too bad, remove those
bn_valid_real <- bn_valid_meta |> 
  filter(!is.na(True.Positive)) |> 
  mutate(Validated = TRUE) |> 
  select(Species, Audio.File, Start.Sec, End.Sec, Validated, True.Positive)
# View
glimpse(bn_valid_real)

################################################################################
# 4) Merge with the full data  #################################################
################################################################################

# Read back in the full dataset
bn_full <- read_csv(path(dat_dir, "birdnet_subsetted_data.csv"))
  
# View
glimpse(bn_full)

# Combine the data
bn_dat_valid <- bn_full |>
  mutate(Species = str_replace_all(Species, "-", "_")) |> 
  left_join(bn_valid_real, by = c("Species", "Audio.File", "Start.Sec", "End.Sec")) |> 
  # Fix a few things
  mutate(
    Validated = replace_na(Validated, FALSE),
    Plot = str_extract(File.Path, "[A-Z]{2}\\d{2}"),
    Date = ymd(str_extract(Audio.File, "\\d{8}")),
    Rec.Time = str_extract(Audio.File, "\\d{6}.wav"),
    Rec.Time = str_remove(Rec.Time, ".wav"),
    Rec.Hour = as.integer(str_sub(Rec.Time, start = 1, end = 2)),
    Rec.Min = as.integer(str_sub(Rec.Time, start = 3, end = 4)),
    Species = str_replace(Species, "-", "_")
         )
# View
glimpse(bn_dat_valid)
bn_dat_valid |> count(Species, True.Positive)
bn_dat_valid |> count(Species, Validated)

# What is the spread of confidence scores?
bn_dat_valid |>
  count(Species, True.Positive) |>
  print(n = Inf)

# Save the data
write_csv(bn_dat_valid, path(dat_dir, "bn_no_threshold_validated_dat.csv"))
