#-------------------------------------------------------------------------------
# GENDER ATTITUDES PROJECT
# GA_00_setup and packages.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------


# PACKAGES ---------------------------------------------------------------------

## WARNING: Remove the leading # to install packages below the first time. 
## Change filepaths below once

# if (!require(remotes)) install.packages("remotes") 
# if (!require(devtools)) install.packages("devtools") 
# remotes::install_github("fsolt/icpsrdata")                      # download ICPSR data
# devtools::install_git("https://git.rud.is/hrbrmstr/waffle.git") # install waffle package not on CRAN
# install.packages("pacman")                                      # Install pacman package if not installed

# Installs and loads packages automatically
library("pacman")                  # Load pacman package

# Install packages not yet installed & load them
pacman::p_load(
  here,       # relative file paths
  foreign,    # read data
  plyr,       #
  dplyr,      # variable processing
  tidyr,      # reshaping data
  forcats,    # reverse factor variables
  srvyr,      # calc % with survey weights
  purrr,      # to use modify_at
  MESS,       # round prop & preserve sum to 100%
  data.table, #
  gtsummary,  # pretty weighted tables
  ggplot2,    # graphing
  colorspace, # color palettes   
  conflicted) # choose default packages

  
# https://cran.r-project.org/web/packages/icpsrdata/vignettes/icpsrdata-vignette.html
library("icpsrdata")

# Address any conflicts in the packages
conflict_scout() # identify the conflicts
conflict_prefer("here", "here")
conflict_prefer("mutate", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("arrange", "dplyr")

# Set-up the Directories -------------------------------------------------------

## Set the project directory to the current working directory.
projDir <- here::here()                                     # File path to this project's directory
dataDir <- "./../../Data/@Monitoring the Future/icpsr_data" # File path to where data will be downloaded
outDir  <- "output"                                         # Name of the sub-folder where we will save results
figDir  <- file.path(outDir, "figs")                        # Name of the sub-folder where we will save generated figures


## This will create sub-directory folders in the master project directory if doesn't exist
if (!dir.exists(here::here(outDir))){
  dir.create(outDir)
} else {
  print("Output directory already exists!")
}

if (!dir.exists(here::here(figDir))){
  dir.create(figDir)
} else {
  print("Figure directory already exists!")
}

# Download the data ------------------------------------------------------------

icpsr_download(file_id = c(  7927,  7928,  7929,  7930,
                             7900,  9013,  9045,  8387,  8388, 
                             8546,  8701,  9079,  9259,  9397, 
                             9745,  9871,  6133,  6367,  6517,
                             6716,  2268,  2477,  2751,  2939,
                             3184,  3425,  3753,  4019,  4264,
                             4536, 20022, 22480, 25382, 28401,
                            30985, 34409, 34861, 35218, 36263,
                            36408, 36798, 37182, 37416, 37841,
                            38156, 38503),
               download_dir = dataDir)

# To download one survey year at a time (for yearly updates):
  # icpsr_download(file_id = 37416)

## Clean up folders -- WARNING -- This code will delete files on your hard-drive. USE WITH CAUTION

### Delete unused SAS files
to_be_deleted <- dir(path=dataDir, pattern="\\.sas$", recursive = TRUE) # Make sure list only includes files in the sub-directory
file.remove(file.path(dataDir, to_be_deleted), recursive = TRUE)        # Delete the files

to_be_deleted <- dir(path=dataDir, pattern="\\.xpt$", recursive = TRUE)
file.remove(file.path(dataDir, to_be_deleted), recursive = TRUE)

### Delete unused SPSS files
to_be_deleted <- dir(path=dataDir, pattern="\\.por$", recursive = TRUE)
file.remove(file.path(dataDir, to_be_deleted), recursive = TRUE)

to_be_deleted <- dir(path=dataDir, pattern="\\.sps$", recursive = TRUE)
file.remove(file.path(dataDir, to_be_deleted), recursive = TRUE)

remove(to_be_deleted)

# Loading the (Stata) data files -----------------------------------------------

form3dta <- list.files(path=dataDir, pattern = ".-0003-Data.dta$|.-0004-Data.dta$", recursive = TRUE) # create a list of Form 3 data files -- different folders based on the year
form5dta <- list.files(path=dataDir, pattern = ".-0005-Data.dta$|.-0006-Data.dta$", recursive = TRUE) # create a list of Form 5 data files -- different folders based on the year

mtf_F3_list <- lapply(file.path(dataDir, form3dta), read.dta) # turn the list into a list of dataframes
mtf_F5_list <- lapply(file.path(dataDir, form5dta), read.dta) # turn the list into a list of dataframes

mtfF3 <- rbindlist(mtf_F3_list, use.names=TRUE, fill=TRUE) # Convert the list of data frames into one data frame
mtfF5 <- rbindlist(mtf_F5_list, use.names=TRUE, fill=TRUE) # Convert the list of data frames into one data frame

## Keep only Form 3 & 5 variables
mtf_V3 <- select(mtfF3, V1, V5, V13, ARCHIVE_WT, starts_with("V3"))
mtf_V5 <- select(mtfF5, V1, V5, V13, ARCHIVE_WT, starts_with("V5"))

## Keep only Form 3 & 5 survey respondents
mtf_V3 <- subset(mtf_V3, !is.na(V3151))
mtf_V5 <- subset(mtf_V5, !is.na(V5151))

## Save the dataframe for easy open in the future
### Note: This data is NOT harmonized. Make frequent and judicious referral to the codebooks.
save(mtf_V3, file=file.path(dataDir, "mtf_form3.Rda"))
save(mtf_V5, file=file.path(dataDir, "mtf_form5.Rda"))

message("End of GA_00_setup and packages") # Marks end of R Script
