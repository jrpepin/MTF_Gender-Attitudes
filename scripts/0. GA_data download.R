# PACKAGES --------------------------------------------------------------------------

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
  MESS,       # round prop & preserve sum to 100%
  data.table, #
  gtsummary,  # pretty weighted tables
  ggplot2,    # graphing
  ggtext,     # automatically wraps the text inside figure
  colorspace) # color palettes   


# https://cran.r-project.org/web/packages/icpsrdata/vignettes/icpsrdata-vignette.html
library("icpsrdata")



#####################################################################################
# Set-up the Directories
mainDir <- "C:/Users/joanna/Dropbox/Data/" # This should be your master data folder 
subDir  <- "@Monitoring the Future/icpsr_data" # This will be the name of the folder where we download the MTF data
dataDir <- file.path(mainDir, subDir)

## This will create a sub-directory folder in the master project directory if doesn't exist
if (!dir.exists(dataDir)){
  dir.create(dataDir)
} else {
  print("Data directory already exists!")
}

setwd(file.path(mainDir, subDir)) # Set the working-directory to the sub-folder where we will download the data

#####################################################################################
# Download the data

icpsr_download(file_id = c(  7927,  7928,  7929,  7930,
                             7900,  9013,  9045,  8387,  8388, 
                             8546,  8701,  9079,  9259,  9397, 
                             9745,  9871,  6133,  6367,  6517,
                             6716,  2268,  2477,  2751,  2939,
                             3184,  3425,  3753,  4019,  4264,
                             4536, 20022, 22480, 25382, 28401,
                            30985, 34409, 34861, 35218, 36263,
                            36408, 36798, 37182, 37416, 38503))

# To download one survey year at a time (for yearly updates):
  # icpsr_download(file_id = 37416)

## Clean up folders -- WARNING -- This code will delete files on your hard-drive. USE WITH CAUTION

### Make sure working directory is the dataDir
getwd()

### Delete unused SAS files
list(list.files(pattern = "\\.sas$", recursive = TRUE)) # Make sure list only includes file in the sub-directory
do.call(file.remove, list(list.files(pattern = "\\.sas$", recursive = TRUE))) # Delete the files

list(list.files(pattern = "\\.xpt$", recursive = TRUE)) # Make sure list only includes file in the sub-directory
do.call(file.remove, list(list.files(pattern = "\\.xpt$", recursive = TRUE))) # Delete the files

### Delete unused SPSS files
list(list.files(pattern = "\\.por$", recursive = TRUE)) # Make sure list only includes file in the sub-directory
do.call(file.remove, list(list.files(pattern = "\\.por$", recursive = TRUE))) # Delete the files

list(list.files(pattern = "\\.sps$", recursive = TRUE)) # Make sure list only includes file in the sub-directory
do.call(file.remove, list(list.files(pattern = "\\.sps$", recursive = TRUE))) # Delete the files

#####################################################################################
# Importing the (Stata) data files

form3dta <- list.files(pattern = ".-0003-Data.dta$|.-0004-Data.dta$", recursive = TRUE) # create a list of Form 3 data files -- different folders based on the year
form5dta <- list.files(pattern = ".-0005-Data.dta$|.-0006-Data.dta$", recursive = TRUE) # create a list of Form 5 data files -- different folders based on the year

mtf_F3_list <- lapply(form3dta, read.dta) # turn the list into a list of dataframes
mtf_F5_list <- lapply(form5dta, read.dta) # turn the list into a list of dataframes

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
save(mtf_V3, file="mtf_form3.Rda")
save(mtf_V5, file="mtf_form5.Rda")