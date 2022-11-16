#####################################################################################
# Set-up the environment

## Set-up the Directories
mainDir <- "C:/Users/Joanna/Dropbox/Data/" # This should be your master data folder 
subDir  <- "@Monitoring the Future/icpsr_data" # This will be the name of the folder where you saved the MTF data
dataDir <- file.path(mainDir, subDir)

repoDir <- "C:/Users/Joanna/Dropbox/Repositories/MTF_Gender-Attitudes" # This should be your master project folder (Project GitRepository)
outDir  <- "data" # This will be the name of the folder where data output goes
projDir <- file.path(repoDir, outDir)

## This will create sub-directory folders in the master project directory if doesn't exist
if (!dir.exists(projDir)){
  dir.create(projDir)
} else {
  print("data directory already exists!")
}

## Figure out how to add figures folder to repoDir automatically

setwd(repoDir) # This will set the working directory to the master project folder

## Open the data
load(paste0(dataDir, "/mtf_form3.Rda"))
load(paste0(dataDir, "/mtf_form5.Rda"))

## Load the libraries
library(haven)
library(tidyverse)

# Create crosswalk of survey year and ICPSR Study ID 
studyid    <- c(7927,  7928,  7929,  7930,
                7900,  9013,  9045,  8387,   8388,
                8546,  8701,  9079,  9259,   9397,
                9745,  9871,  6133,  6367,   6517,
                6716,  2268,  2477,  2751,   2939,
                3184,  3425,  3753,  4019,   4264,
                4536, 20022, 22480,  25382, 28401,
                30985, 34409, 34861, 35218, 36263,
                36408, 36798, 37182, 37416)

surveyyear <- c(1976, 1977, 1978, 1979,
                1980, 1981, 1982, 1983, 1984,
                1985, 1986, 1987, 1988, 1989,
                1990, 1991, 1992, 1993, 1994,
                1995, 1996, 1997, 1998, 1999,
                2000, 2001, 2002, 2003, 2004,
                2005, 2006, 2007, 2008, 2009,
                2010, 2011, 2012, 2013, 2014,
                2015, 2016, 2017, 2018)

Xwalk <- data.frame(surveyyear, studyid)
#####################################################################################
# Set-up the data

## Select Variables
mtf_V3 <- select(mtf_V3, V5, ARCHIVE_WT, V1, V13,                      # Survey variables
               V3151, V3150, V3164, V3165, V3155, V3156, V3169,        # Demographic
               V3214, V3216, V3215, V3211, V3212)                      # Project specific

mtf_V5 <- select(mtf_V5, V5, ARCHIVE_WT, V1, V13,                      # Survey variables
               V5151, V5150, V5155, V5156, V5164, V5165, V5169,        # Demographic
               V5265)                                                  # Project specific

## Rename Variables
mtf_V3 <- dplyr::rename(mtf_V3,      
                      wt7611    = V5,      wt1217   = ARCHIVE_WT,   year       = V1,      region   = V13,
                      raceeth   = V3151,   gender   = V3150,        momed      = V3164,   momemp = V3165,
                      father    = V3155,   mother   = V3156,        religion   = V3169,
                      home      = V3214,   warm     = V3216,        suffer     = V3215,
                      lead      = V3211,   jobopp   = V3212)


mtf_V5 <- dplyr::rename(mtf_V5,      
                      wt7611    = V5,      wt1217   = ARCHIVE_WT,   year       = V1,      region   = V13,
                      raceeth   = V5151,   gender   = V5150,        momed      = V5164,   momemp   = V5165,
                      father    = V5155,   mother   = V5156,        religion   = V5169,
                      hdecide   = V5265)

## Combine forms
mtf_V3$hdecide <- NA
mtf_V5$home    <- NA
mtf_V5$warm    <- NA
mtf_V5$suffer  <- NA
mtf_V5$lead    <- NA
mtf_V5$jobopp  <- NA

data <- rbind(mtf_V3, mtf_V5)
#####################################################################################
# data Wrangling: data Transformation

## Year

data$year[data$year == "76"] <- "1976"
data$year[data$year == "88"] <- "1988"
data$year[is.na(data$year)]  <- "1978" # 34 people in 1978 have a missing year variable

data$year <- droplevels(data)$year

data$year <- as.character(data$year) %>%
             as.integer(data$year)

## Weights
table(data$year, !is.na(data$wt7611))
table(data$year, !is.na(data$wt1217))

data <- data %>%
  mutate(
    weight = case_when(
      year  <= 2011 ~ wt7611,
      year  >= 2012 ~ wt1217))

## Gender attitudes
### "home", "hdecide", "suffer", "warm", "lead", "jobopp"
  ### MTF did not include the "suffer" and "warm" items in 2018.

data <- data %>%
  mutate(
    home = case_when(
      home == "1" | home == "DISAGREE" | home == "DISAGREE:(1)" | home == "Disagree"                     ~ "DISAGREE",
      home == "2" | home == "MOST DIS" | home == "MOST DIS:(2)" | home == "Mostly disagree"              ~ "MOSTLY DISAGREE",
      home == "3" | home == "NEITHER"  | home == "NEITHER:(3)"  | home == "Neither"                      ~ "NEITHER",
      home == "4" | home == "MOST AGR" | home == "MOST AGR:(4)" | home == "Mostly agree"                 ~ "MOSTLY AGREE",
      home == "5" | home == "AGREE"    | home == "AGREE:(5)"    | home == "Agree"                        ~ "AGREE",
      TRUE                                                                                               ~  NA_character_ ),
    hdecide = case_when(
      hdecide == "1" | hdecide == "DISAGREE" | hdecide == "DISAGREE:(1)" | hdecide == "Disagree"         ~ "DISAGREE",
      hdecide == "2" | hdecide == "MOST DIS" | hdecide == "MOST DIS:(2)" | hdecide == "Mostly disagree"  ~ "MOSTLY DISAGREE",
      hdecide == "3" | hdecide == "NEITHER"  | hdecide == "NEITHER:(3)"  | hdecide == "Neither"          ~ "NEITHER",
      hdecide == "4" | hdecide == "MOST AGR" | hdecide == "MOST AGR:(4)" | hdecide == "Mostly agree"     ~ "MOSTLY AGREE",
      hdecide == "5" | hdecide == "AGREE"    | hdecide == "AGREE:(5)"    | hdecide == "Agree"            ~ "AGREE",
      TRUE                                                                                               ~  NA_character_ ),
    suffer = case_when(
      suffer == "1" | suffer == "DISAGREE" | suffer == "DISAGREE:(1)" | suffer == "Disagree"             ~ "DISAGREE",
      suffer == "2" | suffer == "MOST DIS" | suffer == "MOST DIS:(2)" | suffer == "Mostly disagree"      ~ "MOSTLY DISAGREE",
      suffer == "3" | suffer == "NEITHER"  | suffer == "NEITHER:(3)"  | suffer == "Neither"              ~ "NEITHER",
      suffer == "4" | suffer == "MOST AGR" | suffer == "MOST AGR:(4)" | suffer == "Mostly agree"         ~ "MOSTLY AGREE",
      suffer == "5" | suffer == "AGREE"    | suffer == "AGREE:(5)"    | suffer == "Agree"                ~ "AGREE",
      TRUE                                                                                               ~  NA_character_ ),
    warm = case_when(
      warm == "1" | warm == "DISAGREE" | warm == "DISAGREE:(1)" | warm == "Disagree"                     ~ "DISAGREE",
      warm == "2" | warm == "MOST DIS" | warm == "MOST DIS:(2)" | warm == "Mostly disagree"              ~ "MOSTLY DISAGREE",
      warm == "3" | warm == "NEITHER"  | warm == "NEITHER:(3)"  | warm == "Neither"                      ~ "NEITHER",
      warm == "4" | warm == "MOST AGR" | warm == "MOST AGR:(4)" | warm == "Mostly agree"                 ~ "MOSTLY AGREE",
      warm == "5" | warm == "AGREE"    | warm == "AGREE:(5)"    | warm == "Agree"                        ~ "AGREE",
      TRUE                                                                                               ~  NA_character_ ),  
    lead = case_when(
      lead == "1" | lead == "DISAGREE" | lead == "DISAGREE:(1)" | lead == "Disagree"                     ~ "DISAGREE",
      lead == "2" | lead == "MOST DIS" | lead == "MOST DIS:(2)" | lead == "Mostly disagree"              ~ "MOSTLY DISAGREE",
      lead == "3" | lead == "NEITHER"  | lead == "NEITHER:(3)"  | lead == "Neither"                      ~ "NEITHER",
      lead == "4" | lead == "MOST AGR" | lead == "MOST AGR:(4)" | lead == "Mostly agree"                 ~ "MOSTLY AGREE",
      lead == "5" | lead == "AGREE"    | lead == "AGREE:(5)"    | lead == "Agree"                        ~ "AGREE",
      TRUE                                                                                               ~  NA_character_ ),  
    jobopp = case_when(
      jobopp == "1" | jobopp == "DISAGREE" | jobopp == "DISAGREE:(1)" | jobopp == "Disagree"             ~ "DISAGREE",
      jobopp == "2" | jobopp == "MOST DIS" | jobopp == "MOST DIS:(2)" | jobopp == "Mostly disagree"      ~ "MOSTLY DISAGREE",
      jobopp == "3" | jobopp == "NEITHER"  | jobopp == "NEITHER:(3)"  | jobopp == "Neither"              ~ "NEITHER",
      jobopp == "4" | jobopp == "MOST AGR" | jobopp == "MOST AGR:(4)" | jobopp == "Mostly agree"         ~ "MOSTLY AGREE",
      jobopp == "5" | jobopp == "AGREE"    | jobopp == "AGREE:(5)"    | jobopp == "Agree"                ~ "AGREE",
      TRUE                                                                                               ~  NA_character_ ))

data <- data %>%
  mutate(
    home = case_when(
      home == "MOSTLY DISAGREE"    | home == "DISAGREE"                           ~ "Disagree",      # Feminist
      home == "MOSTLY AGREE"       | home == "AGREE"      | home == "NEITHER"     ~ "Agree",         # Conventional
      TRUE                                                                        ~  NA_character_ ),
    hdecide = case_when(
      hdecide == "MOSTLY DISAGREE" | hdecide == "DISAGREE"                        ~ "Disagree",      # Feminist
      hdecide == "MOSTLY AGREE"    | hdecide == "AGREE"   | hdecide == "NEITHER"  ~ "Agree",         # Conventional
      TRUE                                                                        ~  NA_character_ ),
    suffer = case_when(
      suffer == "MOSTLY DISAGREE"  | suffer == "DISAGREE"                         ~ "Disagree",      # Feminist
      suffer == "MOSTLY AGREE"     | suffer == "AGREE"    | suffer == "NEITHER"   ~ "Agree",         # Conventional
      TRUE                                                                        ~  NA_character_ ),
    warm = case_when(
      warm == "MOSTLY DISAGREE"    | warm == "DISAGREE"   | warm == "NEITHER"     ~ "Disagree",      # Conventional
      warm == "MOSTLY AGREE"       | warm == "AGREE"                              ~ "Agree",         # Feminist
      TRUE                                                                        ~  NA_character_ ),
    lead = case_when(
      lead == "MOSTLY DISAGREE"    | lead == "DISAGREE"   | lead == "NEITHER"     ~ "Disagree",      # Conventional
      lead == "MOSTLY AGREE"       | lead == "AGREE"                              ~ "Agree",         # Feminist
      TRUE                                                                        ~  NA_character_ ),    
    jobopp = case_when(
      jobopp == "MOSTLY DISAGREE"  | jobopp == "DISAGREE" | jobopp == "NEITHER"   ~ "Disagree",      # Conventional
      jobopp == "MOSTLY AGREE"     | jobopp == "AGREE"                            ~ "Agree",         # Feminist
      TRUE                                                                        ~  NA_character_ ))

data$home    <- factor(data$home,    levels = c("Agree","Disagree"), ordered = TRUE) 
data$hdecide <- factor(data$hdecide, levels = c("Agree","Disagree"), ordered = TRUE) 
data$suffer  <- factor(data$suffer,  levels = c("Agree","Disagree"), ordered = TRUE) 

data$warm    <- factor(data$warm,    levels = c("Disagree","Agree"), ordered = TRUE) 
data$lead    <- factor(data$lead,    levels = c("Disagree","Agree"), ordered = TRUE) 
data$jobopp  <- factor(data$jobopp,  levels = c("Disagree","Agree"), ordered = TRUE)

## Race
table(data$year, data$raceeth)
data$raceeth <- as.integer(data$raceeth)

data <- data %>%
  mutate(
    race = case_when(
      (year <= 1998 & year != 1993)     & raceeth == 2   ~ "White",
      (year <= 1998 & year != 1993)     & raceeth == 3   ~ "Black",
       year == 1993                     & raceeth == 11  ~ "White",
       year == 1993                     & raceeth == 12  ~ "Black",
      (year >= 1999 & year <= 2004)     & raceeth == 5   ~ "White",
      (year >= 1999 & year <= 2004)     & raceeth == 6   ~ "Black",
      (year >= 2005 & year <= 2009)     & raceeth == 9   ~ "White",
      (year >= 2005 & year <= 2009)     & raceeth == 8   ~ "Black",
      (year >= 2010 & year <= 2018)     & raceeth == 15  ~ "White",
      (year >= 2010 & year <= 2018)     & raceeth == 14  ~ "Black",      
      TRUE                                               ~  NA_character_ ))


## Gender
table(data$year, data$gender)

data <- data %>%
  mutate(
    gender = case_when(
      gender == "1" | gender == "MALE"   | gender == "MALE:(1)"   | gender == "Male"     ~ "Men",
      gender == "2" | gender == "FEMALE" | gender == "FEMALE:(2)" | gender == "Female"   ~ "Women",
      TRUE                                              ~  NA_character_ ))

## Create racesex
data <- data %>%
  mutate(
    racesex = case_when(
      race == "White" & gender == "Men"   ~ "White men",
      race == "White" & gender == "Women" ~ "White women",
      race == "Black" & gender == "Men"   ~ "Black men",
      race == "Black" & gender == "Women" ~ "Black women",
      TRUE                                ~  NA_character_))

data$racesex <- as_factor(data$racesex)
data$racesex <- factor(data$racesex, levels = c("White men", "White women", "Black men", "Black women"), ordered = TRUE)

## Mothers' Education
table(data$year, data$momed)

data <- data %>%
  mutate(
    momed = case_when(
      momed == "1" | momed == "GRDE SCH" | momed == "GRDE SCH:(1)" | momed == "Completed grade school or less"     ~ "COMPLETED GRADE SCHOOL OR LESS",
      momed == "2" | momed == "SOME HS"  | momed == "SOME HS:(2)"  | momed == "Some high school"                   ~ "SOME HIGH SCHOOL",
      momed == "3" | momed == "HS GRAD"  | momed == "HS GRAD:(3)"  | momed == "Completed high school"              ~ "COMPLETED HIGH SCHOOL",
      momed == "4" | momed == "SOME CLG" | momed == "SOME CLG:(4)" | momed ==  "Some college"                      ~ "SOME COLLEGE",
      momed == "5" | momed == "CLG GRAD" | momed == "CLG GRAD:(5)" | momed ==  "Completed college"                 ~ "COMPLETED COLLEGE",
      momed == "6" | momed == "GRAD SCH" | momed == "GRAD SCH:(6)" | momed ==  "Graduate or professional school"   ~ "GRADUATE OR PROFESSIONAL SCHOOL AFTER COLLEGE",
      momed == "7" | momed == "MISSING"  | momed == "DK:(7)"       | momed ==  "Don't know, or does not apply"     ~ "DON'T KNOW, OR DOES NOT APPLY", # These don't match but make missing so doesn't matter
      TRUE                                                                                                         ~  NA_character_ ))      
      
data$momed[data$momed == "DON'T KNOW, OR DOES NOT APPLY"]                  <- NA
data$momed[data$momed == "COMPLETED GRADE SCHOOL OR LESS"]                 <- "LESS THAN HIGH SCHOOL"
data$momed[data$momed == "SOME HIGH SCHOOL"]                               <- "LESS THAN HIGH SCHOOL"
data$momed[data$momed == "GRADUATE OR PROFESSIONAL SCHOOL AFTER COLLEGE"]  <- "COMPLETED COLLEGE"

data$momed <- factor(data$momed, levels = c("LESS THAN HIGH SCHOOL", "COMPLETED HIGH SCHOOL", "SOME COLLEGE", 
                                            "COMPLETED COLLEGE"), ordered = TRUE)


## Mothers' Employment
table(data$year, data$momemp)

data <- data %>%
  mutate(
    momemp = case_when(
      momemp == 1 | momemp == "NO"       | momemp == "NO:(1)"       | momemp == "No"                                                                   ~ "NO, NOT EMPLOYED",
      momemp == 2 | momemp == "SOMETIME" | momemp == "SOMETIME:(2)" | momemp == "Yes, some of the time when growing up" | momemp == "YES/SOME:(2)"     ~ "YES, SOME OF THE TIME WHEN I WAS GROWING UP",
      momemp == 3 | momemp == "MOSTTIME" | momemp == "MOSTTIME:(3)" | momemp == "Yes, most of the time"                 | momemp == "YES/MOST:(3)"     ~ "YES, MOST OF THE TIME",
      momemp == 4 | momemp == "ALL TIME" | momemp == "ALL TIME:(4)" | momemp == "Yes, all or nearly all of the time"    | momemp == "YES/NRLY ALL:(4)" ~ "YES, ALL OR NEARLY ALL OF THE TIME",
      TRUE        ~  NA_character_ ))

data$momemp <- factor(data$momemp, levels = c("NO, NOT EMPLOYED", "YES, SOME OF THE TIME WHEN I WAS GROWING UP", 
                                              "YES, MOST OF THE TIME", "YES, ALL OR NEARLY ALL OF THE TIME"), ordered = TRUE)

## Parental Residence (Family Structure)


## Religiosity
table(data$year, data$religion)

data <- data %>%
  mutate(
    religion = case_when(
      religion == 1 | religion == "NEVER"    | religion == "NEVER:(1)"    | religion == "Never"                     ~ "NEVER",
      religion == 2 | religion == "RARELY"   | religion == "RARELY:(2)"   | religion == "Rarely"                    ~ "RARELY",
      religion == 3 | religion == "1-2X/MO"  | religion == "1-2X/MO:(3)"  | religion == "Once or twice a month"     ~ "ONCE OR TWICE A MONTH",
      religion == 4 | religion == "1/WK OR+" | religion == "1/WK OR+:(4)" | religion == "About once a week or more" ~ "ABOUT ONCE A WEEK OR MORE",
      TRUE        ~  NA_character_ ))

data$religion <- factor(data$religion, levels = c("NEVER", "RARELY", 
                                                  "ONCE OR TWICE A MONTH", "ABOUT ONCE A WEEK OR MORE"), ordered = TRUE)

#####################################################################################
# Select Sample
data <- select(data, weight, year, home, hdecide, suffer, warm, lead, jobopp, 
               racesex, race, gender, momed, momemp, religion)
## Missing
mtf7617 <- subset(data, ((!is.na(home) & !is.na(suffer) & !is.na(warm) & !is.na(lead) & !is.na(jobopp)) | !is.na(hdecide)) & year!= 2018)
mtf18 <- subset(data, ((!is.na(home) & !is.na(lead) & !is.na(jobopp)) | !is.na(hdecide)) & year== 2018)

data <- rbind(mtf7617, mtf18)

data <- subset(data, (!is.na(racesex) & !is.na(momed) & !is.na(momemp) & !is.na(religion)))

data <- data %>% arrange(desc(year))

# Clean up environment
remove(mtf_V3, mtf_V5, mtf7617, mtf18)
