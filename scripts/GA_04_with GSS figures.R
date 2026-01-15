## GSS & MTF -- SLIDES

## Load the packages
library("pacman")                  # Load pacman package

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

## Address any conflicts in the packages
conflict_scout() # identify the conflicts
conflict_prefer("here", "here")
conflict_prefer("mutate", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("arrange", "dplyr")


## Specify the file paths
projDir <- here::here()                                     # File path to this project's directory
dataDir <- "./../../Data/@Monitoring the Future/icpsr_data" # File path to where data will be downloaded
outDir  <- "output"                                         # Name of the sub-folder where we will save results
figDir  <- file.path(outDir, "figs")                        # Name of the sub-folder where we will save generated figures


### Load the data
gssdata <- readRDS("C:/Users/Joanna/Dropbox/Repositories/GSS_Gender-Attitudes/output/gss_ga.rds")
mtfdata <- readRDS("C:/Users/Joanna/Dropbox/Repositories/MTF_Gender-Attitudes/output/mtf_ga.rds")


#For dynamic font sizing
base = 6 # set the height of your figure (and font)
expand = 3 # font size increase (arbitrarily set at 2 for the moment)


# gssdata <- figdata (from GSS_measures.R)
gssdata <- subset(gssdata, select = -c(val))
colnames(gssdata)[colnames(gssdata)=="prog"] <- "val"
colnames(gssdata)[colnames(gssdata)=="att"] <- "variable"

gssdata <- gssdata %>%
  mutate(
    sphere = case_when(
      variable == "fepol"      ~ "Public",
      variable == "fechld"     ~ "Employed Mothers",
      variable == "fepresch"   ~ "Employed Mothers",      
      variable == "fefam"      ~ "Family",      
      TRUE                     ~  NA_character_ 
    ))

gssdata$val[is.na(gssdata$val)] <- "Conventional"
gssdata$source <- "GSS"

#mtfdata <- fig1d
mtfdata$source <- "MTF"

alldata <- rbind(mtfdata, gssdata)

###################################
## GSS & MTF Public Attitudes
pubGSS <- ggplot(subset(alldata, sphere == "Public" & val == "Feminist" & source == "GSS"),
               aes(x = year, y = prop,
                   ymin = prop_low, ymax = prop_upp,
                   color = variable, shape = variable)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.2, 1)) +
  scale_x_continuous(breaks = c(1977, 1996, 2018), label = c("'77", "'96", "'18")) +
  scale_colour_manual(name="",
                      breaks=c("lead", "jobopp", "fepol"),
                      labels=c("Agree women should be considered as seriously \nas men for jobs as executives or politicians (MTF)",
                               "Agree a woman should have exactly the same \njob opportunities as a man (MTF)",
                               "Disagree men are better politicians (GSS)             "),
                      values=c("#bbbbbb", "#619CFF", "#F8766D"))    +
  scale_shape_manual(name="",
                     breaks=c("lead", "jobopp", "fepol"),
                     labels=c("Agree women should be considered as seriously \nas men for jobs as executives or politicians (MTF)",
                              "Agree a woman should have exactly the same \njob opportunities as a man (MTF)",
                              "Disagree men are better politicians (GSS)             "),
                     values=c(19, 17, 19)) +
  theme_minimal() +
  theme(legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52"),
        legend.key.size    = unit(2, "cm"),
        strip.text.x       = element_text(face = "bold"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text               = element_text(size=24))

pubGSS

ggsave("gss&mtf_pubGSS.png", path = figDir, pubGSS, width = 15, height = 8, dpi = 300, bg = 'white')


pub <- ggplot(subset(alldata, sphere == "Public" & val == "Feminist"),
              aes(x = year, y = prop,
                  ymin = prop_low, ymax = prop_upp,
                  color = variable, shape = variable)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.2, 1)) +
  scale_x_continuous(breaks = c(1977, 1996, 2024), label = c("'77", "'96", "'24")) +
  scale_colour_manual(name="",
                      breaks=c("lead", "jobopp", "fepol"),
                      labels=c("Agree women should be considered as seriously \nas men for jobs as executives or politicians (MTF)",
                               "Agree a woman should have exactly the same \njob opportunities as a man (MTF)",
                               "Disagree men are better politicians (GSS)"),
                      values=c("#2EA5D7", "#EFA937", "#bbbbbb"))    +
  scale_shape_manual(name="",
                     breaks=c("lead", "jobopp", "fepol"),
                     labels=c("Agree women should be considered as seriously \nas men for jobs as executives or politicians (MTF)",
                              "Agree a woman should have exactly the same \njob opportunities as a man (MTF)",
                              "Disagree men are better politicians (GSS)"),
                     values=c(15, 19, 3)) +
  theme_minimal() +
  theme(legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52", size = base * expand ),
        legend.key.size    = unit(2, "cm"),
        strip.text.x       = element_text(face = "bold"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(size=base * expand))

pub

ggsave("gss&mtf_pub.png", path = figDir, pub, width = base * 2, height = base, unit = "in", dpi = 300, bg = 'white')


###################################
## GSS & MTF EMPLOYED MOTHERS
empGSS <- ggplot(subset(alldata, sphere == "Employed Mothers" & val == "Feminist" & source == "GSS"),
              aes(x = year, y = prop,
                  ymin = prop_low, ymax = prop_upp,
                  color = variable, shape = variable)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.2, 1)) +
  scale_x_continuous(breaks = c(1977, 1996, 2024), label = c("'77", "'96", "'24")) +
  scale_colour_manual(name="",
                      breaks=c("warm", "suffer", "fechld", "fepresch"),
                      labels=c("Agree a working mother can have warm \nrelationship with her kids (MTF)",
                               "Disagree preschooler suffers if mom works (MTF)",
                               "Agree a working mother can have warm \nrelationship with her kids (GSS)",
                               "Disagree preschooler suffers if mom works (GSS)"),
                      values=c("#aaaaaa", "#bbbbbb", "#C77CFF", "#00BFC4"))    +
  scale_shape_manual(name="",
                     breaks=c("warm", "suffer", "fechld", "fepresch"),
                     labels=c("Agree a working mother can have warm \nrelationship with her kids (MTF)",
                              "Disagree preschooler suffers if mom works (MTF)",
                              "Agree a working mother can have warm \nrelationship with her kids (GSS)",
                              "Disagree preschooler suffers if mom works (GSS)"),
                     values=c(19, 17, 17, 19)) +
  theme_minimal() +
  theme(legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52"),
        legend.key.size    = unit(2, "cm"),
        strip.text.x       = element_text(face = "bold"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(size=24))

empGSS

ggsave("gss&mtf_empGSS.png", path = figDir, empGSS, width = 15, height = 8, unit = "in", dpi = 300, bg = 'white')


emp <- ggplot(subset(alldata, sphere == "Employed Mothers" & val == "Feminist"),
              aes(x = year, y = prop,
                  ymin = prop_low, ymax = prop_upp,
                  color = variable, shape = variable)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.2, 1)) +
  scale_x_continuous(breaks = c(1977, 1996, 2024), label = c("'77", "'96", "'24")) +
  scale_colour_manual(name="",
                      breaks=c("warm", "suffer", "fechld", "fepresch"),
                      labels=c("Agree a working mother can have warm \nrelationship with her kids (MTF)",
                               "Disagree preschooler suffers if mom works (MTF)",
                               "Agree a working mother can have warm \nrelationship with her kids (GSS)",
                               "Disagree preschooler suffers if mom works (GSS)"),
                      values=c("#F27575", "#51E0CE", "#aaaaaa", "#bbbbbb"))    +
  scale_shape_manual(name="",
                     breaks=c("warm", "suffer", "fechld", "fepresch"),
                     labels=c("Agree a working mother can have warm \nrelationship with her kids (MTF)",
                              "Disagree preschooler suffers if mom works (MTF)",
                              "Agree a working mother can have warm \nrelationship with her kids (GSS)",
                              "Disagree preschooler suffers if mom works (GSS)"),
                     values=c(19, 17, 19, 17)) +
  theme_minimal() +
  theme(legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52", size = base * expand ),
        legend.key.size    = unit(2, "cm"),
        strip.text.x       = element_text(face = "bold"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text               = element_text(size=base * expand))

emp

ggsave("gss&mtf_emp.png", path = figDir, emp, width = base * 2, height = base, unit = "in", dpi = 300, bg = 'white')


###################################
## GSS & MTF FAMILY Attitudes
famGSS <- ggplot(subset(alldata, sphere == "Family" & val == "Feminist" & source == "GSS"),
              aes(x = year, y = prop,
                  ymin = prop_low, ymax = prop_upp,
                  color = variable, shape = variable)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.2, 1)) +
  scale_x_continuous(breaks = c(1977, 1996, 2018), label = c("'77", "'96", "'18")) +
  scale_colour_manual(name="",
                      breaks=c("home", "hdecide", "fefam"),
                      labels=c("Disagree woman takes care of home (MTF)",
                               "Disagree husband makes all family decisions (MTF)",
                               "Disagree woman takes care of home (GSS)             "),
                      values=c("#bbbbbb", "#E69F00", "#009E73"))    +
  scale_shape_manual(name="",
                     breaks=c("home", "hdecide", "fefam"),
                     labels=c("Disagree woman takes care of home (MTF)",
                              "Disagree husband makes all family decisions (MTF)",
                              "Disagree woman takes care of home (GSS)             "),
                     values=c(19, 17, 19)) +
  theme_minimal() +
  theme(legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52"),
        legend.key.size    = unit(2, "cm"),
        strip.text.x       = element_text(face = "bold"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(size=24))

famGSS

ggsave("gss&mtf_famGSS.png", path = figDir, famGSS, width = 15, height = 8, unit = "in", dpi = 300, bg = 'white')


fam <- ggplot(subset(alldata, sphere == "Family" & val == "Feminist"),
              aes(x = year, y = prop,
                  ymin = prop_low, ymax = prop_upp,
                  color = variable, shape = variable)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.2, 1)) +
  scale_x_continuous(breaks = c(1977, 1996, 2024), label = c("'77", "'96", "'24")) +
  scale_colour_manual(name="",
                      breaks=c("home", "hdecide", "fefam"),
                      labels=c("Disagree woman takes care of home (MTF)",
                               "Disagree husband makes all family decisions (MTF)",
                               "Disagree woman takes care of home (GSS)"),
                      values=c("#EFA937", "#51E0CE", "#bbbbbb"))    +
  scale_shape_manual(name="",
                     labels=c("Disagree woman takes care of home (MTF)",
                              "Disagree husband makes all family decisions (MTF)",
                              "Disagree woman takes care of home (GSS)"),
                     values=c(19, 17, 19)) +
  theme_minimal() +
  theme(legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52", size = base * expand ),
        legend.key.size    = unit(2, "cm"),
        strip.text.x       = element_text(face = "bold"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(size=base * expand))

fam 

ggsave("gss&mtf_fam.png", path = figDir, fam, width = base * 2, height = base, unit = "in", dpi = 300, bg = 'white')

