setwd(repoDir)

library("srvyr")
library("ggplot2")

#####################################################################################
## Set-up data

figdata <- data %>%
  mutate(
    home = case_when(
      home     == "Disagree"     ~ "Feminist",
      home     == "Agree"        ~ "Conventional",
      TRUE                       ~  NA_character_),
    hdecide = case_when(
      hdecide  == "Disagree"     ~ "Feminist",
      hdecide  == "Agree"        ~ "Conventional",
      TRUE                       ~  NA_character_),
    suffer = case_when(
      suffer   == "Disagree"     ~ "Feminist",
      suffer   == "Agree"        ~ "Conventional",
      TRUE                       ~  NA_character_),
    warm = case_when(
      warm     == "Disagree"     ~ "Conventional",
      warm     == "Agree"        ~ "Feminist",
      TRUE                       ~  NA_character_),
    jobopp = case_when(
      jobopp   == "Disagree"     ~ "Conventional",
      jobopp   == "Agree"        ~ "Feminist",
      TRUE                       ~  NA_character_),
    lead = case_when(
      lead     == "Disagree"     ~ "Conventional",
      lead     == "Agree"        ~ "Feminist",
      TRUE                       ~  NA_character_))

cat_vars <- c( "home", "hdecide", "suffer", "warm", "lead", "jobopp")

figdata <- figdata %>%
  modify_at(cat_vars, as.factor)

figdata <- figdata %>% gather(variable, val, -year, -racesex, - race, -gender, -momed, -momemp, -religion, -weight)

## Create a grouping variable
figdata <- figdata %>%
  mutate(
    sphere = case_when(
      variable == "home"      ~ "Family",
      variable == "hdecide"   ~ "Family",
      variable == "warm"      ~ "Employed Mothers",
      variable == "suffer"    ~ "Employed Mothers",
      variable == "lead"      ~ "Public",
      variable == "jobopp"    ~ "Public",
      TRUE                    ~  NA_character_
    ))

figdata$sphere <- factor(figdata$sphere, levels = c("Public", "Employed Mothers", "Family"), ordered = TRUE)

figdata_svy <- figdata %>%
  as_survey_design(id = NULL,
                   weights = weight)

#####################################################################################
# Figure 1. Young Adults' Gender Attitudes 

## Averages
  ### Do these separately because 2018 is missing data on employed mothers.
fig1a <- figdata_svy %>%
  filter(sphere != "Employed Mothers") %>%
  group_by(year, sphere, variable, val) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fig1b <- figdata_svy %>%
  filter(sphere == "Employed Mothers" & year <= 2017) %>%
  group_by(year, sphere, variable, val) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fig1d <- rbind(fig1a, fig1b)
remove(fig1a, fig1b)

write.csv(fig1d, "data/dol_Figure 1.csv")

## Figure 1
fig1 <- ggplot(subset(fig1d, val == "Feminist"),
            aes(x = year, y = prop,
                ymin = prop_low, ymax = prop_upp,
                color = variable, group = variable, fill = variable)) +
  facet_wrap( ~ sphere) +
  geom_line(size = .9) +
  geom_ribbon(alpha = 0.1, color = NA) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.2, 1)) +
  scale_x_continuous(breaks = c(1976, 1996, 2018), label = c("'76", "'96", "'18")) +
  scale_colour_manual(name="",
                        breaks=c("lead", "jobopp", "warm", "suffer", "hdecide", "home"),
                        labels=c("Agree women should be considered as seriously as men \nfor jobs as executives or politicians",
                                 "Agree a woman should have exactly the same job opportunities as a man",
                                 "Agree a working mother can establish just as warm and secure \na relationship with her children",
                                 "Disagree a preschool child is likely to suffer if the mother works",
                                 "Disagree husband makes all family decisions", "Disagree woman takes care of home"),
                        values=c("#5D478B", "#D1AF00", "#605A52", "#CD3278", "#CD661D", "#116A66")) +
  scale_fill_manual(values=c("#5D478B", "#D1AF00", "#605A52", "#CD3278", "#CD661D", "#116A66")) +
  ggtitle("Trends in Young People's Attitudes About Gender") +
  labs(caption = "Pepin & Cotter \nData source: Monitoring the Future Surveys") +
  guides(fill = FALSE) +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.87), 
        legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52"),
        strip.text.x       = element_text(face = "bold"),
        plot.caption       = element_text(color = "#605A52", size = 6),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

fig1

ggsave("figures/fig1.png", fig1, width = 8, height = 7, dpi = 300)
#####################################################################################
# Figure 2. Young Adults' Gender Attitudes -- by gender

## Averages
fig2a <- figdata_svy %>%
  filter(sphere != "Employed Mothers") %>%
  group_by(year, gender, sphere, variable, val) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fig2b <- figdata_svy %>%
  filter(sphere == "Employed Mothers" & year <= 2017) %>%
  group_by(year, gender, sphere, variable, val) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fig2d <- rbind(fig2a, fig2b)
remove(fig2a, fig2b)

write.csv(fig2d, "data/dol_Figure 2.csv")

## Figure 2
fam_names <- c("hdecide" = "Disagree husband makes all family decisions", "home" = "Disagree woman takes care of home")

fig2 <- ggplot(subset(fig2d, val == "Feminist" & sphere == "Family"),
               aes(x = year, y = prop,
                   ymin = prop_low, ymax = prop_upp,
                   color = gender, group = gender)) +
  facet_wrap( ~ variable, labeller = as_labeller(fam_names)) +
  geom_point(size = 1.7) +
  geom_errorbar(width=.3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = c(1976, 1996, 2018), label = c("'76", "'96", "'18")) +
  scale_colour_manual(name="",
                      breaks= c("Women", "Men"),
                      labels=c("Young Women", "Young Men"),
                      values=c("#386cb0", "#f0027f")) +
  ggtitle("Gender Differences in Young People's Attitudes About Gender in Families") +
  labs(caption = "Pepin & Cotter \nData source: Monitoring the Future Surveys") +
  theme_minimal() +
  theme(legend.position    = "top", 
        legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52"),
        strip.text.x       = element_text(face = "bold"),
        plot.caption       = element_text(color = "#605A52", size = 6),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

  fig2
  
ggsave("figures/fig2.png", fig2, width = 8, height = 5, dpi = 300)

#####################################################################################
# Figure 3. Young Adults' Gender Attitudes -- by moms' employment

## Averages
fig3a <- figdata_svy %>%
  filter(sphere != "Employed Mothers") %>%
  group_by(year, momemp, sphere, variable, val) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fig3b <- figdata_svy %>%
  filter(sphere == "Employed Mothers" & year <= 2017) %>%
  group_by(year, momemp, sphere, variable, val) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fig3d <- rbind(fig3a, fig3b)
remove(fig3a, fig3b)

write.csv(fig3d, "data/dol_Figure 3.csv")

## Figure 3
fam_names <- c("hdecide" = "Disagree husband makes all family decisions", "home" = "Disagree woman takes care of home")

fig3 <- ggplot(subset(fig3d, val == "Feminist" & sphere == "Family"),
               aes(x = year, y = prop,
                   ymin = prop_low, ymax = prop_upp,
                   color = momemp, group = momemp)) +
  facet_wrap( ~ variable, labeller = as_labeller(fam_names)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = c(1976, 1996, 2018), label = c("'76", "'96", "'18")) +
  scale_colour_manual(name="",
                      labels=c("Not employed", "Sometimes employed", "Mostly employed", "Employed"),
                      values=c("#7b3294", "#c2a5cf", "#a6dba0", "#008837")) +
  ggtitle("Young People's Attitudes About Gender in Families \nBy Moms' Employment Status while Growing-up") +
  labs(caption = "Pepin & Cotter \nData source: Monitoring the Future Surveys") +
  theme_minimal() +
  theme(legend.position    = "top", 
        legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52"),
        strip.text.x       = element_text(face  = "bold"),
        plot.caption       = element_text(color = "#605A52", size = 6),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

fig3

ggsave("figures/fig3.png", fig3, width = 8, height = 5, dpi = 300)

#####################################################################################
# Figure 4. Young Adults' Gender Attitudes -- by moms' education

## Averages
fig4a <- figdata_svy %>%
  filter(sphere != "Employed Mothers") %>%
  group_by(year, momed, sphere, variable, val) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fig4b <- figdata_svy %>%
  filter(sphere == "Employed Mothers" & year <= 2017) %>%
  group_by(year, momed, sphere, variable, val) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fig4d <- rbind(fig4a, fig4b)
remove(fig4a, fig4b)

write.csv(fig4d, "data/dol_Figure 4.csv")

## Figure 4
fam_names <- c("hdecide" = "Disagree husband makes all family decisions", "home" = "Disagree woman takes care of home")

fig4 <- ggplot(subset(fig4d, val == "Feminist" & sphere == "Family"),
               aes(x = year, y = prop,
                   ymin = prop_low, ymax = prop_upp,
                   color = momed, group = momed)) +
  facet_wrap( ~ variable, labeller = as_labeller(fam_names)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "loess", span = 0.25, se = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = c(1976, 1996, 2018), label = c("'76", "'96", "'18")) +
  scale_colour_manual(name="",
                      labels=c("< High School", "High School", "Some College", "College", "College +"),
                      values=c("#5f1560", "#ab4ebf", "#f6ce28", "#6b8550")) +
  ggtitle("Young People's Attitudes About Gender in Families \nBy Moms' Education") +
  labs(caption = "Pepin & Cotter \nData source: Monitoring the Future Surveys") +
  theme_minimal() +
  theme(legend.position    = "top", 
        legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52"),
        strip.text.x       = element_text(face  = "bold"),
        plot.caption       = element_text(color = "#605A52", size = 6),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

fig4

ggsave("figures/fig4.png", fig4, width = 8, height = 5, dpi = 300)

#####################################################################################
# Figure 5. Young Adults' Gender Attitudes -- by race

## Averages
fig5a <- figdata_svy %>%
  filter(sphere != "Employed Mothers") %>%
  group_by(year, race, sphere, variable, val) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fig5b <- figdata_svy %>%
  filter(sphere == "Employed Mothers" & year <= 2017) %>%
  group_by(year, race, sphere, variable, val) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fig5d <- rbind(fig5a, fig5b)
remove(fig5a, fig5b)

write.csv(fig5d, "data/dol_Figure 5.csv")

## Figure 5
fam_names <- c("hdecide" = "Disagree husband makes all family decisions", "home" = "Disagree woman takes care of home")

fig5 <- ggplot(subset(fig5d, val == "Feminist" & sphere == "Family"),
               aes(x = year, y = prop,
                   ymin = prop_low, ymax = prop_upp,
                   color = race, group = race)) +
  facet_wrap( ~ variable, labeller = as_labeller(fam_names)) +
  geom_point(alpha = .4) +
  geom_smooth(method = "loess", span = 0.15, fill = "lightgrey") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = c(1976, 1996, 2018), label = c("'76", "'96", "'18")) +
  scale_colour_manual(name="",
                      labels=c("Black", "White"),
                      values=c("#5D478B", "#CD661D")) +
  ggtitle("Racial Differences in Young People's Attitudes About Gender in Families") +
  labs(caption = "Pepin & Cotter \nData source: Monitoring the Future Surveys") +
  theme_minimal() +
  theme(legend.position    = "top", 
        legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52"),
        strip.text.x       = element_text(face  = "bold"),
        plot.caption       = element_text(color = "#605A52", size = 6),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

fig5

ggsave("figures/fig5.png", fig5, width = 8, height = 5, dpi = 300)

#####################################################################################
# Figure 6. Young Adults' Gender Attitudes -- by religiosity

## Averages
fig6a <- figdata_svy %>%
  filter(sphere != "Employed Mothers") %>%
  group_by(year, religion, sphere, variable, val) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fig6b <- figdata_svy %>%
  filter(sphere == "Employed Mothers" & year <= 2017) %>%
  group_by(year, religion, sphere, variable, val) %>%
  summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

fig6d <- rbind(fig6a, fig6b)
remove(fig6a, fig6b)

write.csv(fig6d, "data/dol_Figure 6.csv")

## Figure 6
fam_names <- c("hdecide" = "Disagree husband makes all family decisions", "home" = "Disagree woman takes care of home")

fig6 <- ggplot(subset(fig6d, val == "Feminist" & sphere == "Family"),
               aes(x = year, y = prop,
                   ymin = prop_low, ymax = prop_upp,
                   color = religion, group = religion)) +
  facet_wrap( ~ variable, labeller = as_labeller(fam_names)) +
  geom_point(alpha = .4) +
  geom_smooth(method = "loess", span = 0.2, fill = "lightgrey") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = c(1976, 1996, 2018), label = c("'76", "'96", "'18")) +
  scale_colour_manual(name="",
                      labels=c("Never", "Rarely", "Monthly", "Weekly"),
                      values=c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")) +
  ggtitle("Young People's Attitudes About Gender in Families \nBy Religious Service Attendance") +
  labs(caption = "Pepin & Cotter \nData source: Monitoring the Future Surveys") +
  theme_minimal() +
  theme(legend.position    = "top", 
        legend.title       = element_blank(),
        legend.text        = element_text(color = "#605A52"),
        strip.text.x       = element_text(face  = "bold"),
        plot.caption       = element_text(color = "#605A52", size = 6),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

fig6

ggsave("figures/fig6.png", fig6, width = 8, height = 5, dpi = 300)