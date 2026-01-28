#-------------------------------------------------------------------------------
# GENDER ATTITUDES PROJECT
# GA_05_work-family arrangements.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------

# Set-up data ------------------------------------------------------------------

wfadata <- mtf.wfa |>
  mutate(
    hfw0_d = case_when(
      hfw0   == "DESIRABLE"    ~ "Desired",
      hfw0   != "DESIRABLE"    ~ "Not desired",
      TRUE                     ~  NA_character_),
    hfwh_d = case_when(
      hfwh   == "DESIRABLE"    ~ "Desired",
      hfwh   != "DESIRABLE"    ~ "Not desired",
      TRUE                     ~  NA_character_),
    hfwf_d = case_when(
      hfwf   == "DESIRABLE"    ~ "Desired",
      hfwf   != "DESIRABLE"    ~ "Not desired",
      TRUE                     ~  NA_character_),
    hhwh_d = case_when(
      hhwh   == "DESIRABLE"    ~ "Desired",
      hhwh   != "DESIRABLE"    ~ "Not desired",
      TRUE                     ~  NA_character_),
    hhwf_d = case_when(
      hhwf   == "DESIRABLE"    ~ "Desired",
      hhwf   != "DESIRABLE"    ~ "Not desired",
      TRUE                     ~  NA_character_),
    h0wf_d = case_when(
      h0wf   == "DESIRABLE"    ~ "Desired",
      h0wf   != "DESIRABLE"    ~ "Not desired",
      TRUE                     ~  NA_character_))

cat_vars <- c( "hfw0_d", "hfwh_d", "hfwf_d", "hhwh_d", "hhwf_d", "h0wf_d")

wfadata <- wfadata |>
  modify_at(cat_vars, as.factor)

fig.wfa <- wfadata |> 
  pivot_longer( cols = c(hfw0_d, hfwh_d, hfwf_d, hhwh_d, hhwf_d, h0wf_d), 
                names_to = "variable", 
                values_to = "val", 
                values_transform = list(val = as.character)) |>
  filter(!is.na(val))

fig.wfa_svy <- fig.wfa |>
  as_survey_design(id = NULL,
                   weights = weight)

## Averages
wfa.d <- fig.wfa_svy |>
  group_by(year, variable, val) |>
  summarize(prop = survey_mean(vartype = "ci"))

### Readable labels for variable
wfa.d$variable <- as.factor(wfa.d$variable)
levels(wfa.d$variable)[levels(wfa.d$variable)=="hfw0_d"] <- "Husband full-time; Wife at home"
levels(wfa.d$variable)[levels(wfa.d$variable)=="hfwh_d"] <- "Husband full-time; Wife part-time"
levels(wfa.d$variable)[levels(wfa.d$variable)=="hfwf_d"] <- "Both work full-time"
levels(wfa.d$variable)[levels(wfa.d$variable)=="hhwh_d"] <- "Both work part-time"
levels(wfa.d$variable)[levels(wfa.d$variable)=="hhwf_d"] <- "Husband part-time; Wife full-time"
levels(wfa.d$variable)[levels(wfa.d$variable)=="h0wf_d"] <- "Husband at home; Wife full-time"

wfa.d$variable <- ordered(wfa.d$variable,
                          levels = c(
                            "Husband full-time; Wife at home", 
                            "Husband full-time; Wife part-time", 
                            "Both work full-time", 
                            "Both work part-time", 
                            "Husband part-time; Wife full-time", 
                            "Husband at home; Wife full-time"))

### Create type variable
wfa.d <- wfa.d %>%
  mutate(
    type = case_when(
      variable == "Husband full-time; Wife at home" |
      variable == "Husband full-time; Wife part-time"    ~ "TRADITIONAL",
      variable == "Both work full-time"             |
      variable == "Both work part-time"                  ~ "MATCHED",
      variable == "Husband at home; Wife full-time" |
      variable == "Husband part-time; Wife full-time"    ~ "GENDER ATYPICAL",     
      TRUE                                               ~  NA_character_ ))

### Create values for beginning and end points
wfa.d <- wfa.d |>
  group_by(variable, val) |>
  dplyr::mutate(
    last_value  = dplyr::last(scales::percent(prop, accuracy =  1)),
    first_value = dplyr::first(scales::percent(prop, accuracy =  1))) 

saveRDS(wfa.d, file.path(outDir,"mtf_wfa.rds"))


## figure ----------------------------------------------------------------------

wfa.d |>
  filter(val == "Desired") |>
  ggplot(aes(year, prop, color = variable, ymin = prop_low, ymax = prop_upp)) +
  facet_wrap( ~ val, ncol = 1) +
  geom_linerange(show.legend=FALSE, color = "grey") +
  geom_line(size = 1) +
  ggrepel::geom_text_repel(aes(label = variable), # This plots the dol labels on the right side without overlap.
                  data           = subset(wfa.d, val == "Desired" & year == 2014), # Only plot the labels 1 time
                  segment.colour = NA,
                  nudge_x        = 2019 - subset(wfa.d, val == "Desired" &  year == 2014)$year,
                  direction      = "y",
                  hjust          = 0,
                  size           = 3) +
  ggrepel::geom_text_repel(aes(label = last_value), # This plots the 2014 proportions in line with the dol labels.
                  data           = subset(wfa.d, val == "Desired" &  year == 2014), # Only plot the labels 1 time
                  segment.colour = NA,
                  nudge_x        = 2015 - subset(wfa.d,  val == "Desired" &  year == 2014)$year,
                  direction      = "y",
                  hjust          = 0,
                  size           = 3) +
  directlabels::geom_dl(aes(label=first_value), method = list('first.points', cex = .75)) + #This plots the 1976 proportions.
  coord_cartesian(xlim = c(1976, 2040), # This extendes the x-axis to make room for the dol labels.
                  ylim = c(0, .5),
                  clip = 'off') +   # This keeps the labels from disappearing
  geom_segment(aes(x=1976,xend=2014,y=0,yend=0),   color = "grey90") +
  geom_segment(aes(x=1976,xend=2014,y=.5,yend=.5), color = "grey90") +
  geom_segment(aes(x=1976,xend=2014,y=1,yend=1),   color = "grey90") +
  geom_vline(xintercept = c(1976, 1995, 2014),     color = "grey90") +
  scale_x_continuous(name = "", 
                     breaks   = c(1976, 1995, 2014), 
                     label    = c("1976", "1995", "2014"), 
                     position = "top") +
  scale_y_continuous(labels   = scales::percent_format(accuracy = 1), breaks = NULL) +
  theme_minimal(20) +
  theme(text             = element_text(size=12),
        legend.title     = element_blank(),
        strip.text.x     = element_text(angle = 0, face="bold", hjust = 0),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.placement  = "outside", 
        plot.margin = unit(c(.25,.5,1,1), "cm")) +
  labs(x = " ", y = " ", fill = "") +
  scale_colour_manual(values=c("#E74C3C", "#3498DB", "#18BC9C", "#F39C12", "#e7298a", "#7570b3"))
