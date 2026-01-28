#-------------------------------------------------------------------------------
# GENDER ATTITUDES PROJECT
# GA_05_work-family arrangements.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------

# Set-up data ------------------------------------------------------------------

fig.wfa <- mtf.wfa |>
  mutate(id = paste0(year, "_", row_number()),
    # DESIRABLE recode _desired
    across(
      c(hfw0, hfwh, hfwf, hhwh, hhwf, h0wf),
      ~ factor(
        case_when(
          .x == "DESIRABLE" ~ "Desired",
          .x != "DESIRABLE" ~ "Not desired",
          TRUE              ~ NA_character_),   # preserves NA
        levels = c("Desired", "Not desired")),
      .names = "{.col}_desired"),
    # ACCEPTABLE recode _acceptable
    across(
      c(hfw0, hfwh, hfwf, hhwh, hhwf, h0wf),
      ~ factor(
        case_when(
          .x == "ACCEPTABLE" ~ "Acceptable",
          .x != "ACCEPTABLE" ~ "Not acceptable",
          TRUE               ~ NA_character_),  # preserves NA
        levels = c("Acceptable", "Not acceptable")),
      .names = "{.col}_acceptable")) |>
  pivot_longer(
    cols = matches("_(desired|acceptable)$"),
    names_to = c("variable", "class"),
    names_pattern = "(.*)_(desired|acceptable)$",
    values_to = "val",
    values_drop_na = FALSE) 
# |>
#   pivot_wider(
#     names_from = class,
#     values_from = val) |> 
#   mutate(
#     desired    = fct_drop(desired),
#     acceptable = fct_drop(acceptable)) |>
#   select(-id)

# turn into survey data
fig.wfa_svy <- fig.wfa |>
  as_survey_design(id = NULL,
                   weights = weight)

## Averages
wfa.df <- fig.wfa_svy |>
  filter(!is.na(val)) |>
  group_by(year, variable, class, val) |>
  summarize(prop = survey_mean(vartype = "ci"))

### Readable labels for variable
wfa.df$variable <- as.factor(wfa.df$variable)
levels(wfa.df$variable)[levels(wfa.df$variable)=="hfw0"] <- "Husband FT; Wife at home"
levels(wfa.df$variable)[levels(wfa.df$variable)=="hfwh"] <- "Husband FT; Wife PT"
levels(wfa.df$variable)[levels(wfa.df$variable)=="hfwf"] <- "Both work FT"
levels(wfa.df$variable)[levels(wfa.df$variable)=="hhwh"] <- "Both work PT"
levels(wfa.df$variable)[levels(wfa.df$variable)=="hhwf"] <- "Husband PT; Wife FT"
levels(wfa.df$variable)[levels(wfa.df$variable)=="h0wf"] <- "Husband at home; Wife FT"

wfa.df$variable <- ordered(wfa.df$variable,
                          levels = c(
                            "Husband FT; Wife at home", 
                            "Husband FT; Wife PT", 
                            "Both work FT", 
                            "Both work PT", 
                            "Husband PT; Wife FT", 
                            "Husband at home; Wife FT"))

### Create type variable
wfa.df <- wfa.df %>%
  mutate(
    type = case_when(
      variable == "Husband FT; Wife at home" |
      variable == "Husband FT; Wife PT"      ~ "Traditional",
      variable == "Both work FT"             |
      variable == "Both work PT"             ~ "Matched",
      variable == "Husband at home; Wife FT" |
      variable == "Husband PT; Wife FT"      ~ "Gender Atypical",     
      TRUE                                   ~  NA_character_ ))

wfa.df$type <- ordered(wfa.df$type, 
                        levels = c("Traditional", "Matched", "Gender Atypical"))

### Create values for beginning and end points
wfa.df <- wfa.df |>
  group_by(variable, val) |>
  dplyr::mutate(
    last_value  = dplyr::last(scales::percent(prop, accuracy =  1)),
    first_value = dplyr::first(scales::percent(prop, accuracy =  1))) 

saveRDS(wfa.df, file.path(outDir,"mtf_wfa.rds"))


## figures ---------------------------------------------------------------------

### Desired
wfa.df |>
  filter(val == "Desired") |>
  ggplot(aes(year, prop, color = variable, ymin = prop_low, ymax = prop_upp)) +
  facet_wrap("type") +
  geom_point(alpha = .1) +
  geom_textsmooth(aes(label = variable), text_smoothing = 30, 
                  method = "loess", formula = y ~ x,
                  size = 4, linewidth = 1.15) +
  directlabels::geom_dl(aes(label=last_value), method = list('last.points', cex = 1)) + 
  directlabels::geom_dl(aes(label=first_value), method = list('first.points', cex = 1)) + 
  geom_vline(xintercept = c(1976, 1995, 2014),     color = "grey90") +
  coord_cartesian(xlim = c(1970, 2020), # This extends the x-axis to make room for the value labels.
                  ylim = c(0, .5),
                  clip = 'off') +   # This keeps the labels from disappearing
  scale_x_continuous(name = "", 
                     breaks   = c(1976, 1995, 2014), 
                     label    = c("'76", "'95", "'14"), 
                     position = "bottom") +
  scale_y_continuous(labels   = scales::percent_format(accuracy = 1), breaks = NULL) +
  theme_minimal(22) +
  theme(
    legend.title     = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x     = element_text(face="bold"),
    legend.position  = "none",
    plot.margin      = margin(b = 0)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_colour_manual(values=c("#E74C3C", "#3498DB", "#18BC9C", "#F39C12", "#e7298a", "#7570b3"))


### Acceptable
wfa.df |>
  filter(val == "Acceptable") |>
  ggplot(aes(year, prop, color = variable, ymin = prop_low, ymax = prop_upp)) +
  facet_wrap("type") +
  geom_point(alpha = .1) +
  geom_textsmooth(aes(label = variable), text_smoothing = 30, 
                  method = "loess", formula = y ~ x,
                  size = 4, linewidth = 1.15) +
  directlabels::geom_dl(aes(label=last_value), method = list('last.points', cex = 1)) + 
  directlabels::geom_dl(aes(label=first_value), method = list('first.points', cex = 1)) + 
  geom_vline(xintercept = c(1976, 1995, 2014),     color = "grey90") +
  coord_cartesian(xlim = c(1970, 2020), # This extends the x-axis to make room for the value labels.
                  ylim = c(0, .6),
                  clip = 'off') +   # This keeps the labels from disappearing
  scale_x_continuous(name = "", 
                     breaks   = c(1976, 1995, 2014), 
                     label    = c("'76", "'95", "'14"), 
                     position = "bottom") +
  scale_y_continuous(labels   = scales::percent_format(accuracy = 1), breaks = NULL) +
  theme_minimal(22) +
  theme(
    legend.title     = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x     = element_text(face="bold"),
    legend.position  = "none",
    plot.margin      = margin(b = 0)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_colour_manual(values=c("#E74C3C", "#3498DB", "#18BC9C", "#F39C12", "#e7298a", "#7570b3"))

