## Mikael Poul Johannesson
## 2017

## Start Matter ------------------------------------------------------

library(here)
library(tidyverse)
library(ranger)
library(broom)
library(knitr)
library(stargazer)

if (!require(wiggle)) {
  devtools::install_github("mikajoh/wiggle")
}

set.seed(2016)

## Get Data ----------------------------------------------------------

## Prepared data from NCP wave 7.
## Md5sum: 35e6586fefe4f2968bd7e540e6866cd9
## tools::md5sum(here("data", "wiggle.csv"))
wiggle_raw <- read.csv(
  here("data", "wiggle.csv"),
  stringsAsFactors = FALSE
)

## We want the value labels in particular order for the figs.
wiggle <-
  wiggle_raw %>%
  mutate(
    treat_majority = lvls_reorder(treat_majority, c(4, 3:1)),
    treat_turnout = lvls_reorder(treat_turnout, c(5, 4:1)),
    treat_outcome = fct_rev(treat_outcome),
    treat_outcome_match = lvls_reorder(treat_outcome_match, c(2, 1, 3))
  )


## Descriptive Statistics --------------------------------------------

tbl_dscr <-
  wiggle %>%
  dplyr::select(rsp_age, rsp_gender, rsp_edu, rsp_party, rsp_eu) %>%
  mutate(
    rsp_age = case_when(
      rsp_age == 1 ~ "18-27 yrs",
      rsp_age == 2 ~ "26-35 yrs",
      rsp_age == 3 ~ "36-45 yrs",
      rsp_age == 4 ~ "46-55 yrs",
      rsp_age == 5 ~ "56-65 yrs",
      rsp_age == 6 ~ "66-75 yrs",
      rsp_age == 7 ~ "75+ yrs"
    )
  ) %>%
  gather(var, val) %>%
  mutate(
    var = case_when(
      var == "rsp_age"    ~ "Age",
      var == "rsp_gender" ~ "Gender",
      var == "rsp_edu"    ~ "Education",
      var == "rsp_party"  ~ "Prefered party",
      var == "rsp_eu"     ~ "EU preference"
    )
  ) %>%
  filter(!is.na(val)) %>%
  group_by(var) %>% mutate(n_tot = n()) %>% ungroup() %>%
  group_by(var, val) %>%
  summarize(
    n = n(),
    prop = round(n / n_tot[1], 2) * 100
  ) %>%
  ungroup() %>%
  mutate(
    var = lvls_reorder(factor(var), c(1, 4, 2, 3, 5)),
    val = lvls_reorder(
      factor(val),
      c(1:7, 20, 13, 19, 16, 22, 23, 17, 9, 15, 10, 18, 11, 21, 8, 14, 12)
    )
  ) %>%
  arrange(var, val) %>%
  rename_(
    "Variable" = "var",
    "Value" = "val",
    "N obs" = "n",
    "% obs" = "prop"
  ) 

writeLines(
  knitr::kable(tbl_dscr, "latex", booktabs = TRUE), 
  here("output", "tbls", "tbl_dscr.txt")
)

## Treatment assignment ----------------------------------------------

tbl_treat <-
  wiggle %>%
  dplyr::select(treat_turnout, treat_majority, treat_outcome, treat_outcome_match) %>%
  gather(var, val) %>%
  mutate(
    var = case_when(
      var == "treat_turnout"       ~ "Turnout",
      var == "treat_majority"      ~ "Majority",
      var == "treat_outcome"       ~ "Outcome",
      var == "treat_outcome_match" ~ "Outcome * EU preference"
    )
  ) %>%
  filter(!is.na(val)) %>%
  group_by(var) %>% mutate(n_tot = n()) %>% ungroup() %>%
  group_by(var, val) %>%
  summarize(
    n = n(),
    prop = round(n / n_tot[1], 2) * 100
  ) %>%
  ungroup() %>%
  mutate(
    var = lvls_reorder(factor(var), c(4, 1, 2, 3)),
    val = lvls_reorder(
      factor(val),
      c(11, 1:7, 8, 10, 12, 9)
    )
  ) %>%
  arrange(var, val) %>%
  rename_(
    "Treatment" = "var",
    "Assigned Value" = "val",
    "N obs" = "n",
    "% obs" = "prop"
  ) 

writeLines(
  knitr::kable(tbl_treat, "latex", booktabs = TRUE), 
  here("output", "tbls", "tbl_treat.txt")
)

## Post-treatment distribution ---------------------------------------

tbl_post <-
  wiggle %>%
  dplyr::select(post) %>%
  gather(Variable, Value, post) %>%
  count(Variable, Value) %>%
  mutate(
    Variable = "Should referendum be followed",
    Value = case_when(
      Value == 1 ~ "Should follow referendum",
      Value == 0 ~ "Should not follow referendum"
    ),
    prop = round(n / sum(n), 2) * 100
  ) %>%
  rename_(
    "Post treatment" = "Variable",
    "N obs" = "n",
    "% obs" = "prop"
  )

writeLines(
  knitr::kable(tbl_post, "latex", booktabs = TRUE), 
  here("output", "tbls", "tbl_post.txt")
)

## List with verbatim wording of treatment ---------------------------

tbl_wording <-
  wiggle %>%
  dplyr::select(treat_turnout, treat_majority, treat_outcome, treat_text) %>%
  filter(!duplicated(treat_text)) %>%
  arrange(treat_turnout, treat_majority, treat_outcome) %>%
  rename_(
    "Assigned Turnout"  = "treat_turnout",
    "Assigned Majority" = "treat_majority",
    "Assigned Outcome"  = "treat_outcome",
    "Exact Wording"     = "treat_text"
  )

writeLines(
  knitr::kable(tbl_wording, "latex", booktabs = TRUE), 
  here("output", "tbls", "tbl_wording.txt")
)

## Table for logistic regression -------------------------------------

## Logitic regression model fit.
load(here("output", "wgl_glm_fit.RData"))

tbl_logit_fit <- stargazer(
  wgl_fit,
  style = "ajps",
  dep.var.labels = "Should follow referendum",
  covariate.labels = c(
    "Majority of 70%",
    "Majority of 55%",
    "Majority of 51%",
    "Turnout of 85%",
    "Turnout of 53%",
    "Turnout of 47%",
    "Turnout of 35%",
    "Favourable outcome",
    "Unfavourable outcome"
  ),
  ci = TRUE
)

writeLines(
  tbl_logit_fit,
  here("output", "tbls", "tbl_logit_fit.txt")
)

## Figure 2 in table format ------------------------------------------

## We only want to plot the predictions for when the respondent would
## know all the referendum attributes.
wgl_pred_data <- expand.grid(
  treat_majority = levels(wiggle$treat_majority),
  treat_turnout = levels(wiggle$treat_turnout),
  treat_outcome_match = levels(wiggle$treat_outcome_match)
)
wgl_preds <-
  wgl_fit %>%
  augment(newdata = wgl_pred_data, type.predict = "response") %>%
  filter_at(vars(matches("treat_")), all_vars(. != "Not shown")) %>%
  mutate(
    treat_majority = case_when(
      treat_majority == "51%" ~ "Majority of\n51%",
      treat_majority == "55%" ~ "Majority of\n55%",
      treat_majority == "70%" ~ "Majority of\n70%"),
    treat_majority = fct_rev(treat_majority),
    treat_turnout = fct_rev(treat_turnout),
    treat_outcome_match = fct_rev(treat_outcome_match)
  )

tbl_preds <-
  wgl_preds %>%
  mutate(
    treat_majority = gsub("Majority of\n", "", treat_majority),
    .fitted = round(.fitted, 2) * 100,
    .se.fit = round(.se.fit, 3) * 100
  ) %>%
  rename_(
    "Majority" = "treat_majority",
    "Turnout" = "treat_turnout",
    "Outcome * EU Preference" = "treat_outcome_match",
    "Pred. Prob (%)" = ".fitted",
    "Std. Error (%)" = ".se.fit"
  )

writeLines(
  knitr::kable(tbl_preds, "latex", booktabs = TRUE),
  here("output", "tbls", "tbl_preds.txt")
)

## Interaction effects -----------------------------------------------

plot_int <- function(res_main, var) {  
  res_main %>%
    ggplot(aes(x = estimate, y = value)) +
    facet_grid(
      paste0("treatment ~ ", var),
      scales = "free_y",
      space = "free_y") +
    geom_errorbarh(
      aes(xmin = estimate - (2 * std_error),
          xmax = estimate + (2 * std_error)),
      height = 0) +
    geom_point() +
    geom_vline(aes(xintercept = 0), linetype = "dotted") +
    scale_x_continuous(
      limits = c(-.6, .6),
      breaks = round(seq(-.6, .6, .2), 2),
      expand = c(0, 0),
      labels = function(x) x * 100) +
    scale_y_discrete(labels = function(x) parse(text = as.character(x))) +
    labs(
      x = "Marginal effect, should follow referendum (%)",
      y = "Referendum attributes") +
    theme_m() +
    theme(plot.margin = unit(c(2, 2, 2, 2), "mm"))
}

## Main effects by Age -----------------------------------------------
res_main_age <-
  wiggle %>%
  amce(
    post, treat_outcome, treat_majority, treat_turnout,
    subgroup = "rsp_age_cat") %>%
  add_labels()

fig_apx_int_age <- plot_int(res_main_age, "rsp_age_cat")

ggsave(
  here("output", "figs", "fig_apx_int_age.pdf"),
  plot = fig_apx_int_age,
  width = 8, height = 2.75
)

ggsave(
  here("output", "figs", "pngs", "fig_apx_int_age.png"),
  plot = fig_apx_int_age,
  width = 8, height = 2.75
)


## Main effects by Age -----------------------------------------------
res_main_gen <-
  wiggle %>%
  amce(
    post, treat_outcome, treat_majority, treat_turnout,
    subgroup = "rsp_gender") %>%
  add_labels()

fig_apx_int_gen <- plot_int(res_main_gen, "rsp_gender")

ggsave(
  here("output", "figs", "fig_apx_int_gen.pdf"),
  plot = fig_apx_int_gen,
  width = 6, height = 2.75
)

ggsave(
  here("output", "figs", "pngs", "fig_apx_int_gen.png"),
  plot = fig_apx_int_gen,
  width = 6, height = 2.75
)


## Main effects by Education -----------------------------------------

res_main_edu <-
  wiggle %>%
  amce(
    post, treat_outcome, treat_majority, treat_turnout,
    subgroup = "rsp_edu") %>%
  add_labels()

fig_apx_int_edu <- plot_int(res_main_edu, "rsp_edu")

ggsave(
  here("output", "figs", "fig_apx_int_edu.pdf"),
  plot = fig_apx_int_edu,
  width = 6, height = 2.75
)

ggsave(
  here("output", "figs", "pngs", "fig_apx_int_edu.png"),
  plot = fig_apx_int_edu,
  width = 6, height = 2.75
)

## Main effects by Preferences ---------------------------------------

res_main_eu <-
  wiggle %>%
  amce(
    post, treat_outcome, treat_majority, treat_turnout,
    subgroup = "rsp_eu") %>%
  add_labels()

fig_apx_int_eu <- plot_int(res_main_eu, "rsp_eu")

ggsave(
  here("output", "figs", "fig_apx_int_eu.pdf"),
  plot = fig_apx_int_eu,
  width = 8, height = 2.75
)

ggsave(
  here("output", "figs", "pngs", "fig_apx_int_eu.png"),
  plot = fig_apx_int_eu,
  width = 8, height = 2.75
)

## Main effects by Majority ------------------------------------------

res_main_maj <-
  wiggle %>%
  amce(
    post, treat_outcome, treat_turnout,
    subgroup = "treat_majority") %>%
  add_labels() %>%
  mutate(treat_majority = paste0("Majority\n", treat_majority))

fig_apx_int_maj <- plot_int(res_main_maj, "treat_majority")

ggsave(
  here("output", "figs", "fig_apx_int_maj.pdf"),
  plot = fig_apx_int_maj,
  width = 10, height = 2.75
)
ggsave(
  here("output", "figs", "pngs", "fig_apx_int_maj.png"),
  plot = fig_apx_int_maj,
  width = 10, height = 2.75
)

## Main effects by Turnout -------------------------------------------

res_main_turn <-
  wiggle %>%
  amce(
    post, treat_outcome, treat_majority,
    subgroup = "treat_turnout") %>%
  add_labels() %>%
  mutate(treat_turnout = paste0("Turnout\n", treat_turnout))

fig_apx_int_turn <- plot_int(res_main_turn, "treat_turnout")

ggsave(
  here("output", "figs", "fig_apx_int_turn.pdf"),
  plot = fig_apx_int_turn,
  width = 10, height = 2.75
)
ggsave(
  here("output", "figs", "pngs", "fig_apx_int_turn.png"),
  plot = fig_apx_int_turn,
  width = 10, height = 2.75
)


## Alternative model for figure using Random Forest ------------------

wgl_rf_fit <- ranger(
  factor(post) ~ treat_majority + treat_turnout + treat_outcome_match,
  data = wiggle %>%
    select(post, treat_majority, treat_turnout, treat_outcome_match) %>%
    na.omit() %>%
    as.data.frame(),
  keep.inbag = TRUE,
  probability = TRUE
)

## We only want to plot the predictions for when the respondent would
## know all the referendum attributes.
wgl_pred_data <- expand.grid(
  treat_majority = levels(wiggle$treat_majority),
  treat_turnout = levels(wiggle$treat_turnout),
  treat_outcome_match = levels(wiggle$treat_outcome_match)
)

## Get predictions
wgl_rf_preds_raw <- predict(
  object = wgl_rf_fit,
  data = wgl_pred_data,
  type = "se"
)

wgl_rf_preds <-
  wgl_pred_data %>%
  mutate(
    .fitted = wgl_rf_preds_raw$predictions[, 2],
    .se.fit = wgl_rf_preds_raw$se[, 2]
  ) %>%
  unnest() %>%
  filter_at(vars(matches("treat_")), all_vars(. != "Not shown")) %>%
  mutate(
    treat_majority = case_when(
      treat_majority == "51%" ~ "Majority of\n51%",
      treat_majority == "55%" ~ "Majority of\n55%",
      treat_majority == "70%" ~ "Majority of\n70%"),
    treat_majority = fct_rev(treat_majority),
    treat_turnout = fct_rev(treat_turnout),
    treat_outcome_match = fct_rev(treat_outcome_match)
  )

## Plot figure.
fig_rf_pred <-
  wgl_rf_preds %>%
  ggplot(
    aes(
      x = .fitted, y = treat_turnout,
      xmin = .fitted - (2 * .se.fit),
      xmax = .fitted + (2 * .se.fit))
  ) +
  facet_grid(treat_majority ~ treat_outcome_match) +
  geom_errorbarh(height = 0) +
  geom_point() +
  geom_vline(aes(xintercept = .5), linetype = "dotted") +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = round(seq(0, 1, .1), 2),
    expand = c(0, 0),
    labels = function(x) x * 100) +
  labs(
    x = "Expected proportion that would follow referendum (%)",
    y = "Turnout") +
  theme_m() +
  theme(strip.text.y = element_text(angle = 0, face = "bold.italic"))

ggsave(
  here("output", "figs", "fig_apx_rf_preds.pdf"),
  plot = fig_rf_pred,
  width = 8, height = 2.75
)

ggsave(
  here("output", "figs", "pngs", "fig_apx_rf_preds.png"),
  plot = fig_rf_pred,
  width = 8, height = 2.75
)
