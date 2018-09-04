## Mikael Poul Johannesson
## 2017

## Start Matter ------------------------------------------------------

library(here)
library(tidyverse)
library(broom)
library(knitr)

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

## Figure 1 ----------------------------------------------------------

## Estimate the AMCEs.
res_main <-
  wiggle %>%
  amce(post, treat_outcome, treat_majority, treat_turnout) %>%
  add_labels()

## Plot the AMCEs
fig_amce <-
  res_main %>%
  ggplot(aes(x = estimate, y = value)) +
  facet_grid(
    treatment ~ .,
    scales = "free_y",
    space = "free_y") +
  geom_errorbarh(
    aes(xmin = estimate - (2 * std_error),
        xmax = estimate + (2 * std_error)),
    height = 0) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  scale_x_continuous(
    limits = c(-.5, .5),
    breaks = round(seq(-.5, .5, .1), 2),
    expand = c(0, 0),
    labels = function(x) x * 100) +
  scale_y_discrete(labels = function(x) parse(text = as.character(x))) +
  labs(
    x = "Marginal effect, should follow referendum (%)",
    y = "Referendum attributes") +
  theme_m() +
  theme(plot.margin = unit(c(2, 2, 2, 2), "mm"))
fig_amce

ggsave(
  here("output", "figs", "fig_01_main.pdf"),
  plot = fig_amce,
  width = 5.5, height = 2.75
)

ggsave(
  here("output", "figs", "pngs", "fig_01_main.png"),
  plot = fig_amce,
  width = 5.5, height = 2.75
)

## Figure 2 ----------------------------------------------------------

## Fit regression model.
wgl_fit <- glm(
  post ~ treat_majority + treat_turnout + treat_outcome_match,
  data = wiggle,
  family = binomial(link = "logit")
)
save(wgl_fit, file = here("output", "wgl_glm_fit.RData"))

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

## Plot figure.
fig_pred <-
  wgl_preds %>%
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
fig_pred

ggsave(
  here("output", "figs", "fig_02_preds.pdf"),
  plot = fig_pred,
  width = 8, height = 2.75
)

ggsave(
  here("output", "figs", "pngs", "fig_02_preds.png"),
  plot = fig_pred,
  width = 8, height = 2.75
)

## Figure 3 ----------------------------------------------------------

## Estimate the AMCEs.
res_outcome <-
  wiggle %>%
  amce(
    post, treat_majority, treat_turnout,
    subgroup = "treat_outcome_match"
  ) %>%
  add_labels() %>%
  mutate(treat_outcome_match = fct_rev(treat_outcome_match))

## Plot the AMCEs
fig_outcome_amce <-
  res_outcome %>%
  ggplot(aes(x = estimate, y = value)) +
  facet_grid(
    treatment ~ treat_outcome_match,
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
  scale_y_discrete(
    labels = function(x) parse(text = as.character(x))
  ) +
  labs(
    x = "Marginal effect, should follow referendum (%)",
    y = "Referendum attributes") +
  theme_m() +
  theme(plot.margin = unit(c(2, 2, 2, 2), "mm"))
fig_outcome_amce

ggsave(
  here("output", "figs", "fig_03_outcome.pdf"),
  plot = fig_outcome_amce,
  width = 8, height = 2.75
)

ggsave(
  here("output", "figs", "pngs", "fig_03_outcome.png"),
  plot = fig_outcome_amce,
  width = 8, height = 2.75
)



## Table 1 -----------------------------------------------------------

tbl_treat <-
  wiggle %>%
  filter(!is.na(post), !is.na(treat_majority)) %>%
  mutate(n_total = n()) %>%
  gather(
    treatment, value,
    treat_outcome, treat_majority, treat_turnout
  ) %>%
  filter(!is.na(value)) %>%
  left_join(treat_labs()) %>%
  mutate(
    treatment = fct_inorder(treatment),
    treatment_label = fct_inorder(treatment_label),
    value = fct_inorder(value)
  ) %>%
  group_by(treatment_label, value) %>%
  summarize(n = n(),
            prop = round(n / n_total[1], 2)) %>%
  ungroup() %>%
  arrange(treatment_label, value) %>%
  rename_(
    "Treatment" = "treatment_label",
    "Value" = "value",
    "N obs" = "n",
    "% obs" = "prop"
  )

tbl_treat %>%
  knitr::kable("latex", booktabs = TRUE)

wiggle %>%
  filter(
    treat_outcome == "Not shown",
    treat_turnout == "Not shown",
    treat_majority == "Not shown",
    !is.na(post)
  ) %>%
  summarize(
    est = mean(post),
    n = n()
  )
