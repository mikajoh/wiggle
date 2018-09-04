#' Labels to use in the figures.
#'
#' @importFrom tibble tribble
#' 
#' @export
treat_labs <- function() {
  tribble(
    ~treatment,              ~treatment_label,
    "rsp_eu",                "Preference on EU membership",
    "treat_outcome",         "Outcome",
    "treat_outcome_match",   "Outcome favourability",
    "outcome_agree_outcome", "Outcome, Agree with outcome",
    "treat_majority",        "Size of majority",
    "treat_turnout",         "Size of turnout",
    "rsp_gender",            "Gender",
    "rsp_edu",               "Education",
    "rsp_city",              "City",
    "rsp_polint",            "Interest in politics",
    "rsp_nfc",               "Need for cognition",
    "rsp_nte",               "Need to evaluate",
    "post",                  "Should referendum be followed"
  )
}


#' Function for adding a row with treatment labels for the figures.
#' 
#' @param amce Estimates from \code{amce()}.
#' @param labels The dataframe containing the labels. It expects a
#'   column \code{treatment} corresponding to the treatment variables
#'   and a column \code{treatment_label} with their labels. Defaults
#'   to \code{wiggle::treat_labs()}, which is included in the
#'   compendium.
#' 
#' @export
add_labels <- function(amce, labels = wiggle::treat_labs()) {

  amce <- amce[rev(order(amce$value)), ]
  amce$value <- paste0("plain('", as.character(amce$value), "')")
  amce$value <- factor(amce$value, levels = unique(amce$value))
  add_data <- labels[labels$treatment %in% amce$treatment, ]
  names(add_data)[2] <- "value"
  add_data$value <- gsub("\n", " ", as.character(add_data$value))
  add_data$value <- paste0("bold('", add_data$value, "')")
  for (var in c("value_order", "estimate", "std_error", "statistic", "p_value")) {
    add_data[[var]] <- NA
  }
  add_data$value_order <- -Inf
  if (any(!(names(amce) %in% names(add_data)))) {
    for (.c in names(amce)[!(names(amce) %in% names(add_data))]) {
      add_data[[.c]] <- amce[[.c]][1]
    }
  }
  if (any(!(names(add_data) %in% names(amce)))) {
    for (.c in names(add_data)[!(names(add_data) %in% names(amce))]) {
      add_data <- add_data[, -c(which(names(add_data) == .c))]
    }
  }
  amce <- rbind(amce, add_data)
  amce <- amce[order(amce$treatment, amce$value_order), ]
  return(amce)
}
