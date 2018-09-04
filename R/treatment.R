

make_profiles <- function(referendum_turnout = sample(c(TRUE, FALSE), 1),
                          referendum_majority = sample(c(TRUE, FALSE), 1),
                          outcome = sample(c("den vinnende siden", "ja-siden", "nei-siden"), 1)) {

  turnout <- paste0(sample(c("35%", "47%", "53%", "85%"), 1), " av befolkningen deltok")
  majority <- paste0(sample(c("51%", "55%", "70%"), 1), " av stemmene")
  outcome_majority <- paste0(outcome, " fikk ", ifelse(referendum_majority, majority, "flertall"))

  if (!referendum_turnout) turnout <- NULL
  if (!referendum_majority & !grepl("ja|nei", outcome)) outcome_majority <- NULL

  out <- paste0("La oss si at ", paste0(c(turnout, outcome_majority), collapse = " og at "), ".")
  if (!(referendum_turnout | referendum_majority | grepl("ja|nei", outcome))) out <- ""

  return(out)
}

## Func for making all
make_all <- function(grid) {
  all <- sapply(1:nrow(grid), function(x) {
    referendum_turnout <- grid$turnout[x] != "blank"
    referendum_majority <- grid$majority[x] != "blank"
    turnout <- paste0(grid$turnout[x], " av befolkningen deltok")
    majority <- paste0(grid$majority[x], " av stemmene")
    outcome <- grid$outcome[x]
    outcome_majority <- paste0(outcome, " fikk ", ifelse(referendum_majority, majority, "flertall"))
    if (!referendum_turnout) turnout <- NULL
    if (!referendum_majority & !grepl("ja|nei", outcome)) outcome_majority <- NULL
    out <- paste0("La oss si at ", paste0(c(turnout, outcome_majority), collapse = " og at "), ".")
    if (!(referendum_turnout | referendum_majority | grepl("ja|nei", outcome))) {
      out <- ""
    }
    return(out)
  })
  out <- cbind(grid, data.frame(id = 1:length(all), tekst = all, stringsAsFactors = FALSE))
  return(out)
}

wgl_treat_all <- function(unique_only = FALSE) {

  grid <- expand.grid(
    referendum_turnout = c(
      "blank", "blank", "blank", "blank",
      "35%", "47%", "53%", "85%"
    ),
    referendum_majority = c(
      "blank", "blank", "blank",
      "51%", "55%", "70%"
    ),
    outcome = c("den vinnende siden", "ja-siden", "nei-siden"),
    stringsAsFactors = FALSE
  )

  if (unique_only)
    grid <- grid[!duplicated(apply(grid, 1, paste, collapse = "")), ]
  
  do.call("make_all", grid)
  
}

## ## Versjon hvor bare 1/x av gruppene ikke får informasjon
## grid <- expand.grid(turnout = c("blank", "35%", "47%", "53%", "85%"),
##                     majority = c("blank", "51%", "55%", "70%"),
##                     outcome = c("den vinnende siden", "ja-siden", "nei-siden"),
##                     stringsAsFactors = FALSE)
## all_unbalanced <- make_all(grid)

## Versjon hvor 1/2 ikke får informasjon (som vi først ville ha)
grid_balanced <- expand.grid(turnout = c("blank", "blank", "blank", "blank", "35%", "47%", "53%", "85%"),
                   majority = c("blank", "blank", "blank", "51%", "55%", "70%"),
                   outcome = c("den vinnende siden", "ja-siden", "nei-siden"),
                   stringsAsFactors = FALSE)
all <- make_all(grid_balanced)
to_data_collector <- all[, c("id", "tekst")]
write.csv(to_data_collector, file = "r7padpilot.csv", row.names = FALSE)
