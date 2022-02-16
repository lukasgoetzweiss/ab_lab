
# UI ----

#' create_experiment_ui
#'
#' UI for experiment creation survey
#'
#' @export
create_experiment_ui = function(rv, impact_variables){
  mainPanel(
    id = "create_experiment_tabset",
    textInput(
      "newExperimentName",
      "Experiment Name: "
    ),
    selectInput(
      "newExperimentTreatment",
      label = "Select Treatment",
      choices = rv$treatment[, unique(name)]
    ),
    selectInput(
      "newExperimentControlTreatment",
      "Select Control Treatment",
      choices = rv$treatment[, unique(name)]
    ),
    selectInput(
      "newExperimentAudience",
      "Select Audience",
      choices = rv$audience[, unique(name)],
      multiple = F
    ),
    selectInput(
      "newExperimentVariable",
      "Select Main Impact Variable",
      choices = impact_variables
    ),
    dateRangeInput(
      "newExperimentDateRange",
      "Select dates",
      start = Sys.Date() + days(1),
      end = Sys.Date() + days(8),
      min = Sys.Date() + days(1)
    ),
    numericInput(
      "newExperimentControlPercentage",
      "Control Percentage",
      50,
      min = 0,
      max = 100,
      step = 1
    ),
    actionButton("newExperimentSave", "Save")
  )
}
