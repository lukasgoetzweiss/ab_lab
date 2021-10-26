#' Experiment UI
#'
#' Main audience panel UI
#'
#' @import shiny DT
#' @export
#'
#' @examples
experiment_ui = function(experiment, impact_variables){
  tabPanel(
    "Experiment", value = "experiment",
    fluidRow(
      column(3,
             selectInput(inputId = "selectedExperiment",
                         label = "Select Experiment",
                         choices = experiment[, name],
                         multiple = F),
             actionButton("createExperiment",
                          "Create new experiment",
                          icon = icon("plus"))
      ),
      column(3,
             selectInput("impactVariable",
                         "Select Impact Variable",
                         choices = impact_variables,
                         multiple = F)),
      column(4,
             actionButton("loadImpact", "Measure impact")),
      column(2, p(""))
    ),
    hr(),
    fluidRow(
      column(5,
             p("Setup"),
             verbatimTextOutput("experimentSummary"),
             p("Results"),
             verbatimTextOutput("impactSummary")),
      column(7,
             plotOutput("impactPlot"))
    )
  )
}
