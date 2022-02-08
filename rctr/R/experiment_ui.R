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
      column(9,
             p("Setup"),
             verbatimTextOutput("experimentSummary"))
    ),
    hr(),
    fluidRow(
      column(5,
             selectInput("impactVariable",
                         "Select Impact Variable",
                         choices = impact_variables,
                         multiple = F),
             actionButton("loadImpact", "Measure impact"),
             p("Results"),
             verbatimTextOutput("impactSummary")),
      column(7,
             tabsetPanel(
               tabPanel(
                 "Time Series",
                 plotOutput("timeseriesPlot")
               ),
               tabPanel(
                 "Cumulative Impact",
                 plotOutput("cumulativePlot")
               ),
               tabPanel(
                 "Population",
                 plotOutput("populationPlot")
               )
             )
      )
    )
  )
}
