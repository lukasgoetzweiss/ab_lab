#' Experiment UI
#'
#' Main audience panel UI
#'
#' @import shiny DT
#' @export
#'
#' @examples
experiment_ui = function(impact_variables){
  tabPanel(
    "Experiment", value = "experiment",
    fluidRow(
      column(3,
             uiOutput("selectExperimentUI"),
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

select_experiment_ui = function(rv){
  return(renderUI(
    selectInput(inputId = "selectedExperiment",
                label = "Select Experiment",
                choices = rv$experiment[, name],
                multiple = F)
  ))
}
