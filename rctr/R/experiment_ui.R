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
      column(3,
             selectInput("impactVariable",
                         "Select Impact Variable",
                         choices = impact_variables,
                         multiple = T),
             actionButton("loadImpact", "Measure impact")
      )
    ),

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
        "Distribution",
        plotOutput("populationPlot"),
        radioButtons("popVsEst", "",
                     choices = c("Population", "Mean Estimate"),
                     selected = "Population",
                     inline = T)
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
