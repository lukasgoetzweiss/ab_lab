#### description ----
# This file combines elements defined in the rctr package to create the main
# shiny web app for the project.
#
# To run the app locally you will need to load rctr and set the environment
# yourself (see set env section)

#### set env ----

# shiny libraries
library(shiny)
library(shinythemes)
library(DT)

# general libraries
library(bigQueryR)
library(data.table)
library(lubridate)
library(glue)
library(stringr)
library(ggplot2)
library(scales)

# options
options(shiny.sanitize.errors = FALSE)

# load rctr and configure environment variables with rctr::set_env()
run_locally = T
if(run_locally){
  if(!("rctr" %in% .packages())){
    stop("Please load rctr and set environment by running ",
         "devtools::load_all('<path>/ab_lab/rctr')")
  }
  if(Sys.getenv("bq_dataSet") == ""){
    stop("Please set environment by running ",
         "rctr::set_env(<path to context.yml>) (see readme)")
  }
} else {
  devtools::load_all("/src/rctr/rctr")
  set_env("/srv/context.yml")
}

# check that schema is set up correctly, if any tables are missing this function
# will try to create them
schema_tbls = ddl.check()

#### global data ----

# query common data structure tables from project schema
treatment = get_table("treatment")
audience = get_table("audience")
audience_filter = get_table("audience_filter")
experiment = get_table("experiment")
experiment_treatment = get_table("experiment_treatment")
experiment_audience = get_table("experiment_audience")

# query data integration tables from project schema
user = get_table(Sys.getenv("segment_table"))

# check if time series table is setup, if so load impact variable options
if(Sys.getenv("timeseries_table") %in% schema_tbls){
  impact_variables = setdiff(
    names(get_table(Sys.getenv("timeseries_table"), limit = 1)),
    c(Sys.getenv("unit_pk"), Sys.getenv("timeseries_timestamp"))
  )
} else {
  impact_variables = c("(set up timeseries table)")
}

# prepare data
metadata = create_user_metadata(user)
user[, incl := T]

#### UI ----

# set css elements
css <- HTML(" body {
    background-color: #DDDD00;
            }")

# define main UI
ui = navbarPage(
  title = "rctr",
  id = "mainNav",
  theme = shinytheme("flatly"),
  selected = "experiment",
  tags$head(tags$style(HTML('* {font-family: "Verdana"};'))),

  # experiment
  experiment_ui(impact_variables),

  # treatment
  treatment_ui(),

  # audience
  audience_ui(),

  tags$script(
    HTML("var header = $('.navbar > .container-fluid');
          header.append('<div style=\"float:right; padding-top: 8px\"><button id=\"reloadData\" type=\"button\" class=\"btn btn-primary action-button\"><i class=\"fa fa-rotate-right\" role=\"presentation\" aria-label=\"rotate-right icon\"></i> </button></div>')")
  )

)

#### SERVER ----

server <- function(input, output, session) {

  # initialize ----
  rv = reactiveValues()

  # refresh data observer
  observeEvent(input$reloadData, {
    message("reloading data")
    showModal(loadingModal("Refreshing data ..."))
    rv$treatment = get_table("treatment")
    rv$audience = get_table("audience")
    rv$audience_filter = get_table("audience_filter")
    rv$experiment = get_table("experiment")
    rv$experiment_treatment = get_table("experiment_treatment")
    rv$experiment_audience = get_table("experiment_audience")

    # query data integration tables from project schema
    user = get_table(Sys.getenv("segment_table"))

    # check if time series table is setup, if so load impact variable options
    if(Sys.getenv("timeseries_table") %in% schema_tbls){
      impact_variables = setdiff(
        names(get_table(Sys.getenv("timeseries_table"), limit = 1)),
        c(Sys.getenv("unit_pk"), Sys.getenv("timeseries_timestamp"))
      )
    } else {
      impact_variables = c("(set up timeseries table)")
    }
    removeModal()
  })

  # * * * * TREATMENT * * * * ----

  # reactive values ----
  rv$treatment = copy(treatment)

  # outputs ----
  output$treatment <- renderDT(rv$treatment)

  # observers ----
  createTreatmentObs(input)
  createTreatmentObsOk(input, rv)

  # * * * *  AUDIENCE * * * * ----

  # reactive values ----

  # local copy of audience tables
  rv$audience = copy(audience)
  rv$audienceFilterAll = copy(audience_filter)

  # local audience filter for creating new audiences
  rv$audienceFilter = data.table()

  # user distribution for estimating audience filter impact on audience size
  rv$user = reactive({
    u = copy(user)
    if(nrow(rv$audienceFilter)){
      for(i in 1:nrow(rv$audienceFilter)){
        u = apply_filter(u, rv$audienceFilter[i, comparator_params])
      }
    }
    return(u)
  })

  # . view ----

  # . . outputs ----
  output$audience <- renderDT(rv$audience[, .(`Audience Name` = name)],
                              selection = 'single')

  # . . observers ----
  audienceRowsSelectedObs(input, output, rv)

  # . create ----

  # . . outputs ----

  # audienceFilter
  output$audienceFilterSql = renderText(
    format_audience_query(rv$audienceFilter)
  )

  # new filter UIs
  output$audienceFilterMetricRange = renderUI({
    audienceFilterMetricRangeUI(input, metadata)
  })

  output$audienceFilterVariableRange = renderUI({
    audienceFilterVariableRangeUI(input, metadata)
  })

  # new filter plots

  # audienceFilterMetricPlot
  output$audienceFilterMetricPlot = renderPlot({
    create_audience_filter_metric_plot(input, rv)
  })

  # audienceFilterVariablePlot
  output$audienceFilterVariablePlot = renderPlot({
    create_audience_filter_variable_plot(input, rv)
  })

  # audienceCreateUI
  output$audienceCreateUI = renderUI({
    if(nrow(rv$audienceFilter)){
      return(
        actionButton(
          "createAudience",
          "Save Audience"
        )
      )
    } else {
      return(p("Add a filter to begin"))
    }
  })

  # . . observers ----

  # audienceFilterModal
  observeEvent(input$createAudienceFilter, {
    showModal(audienceFilterModal(input, output, metadata))
  })

  # audienceFilterMetricOk
  audienceFilterMetricOkObs(input, rv)

  # audienceFilterVariableOk
  audienceFilterVariableOkObs(input, rv)

  # resetAudienceFilter
  observeEvent(input$resetAudienceFilter,{ rv$audienceFilter = data.table() })

  # createAudienceModal
  observeEvent(input$createAudience,{showModal(createAudienceModal())})

  # createAudienceOk
  createAudienceOkObs(input, rv, session)


  # * * * * EXPERIMENT * * * * ----

  # reactive values ----

  # local copy of experiment tables
  rv$experiment = copy(experiment)
  rv$experimentTreatment = copy(experiment_treatment)
  rv$experimentAudience = copy(experiment_audience)

  # . create ----

  # . . observers ----

  # (functions defined in experiment_creation_survey.R)
  observeEvent(
    input$createExperiment,
    showModal(modalDialog(create_experiment_ui(rv, impact_variables),
                          title = "Create Experiment",
                          size = "l"))
  )

  # click through
  observeEvent(input$newExperimentSave, {
    removeModal()
    showModal(loadingModal("Creating experiment ..."))
    create_experiment(
      input, rv, session
    )
    removeModal()
    showModal(modalDialog(p("Experiment created"), easyClose = T))
  })

  # . view ----

  # . . outputs ----

  # selectExperimentUI
  output$selectExperimentUI = select_experiment_ui(rv)

  # experimentSummary
  output$experimentSummary <- renderText(
    get_experiment_summary(input$selectedExperiment, rv)
  )

  # analysisHorizonUI
  output$analysisHorizonUI = renderUI(analysis_horizon_ui(input, rv))

  # impact table
  output$cumulativeMeasurementDT <- renderDT(
    format_cumulative_impact(
      compute_cumulative_impact(
        rv$cumulative_impact_data,
        control_treatment_id = rv$experiment[
          name == rv$selectedExperimentLoaded,
          control_treatment_id
        ]
      ),
      horizon_select = input$analysisHorizon
    )
  )

  # impact plots
  output$timeseriesPlot <- renderPlot(
    plot_timeseries_impact(rv$timeseries_impact_data,
                           rv$experiment[name == rv$selectedExperimentLoaded,
                                         start_datetime])
  )

  output$cumulativePlot <- renderPlot(
    plot_cumulative_impact(compute_cumulative_impact(
      rv$cumulative_impact_data,
      control_treatment_id = rv$experiment[
        name == rv$selectedExperimentLoaded,
        control_treatment_id
      ]
    ))
  )

  output$populationPlot <- renderPlot(
    plot_distribution(rv$user_impact_data, input$popVsEst == "Mean Estimate")
  )

  # . . observers ----

  # loadImpact
  observeEvent(input$loadImpact, {
    showModal(loadingModal("Loading unit-wise observations ..."))
    rv$impactVariableLoaded = copy(input$impactVariable)
    rv$selectedExperimentLoaded = copy(input$selectedExperiment)
    rv$user_impact_data = get_user_impact_data(
      experiment_id = rv$experiment[name == input$selectedExperiment,
                                    experiment_id],
      impact_variable = input$impactVariable,
      pre_period_days = 14
    )
    showModal(loadingModal("Loading timeseries data ..."))
    rv$timeseries_impact_data = get_timeseries_impact_data(
      experiment_id = rv$experiment[name == input$selectedExperiment,
                                    experiment_id],
      impact_variable = input$impactVariable,
      pre_period_days = 14
    )
    removeModal()
    showModal(loadingModal("Loading cumulative impact data ..."))
    rv$cumulative_impact_data = get_cumulative_impact_data(
      experiment_id = rv$experiment[name == input$selectedExperiment,
                                    experiment_id],
      impact_variable = input$impactVariable,
      max_horizon = as.numeric(
        (today() - days(1) -
        rv$experiment[name == input$selectedExperiment,
                      as_date(start_datetime)])
      )
    )
    removeModal()
  })

}

#### run ----

app <- shinyApp(ui = ui, server = server)
runApp(app, host ="0.0.0.0", port = 80, launch.browser = FALSE)
