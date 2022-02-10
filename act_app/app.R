#### set env ----

# shiny libraries
library(shiny)
library(shinythemes)
library(DT)

# general libraries
library(bigrquery)
library(bigQueryR)
library(data.table)
library(lubridate)
library(glue)
library(stringr)
library(ggplot2)
library(scales)

options(shiny.sanitize.errors = FALSE)

run_locally = T
if(run_locally){
  devtools::load_all("~/ab_lab/rctr/")
  # set_env("~/exampleCorp/ec/context.yml")
  set_env("~/ab_lab_mnt_lummo/context.yml")
} else {
  devtools::load_all("/src/rctr/rctr")
  set_env("/srv/context.yml")
}

schema_tbls = ddl.check()

#### global data ----

# # query data from project schema
treatment = get_table("treatment")
audience = get_table("audience")
audience_filter = get_table("audience_filter")
experiment = get_table("experiment")
experiment_treatment = get_table("experiment_treatment")
experiment_audience = get_table("experiment_audience")

user = get_table(Sys.getenv("segment_table"))

if(Sys.getenv("timeseries_table") %in% schema_tbls){
  impact_variables = setdiff(
    names(get_table(Sys.getenv("timeseries_table"), limit = 1)),
    c(Sys.getenv("unit_pk"), Sys.getenv("timeseries_timestamp"))
  )
} else {
  impact_variables = c("(set up timeseries table)")
}

# prepare
metadata = create_user_metadata(user)
user[, incl := T]

#### UI ----

css <- HTML(" body {
    background-color: #DDDDDD;
            }")

ui = navbarPage(
  "rctr",
  id = "mainNav",
  theme = shinytheme("flatly"),
  selected = "experiment",
  tags$head(tags$style(HTML('* {font-family: "Verdana"};'))),

  # experiment
  experiment_ui(impact_variables),

  # treatment
  treatment_ui(),

  # audience
  audience_ui()
)

#### SERVER ----

server <- function(input, output, session) {

  # initialize ----

  rv = reactiveValues()

  # * ----
  # * * * * TREATMENT * * * * ----

  # reactive values ----
  rv$treatment = copy(treatment)

  # outputs ----
  output$treatment <- renderDT(rv$treatment)

  # observers ----
  createTreatmentObs(input)
  createTreatmentObsOk(input, rv)

  # * ----
  # * * * *  AUDIENCE * * * * ----

  # reactive values ----

  # local copy of audience table
  rv$audience = copy(audience)

  # local copy of audience_filter table
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

  # VIEW AUDIENCE ----

  # audience
  output$audience <- renderDT(rv$audience[, .(`Audience Name` = name)],
                              selection = 'single')

  # audience_rows_selected
  audienceRowsSelectedObs(input, output, rv)

  # CREATE AUDIENCE ----

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

  # . new filter plots ----

  # audienceFilterMetricPlot
  output$audienceFilterMetricPlot = renderPlot({
    create_audience_filter_metric_plot(input, rv)
  })

  # audienceFilterVariablePlot
  output$audienceFilterVariablePlot = renderPlot({
    create_audience_filter_variable_plot(input, rv)
  })

  # . audienceCreateUI ----
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

  # events ----


  # . audienceFilterModal ----

  observeEvent(input$createAudienceFilter, {
    showModal(audienceFilterModal(input, output, metadata))
  })

  # . audienceFilterMetricOk ----
  audienceFilterMetricOkObs(input, rv)

  # . audienceFilterVariableOk ----
  audienceFilterVariableOkObs(input, rv)

  # . resetAudienceFilter ----
  observeEvent(input$resetAudienceFilter,{ rv$audienceFilter = data.table() })

  # . createAudienceModal ----
  observeEvent(input$createAudience,{showModal(createAudienceModal())})

  # . createAudienceOk ----

  createAudienceOkObs(input, rv, session)

  # * ----
  # * * * * EXPERIMENT * * * * ----
  # reactive values ----

  # local copy of experiment tables
  rv$experiment = copy(experiment)
  rv$experimentTreatment = copy(experiment_treatment)
  rv$experimentAudience = copy(experiment_audience)

  # CREATE EXPERIMENT ----

  # Survey ----
  # (functions defined in experiment_creation_survey.R)

  observeEvent(
    input$createExperiment,
    showModal(modalDialog(create_experiment_ui(rv, impact_variables),
                          title = "Create Experiment",
                          size = "l"))
  )

  # . click through ----

  observeEvent(input$newExperimentSave, {
    removeModal()
    showModal(loadingModal("Creating experiment ..."))
    create_experiment(
      input, rv, session,
      audience_id = rv$audience[
        name == input$newExperimentAudience, audience_id
      ],
      treatment_id = rv$treatment[
        name == input$newExperimentTreatment, treatment_id
      ]
    )
    removeModal()
    showModal(modalDialog(p("Experiment created"), easyClose = T))
  })

  # VIEW EXPERIMENT ----

  # . select an experiment ----

  output$selectExperimentUI = select_experiment_ui(rv)

  # . experimentSummary ----

  # this should eventually include all of the information about the experiment
  # setup from the ECS
  output$experimentSummary <- renderText(
    get_experiment_summary(input$selectedExperiment,
                           experiment_audience = rv$experimentAudience,
                           experiment = rv$experiment,
                           treatment = rv$treatment)
  )

  # . loadImpact ----

  observeEvent(input$loadImpact, {
    showModal(loadingModal("Loading unit-wise observations ..."))
    rv$impactVariableLoaded = copy(input$impactVariable)
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

  # . impactSummary ----
  output$impactSummary <- renderText(
    measure_user_impact(rv$user_impact_data)
  )

  # . impactPlots ----

  output$timeseriesPlot <- renderPlot(
    plot_timeseries_impact(rv$timeseries_impact_data,
                           rv$experiment[name == input$selectedExperiment,
                                         start_datetime])
  )

  output$cumulativePlot <- renderPlot(
    plot_cumulative_impact(compute_cumulative_impact(rv$cumulative_impact_data))
  )

  output$populationPlot <- renderPlot(
    plot_distribution(rv$user_impact_data, input$popVsEst == "Mean Estimate")
  )

}

#### run ----

# shinyApp(ui, server)

app <- shinyApp(ui = ui, server = server)
runApp(app, host ="0.0.0.0", port = 80, launch.browser = FALSE)
