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

devtools::load_all("../rctr/")

#### global data ----

# query data from rctr schema
# treatment = get_table("treatment")
# audience = get_table("audience")
# audience_filter = get_table("audience_filter")
# experiment = get_table("experiment")
# experiment_treatment = get_table("experiment_treatment")
# experiment_audience = get_table("experiment_audience")
#
# user = get_table("user", dataset = "example_data")
# impact_variables = setdiff(
#   names(get_table("user_data", dataset = "example_data", limit = 1)),
#   c("user_id", "timestamp")
# )

# prepare
metadata = create_user_metadata(user)
user[, incl := T]

#### UI ----

css <- HTML(" body {
    background-color: #DDDDDD;
            }")

ui = navbarPage(
  "ACT",
  id = "mainNav",
  theme = shinytheme("flatly"),
  selected = "experiment",
  tags$head(tags$style(HTML('* {font-family: "Courier"};'))),

  # experiment
  experiment_ui(),

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
  output$audience <- renderDT(rv$audience, selection = 'single')

  # audience_rows_selected
  audienceRowsSelectedObs(input, output, rv)

  # CREATE AUDIENCE ----

  # audienceFilter
  output$audienceFilter = renderDT(rv$audienceFilter)

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

  # . createAudienceModal ----
  observeEvent(input$createAudience,{showModal(createAudienceModal())})

  # . createAudienceOk ----

  createAudienceOkObs(input, rv, session)

  # * ----
  # * * * * EXPERIMENT * * * * ----
  # reactive values ----

  # local copy of experiment table
  rv$experiment = copy(experiment)

  # local copy of experiment_treatment table
  rv$experimentTreatment = copy(experiment_treatment)

  # CREATE EXPERIMENT ----

  # Survey ----
  # (functions defined in experiment_creation_survey.R)

  observeEvent(
    input$createExperiment,
    showModal(modalDialog(create_experiment_ui(),
                          title = "Create Experiment",
                          size = "l"))
  )

  # . dynamic questionnaire ----

  # delivery
  output$qD.1.ui <- ecs.qD.1.ui(input)
  output$qD.2.ui <- ecs.qD.2.ui(input)
  output$qD.3.ui <- ecs.qD.3.ui(input)
  output$qD.3b.ui <- ecs.qD.3b.ui(input)

  # attrition
  output$qA.1.ui <- ecs.qA.1.ui(input)
  output$qA.2.ui <- ecs.qA.2.ui(input)
  output$qA.2b.ui <- ecs.qA.2b.ui(input)

  # contamination
  # (put code here)

  # sizing
  # (put code here)

  # . click through ----

  observeEvent(input$ecsBasicsNext, {
    updateTabsetPanel(session, "create_experiment_tabset", "delivery")
  })
  observeEvent(input$ecsDeliveryNext, {
    updateTabsetPanel(session, "create_experiment_tabset", "attrition")
  })
  observeEvent(input$ecsAttritionNext, {
    updateTabsetPanel(session, "create_experiment_tabset", "contamination")
  })
  observeEvent(input$ecsContaminationNext, {
    updateTabsetPanel(session, "create_experiment_tabset", "sizing")
  })
  observeEvent(input$ecsSizingNext, {
    updateTabsetPanel(session, "create_experiment_tabset", "finish")
  })

  # VIEW EXPERIMENT ----

  # . experimentSummary ----

  # this should eventually include all of the information about the experiment
  # setup from the ECS
  output$experimentSummary <- renderText(
    get_experiment_summary(input$selectedExperiment,
                           experiment_audience = experiment_audience,
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
  })

  # . impactSummary ----
  output$impactSummary <- renderText(
    measure_user_impact(rv$user_impact_data)
  )

  # . impactPlot ----
  output$impactPlot <- renderPlot(
    plot_timeseries_impact(rv$timeseries_impact_data,
                           input$selectedExperiment,
                            experiment[name == input$selectedExperiment,
                                       start_datetime],
                           rv$impactVariableLoaded)
  )



  # deprecate / revisit ----

  # . experiment_rows_selected (deprecate) ----
  # output$experimentSelected = renderDT(
  #   rv$experimentTreatment[
  #     experiment_id == rv$experiment[
  #       name == input$selectedExperiment,
  #       experiment_id
  #     ]
  #   ]
  # )

  # . createExperimentModal (revisit later) ----
  # createExperimentModal = function(){
  #   modalDialog(
  #     textInput(
  #       "experimentName",
  #       "Experiment Name: "
  #     ),
  #     selectInput(
  #       "experimentAudienceNew",
  #       "Select Audience",
  #       choices = rv$audience[, unique(name)]
  #     ),
  #     dateRangeInput(
  #       "experimentDateRange",
  #       "Select Dates",
  #       start = Sys.Date() + days(1),
  #       end = Sys.Date() + days(8),
  #       min = Sys.Date() + days(1)
  #     ),
  #     footer = tagList(
  #       modalButton("Cancel"),
  #       actionButton("createExperimentOk", "OK")
  #     )
  #   )
  # }


  # . createExperimentOk (revisit later) ----

  # observeEvent(input$createExperimentOk, {
  #   removeModal()
  #   showModal(loadingModal("Creating Experiment ..."))
  #   create_experiment(
  #     name = input$experimentName,
  #     audience_id = rv$audience[which(name == input$experimentAudienceNew),
  #                               audience_id],
  #     experiment_treatment = rv$experimentTreatmentNew,
  #     start_datetime = input$experimentDateRange[1],
  #     end_datetime = input$experimentDateRange[2]
  #   )
  #   rv$experiment = get_table("experiment")
  #   rv$experimentTreatment = get_table("experiment_treatment")
  #   rv$experimentTreatmentNew = data.table(treatment_id = 1, weight = 1)
  #   removeModal()
  #   updateTabsetPanel(
  #     session, "mainNav",
  #     selected = "viewExperiment"
  #   )
  # })
  #
  # deprecate...
  # output$experiment <- renderDT(rv$experiment[, .(name)], selection = 'single')
  #
  # . add treatments (deprecate) ----
  # output$experiment_treatment <- renderDT(rv$experiment_treatment, selection = 'single')
  #
  # # . experimentTreatmentNew ----
  # output$experimentTreatmentNew <- renderDT(
  #   format_experiment_treatment(
  #     rv$experimentTreatmentNew,
  #     rv$treatment
  #   ),
  #   options = list(paging = FALSE, searching = FALSE)
  # )
  #
  # # . createExperimentTreatmentModal ----
  # createExperimentTreatmentModal = function(){
  #   modalDialog(
  #     selectInput(
  #       "createExperimentTreatmentSelected",
  #       label = "Select Treatment",
  #       choices = setdiff(rv$treatment[, unique(name)], "Control")
  #     ),
  #     numericInput(
  #       "createExperimentTreatmentWeight",
  #       label = "Treatment Weight",
  #       value = 1,
  #       min = 0
  #     ),
  #     footer = tagList(
  #       modalButton("Cancel"),
  #       actionButton("createExperimentTreatmentOk", "OK")
  #     )
  #   )
  # }
  #
  # observeEvent(input$createExperimentTreatment, {
  #   showModal(createExperimentTreatmentModal())
  # })
  #
  # # . createExperimentTreatmentOk ----
  # observeEvent(input$createExperimentTreatmentOk, {
  #   rv$experimentTreatmentNew = rbind(
  #     copy(rv$experimentTreatmentNew),
  #     data.table(
  #       treatment_id = rv$treatment[
  #         name == input$createExperimentTreatmentSelected,
  #         treatment_id
  #       ],
  #       weight = input$createExperimentTreatmentWeight
  #     )
  #   )
  #   removeModal()
  # })
  #
  # . ecsTreatmentNextUI ----
  #
  # output$ecsTreatmentNextUI = renderUI({
  #   if(nrow(rv$experimentTreatmentNew) > 1){
  #     return(
  #       actionButton(
  #         "ecsTreatmentNext",
  #         "Next"
  #       )
  #     )
  #   } else {
  #     return(p("Add a treatment to continue"))
  #   }
  # })

  # holds treatments while creating new experiment
  # initialize with control and a weighting of 1
  # rv$experimentTreatmentNew = data.table(treatment_id = 1, weight = 1)




}

#### run ----

shinyApp(ui, server)
