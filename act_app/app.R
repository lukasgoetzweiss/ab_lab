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
source("experiment_creation_survey.R")

loadingModal = function(text = "Loading...") {
  modalDialog(p(text), footer = NULL)
}

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
  selected = "viewExperiment",
  tags$head(tags$style(HTML('* {font-family: "Courier"};'))),
  # . experiment ----
  navbarMenu(
    "Experiment",
    tabPanel(
      "View",
      value = "viewExperiment",
      fluidRow(
        column(3, 
               selectInput(inputId = "selectedExperiment",
                           label = "Select Experiment", 
                           choices = experiment[, name], 
                           multiple = F)
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
    ),
    tabPanel(
      "Create",
      create_experiement_ui() # (see experiment_creation_survey.R)
    )
  ),
  
  # . treatment ----
  tabPanel(
    "Treatment",
    DTOutput('treatment'),
    actionButton("createTreatment", "Create New", icon = icon("fas fa-plus"))
  ),
  # . audience ----
  navbarMenu(
    "Audience",
    tabPanel(
      "View",
      value = "viewAudience",
      DTOutput('audience'),
      DTOutput('audienceSelected')
    ),
    tabPanel(
      "Create",
      h2("Audience Filters"),
      DTOutput('audienceFilter'),
      actionButton("createAudienceFilter", 
                   "Add Filter", 
                   icon = icon("fas fa-plus")),
      p(),
      uiOutput("audienceCreateUI"),
      p()
    )
  )
)



#### SERVER ----

server <- function(input, output, session) {
  
  # initialize ----
  rv = reactiveValues()

  # * ----
  # * * * * TREATMENT * * * * ----
  
  # outputs ----
  
  rv$treatment = copy(treatment)
  
  output$treatment <- renderDT(rv$treatment)
  
  # events ----
  
  # . createTreatmentModal ----
  
  createTreatmentModal <- function() {
    modalDialog(
      textInput("treatmentName", "Treatment Name: "
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("createTreatmentOk", "OK")
      )
    )
  }
  
  observeEvent(input$createTreatment, {
    showModal(createTreatmentModal())
  })
  
  # . createTreatmentOk ----
  observeEvent(input$createTreatmentOk, {
    removeModal()
    showModal(loadingModal("Creating Treatment ..."))
    create_treatment(name = input$treatmentName)
    rv$treatment = get_table("treatment")
    removeModal()
  })

  # * ----
  # * * * *  AUDIENCE * * * * ----
  
  # reactive values ----
  
  # local copy of audience table
  rv$audience = copy(audience)
  
  # local copy of audience_filter table
  rv$audienceFilterAll = copy(audience_filter)
  
  # local audience filter for creating new audiences
  rv$audienceFilter = data.table()

  # . user ----
  
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
  
  # outputs ----
  
  # . audience -----
  output$audience <- renderDT(rv$audience, selection = 'single')
  
  # . audienceFilter ----
  output$audienceFilter = renderDT(rv$audienceFilter)
  
  # . audienceFilterMetricOptions ----
  rv$audienceFilterMetricOptions = reactive({
    metadata$user_metric[
      name == input$audienceFilterMetric, 
      c(metric_min, metric_max)
    ]
  })
  
  # . audienceFilterMetricRange ----
  output$audienceFilterMetricRange = renderUI({
    sliderInput(
      "audienceFilterMetricRange",
      "Range: ", 
      rv$audienceFilterMetricOptions()[1],
      rv$audienceFilterMetricOptions()[2],
      rv$audienceFilterMetricOptions()
    )
  })
  
  # . audienceFilterMetricPlot ----
  output$audienceFilterMetricPlot = renderPlot({
    
    rv$user()[, incl_margin := get(input$audienceFilterMetric) %between% input$audienceFilterMetricRange]
    
    ggplot(rv$user(), aes(get(input$audienceFilterMetric),
                          fill = ifelse(incl, ifelse(incl_margin, "Included", "Removed"), "Filtered by prior filter"))) +
      geom_histogram(stat="count") + 
      ggtitle(glue("{rv$user()[, sum(incl & incl_margin)]} of {rv$user()[, sum(incl)]} users remain")) + 
      scale_fill_manual(values = c("#6280F8", "#9EB0FA", "grey85"), 
                        breaks = c("Included", "Removed", "Filtered by prior filter"),
                        name = "") +
      xlab(input$audienceFilterMetric) + 
      ylab("Users") + 
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_line(color = "grey90"),
            legend.position = "bottom")
  })
  
  # . audienceFilterVariableOptions ----
  rv$audienceFilterVariableOptions = reactive({
    metadata$user_variable_value[
      variable == input$audienceFilterVariable, 
      unique(value)
    ]
  })
  
  # . audienceFilterVariableRange ----
  output$audienceFilterVariableRange = renderUI({
    selectInput(
      "audienceFilterVariableSelected",
      "Values: ", 
      choices = rv$audienceFilterVariableOptions(),
      multiple = T
    )
  })
  
  # . audienceFilterVariablePlot ----
  output$audienceFilterVariablePlot = renderPlot({
    
    rv$user()[, incl_margin := get(input$audienceFilterVariable) %in% input$audienceFilterVariableSelected]
    
    ggplot(rv$user(), aes(get(input$audienceFilterVariable),
                          fill = ifelse(incl, ifelse(incl_margin, "Included", "Removed"), "Filtered by prior filter"))) +
      geom_histogram(stat="count") + 
      ggtitle(glue("{rv$user()[, sum(incl & incl_margin)]} of {rv$user()[, sum(incl)]} users remain")) + 
      scale_fill_manual(values = c("#6280F8", "#9EB0FA", "grey85"), 
                        breaks = c("Included", "Removed", "Filtered by prior filter"),
                        name = "") +
      xlab(input$audienceFilterMetric) + 
      ylab("Users") + 
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_line(color = "grey90"),
            legend.position = "bottom")
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
  
  # . audience_rows_selected ----
  observeEvent(input$audience_rows_selected, {
    output$audienceSelected = renderDT(
      rv$audienceFilterAll[
        audience_id == rv$audience[
          input$audience_rows_selected,
          audience_id
        ]
      ]
    )
  })
  
  # . audienceFilterModal ----
  audienceFilterModal <- function() {
    modalDialog(
      tabsetPanel(
        #  metric tab ----
        tabPanel(
          "Metric",
          selectInput(
            "audienceFilterMetric",
            "Select Metric",
            choices = metadata$user_metric[, name], 
            selected = metadata$user_metric[1, name]
            
          ),
          uiOutput("audienceFilterMetricRange"),
          plotOutput("audienceFilterMetricPlot"),
          modalButton("Cancel"),
          actionButton("audienceFilterMetricOk", "OK")
        ),
        # variable tab ----
        tabPanel(
          "Variable",
          selectInput(
            "audienceFilterVariable",
            "Select Variable",
            choices = metadata$user_variable[, name], 
            selected = metadata$user_variable[1, name]
            
          ),
          uiOutput("audienceFilterVariableRange"),
          plotOutput("audienceFilterVariablePlot"),
          modalButton("Cancel"),
          actionButton("audienceFilterVariableOk", "OK")
        )
      )
    )
  }
  
  observeEvent(input$createAudienceFilter, {
    showModal(audienceFilterModal())
  })
  
  # . audienceFilterMetricOk ----
  observeEvent(input$audienceFilterMetricOk, {
    
    comparator_params = paste(
      c("m", input$audienceFilterMetric, input$audienceFilterMetricRange),
      collapse = ":"
    )
    
    rv$audienceFilter = copy(rbind(
      rv$audienceFilter, 
      data.table(
        filter_on = input$audienceFilterMetric,
        comparator_sql = paste(
          input$audienceFilterMetric, ">=", input$audienceFilterMetricRange[1],
          "and", input$audienceFilterMetric, "<=", input$audienceFilterMetricRange[2]
        ),
        comparator_params = comparator_params
      )
    ))
    removeModal()
  })
  
  # . audienceFilterVariableOk ----
  observeEvent(input$audienceFilterVariableOk, {
    
    comparator_params = paste(
      c("v", input$audienceFilterVariable, input$audienceFilterVariableSelected),
      collapse = ":"
    )
    
    comparator_sql_str = paste(input$audienceFilterVariableSelected, 
                               collapse = "', '")
    
    rv$audienceFilter = copy(rbind(
      rv$audienceFilter, 
      data.table(
        filter_on = input$audienceFilterVariable, 
        comparator_sql = paste(
          input$audienceFilterVariable, " in ('", comparator_sql_str, "')",
          sep = ""
        ),
        comparator_params = comparator_params
      )
    ))
    removeModal()
  })
  
  # . createAudienceModal ----
  
  createAudienceModal <- function(){
    modalDialog(
      textInput(
        "audienceName",
        "Audience Name: "
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("createAudienceOk", "OK")
      )
    )
  }
  
  observeEvent(input$createAudience,{
    showModal(createAudienceModal())
  })
  
  # . createAudienceOk ----
  
  observeEvent(input$createAudienceOk, {
    removeModal()
    showModal(loadingModal("Creating Audience ..."))
    create_audience(
      name = input$audienceName, 
      audience_filter = rv$audienceFilter
    )
    rv$audience = get_table("audience")
    rv$audienceFilterAll = get_table("audience_filter")
    # reset rv$audienceFilter
    rv$audienceFilter = data.table()
    removeModal()
    updateTabsetPanel(
      session, "mainNav",
      selected = "viewAudience"
    )
  })
  
  
  # * ----
  # * * * * EXPERIMENT * * * * ----
  # reactive values ----
  
  # local copy of experiment table
  rv$experiment = copy(experiment)
  
  # local copy of experiment_treatment table
  rv$experimentTreatment = copy(experiment_treatment)
  
  # holds treatments while creating new experiment
  # initialize with control and a weighting of 1
  rv$experimentTreatmentNew = data.table(treatment_id = 1, weight = 1)
  
  # DO I NEED THIS?
  # output$experiment <- renderDT(rv$experiment[, .(name)], selection = 'single')
  
  # CREATE EXPERIMENT ----
  # 
  # add treatments (deprecate) ----
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
  
  # Survey ----
  
  # (functions defined in experiment_creation_surver.R)
  
  # . deliverability ----
  
  output$qD.1.ui <- ecs.qD.1.ui(input)
  output$qD.2.ui <- ecs.qD.2.ui(input)
  output$qD.3.ui <- ecs.qD.3.ui(input)
  output$qD.3b.ui <- ecs.qD.3b.ui(input)
  
  # . attrition ----
  
  output$qA.1.ui <- ecs.qA.1.ui(input)
  output$qA.2.ui <- ecs.qA.2.ui(input)
  output$qA.2b.ui <- ecs.qA.2b.ui(input)
  
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
  
  # . experimentSummary ----
  
  output$experimentSummary <- renderText(
    get_experiment_summary(input$selectedExperiment,
                           experiment_audience = experiment_audience,
                           experiment = rv$experiment,
                           treatment = rv$treatment)
  )
  
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
  
  

  
}

#### run ----

shinyApp(ui, server)