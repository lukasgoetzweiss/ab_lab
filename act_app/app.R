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

loadingModal = function(text = "Loading...") {
  modalDialog(p(text), footer = NULL)
}

#### global data ----

# query data from rctr schema
treatment = get_table("treatment")
audience = get_table("audience")
audience_filter = get_table("audience_filter")
experiment = get_table("experiment")
experiment_treatment = get_table("experiment_treatment")
experiment_audience = get_table("experiment_audience")

user = get_table("user", dataset = "example_data")
impact_variables = setdiff(
  names(get_table("user_data", dataset = "example_data", limit = 1)),
  c("user_id", "timestamp")
)

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
      tabsetPanel(
        tabPanel(
          "Treatment",
          value = "questionnaire.treatment",
          sidebarLayout(
            sidebarPanel(
              actionButton("createExperimentTreatment", "Add Treatment"), 
              p(),
              uiOutput("experimentCreateUI"),
              width = 3
            ),
            mainPanel(
              DTOutput("experimentTreatmentNew")
            )
          )
        ),
        tabPanel(
          "Audience and Variable",
          value = "questionnaire.audience_variable",
          h4("Audience"),
          p("put audience seletion here"),
          
          h4("Impact Variables"),
          p("put impact variables seletion here")
        ),
        tabPanel(
          "Delivery",
          value = "questionnaire.delivery",
          uiOutput("qD.1.ui"),
          uiOutput("qD.2.ui"),
          uiOutput("qD.3.ui"),
          uiOutput("qD.3b.ui"),
          actionButton("q.d.next", "Continue")
        ),
        tabPanel(
          "Attrition",
          value = "questionnaire.attrition",
          uiOutput("qA.1.ui"),
          uiOutput("qA.2.ui"),
          textOutput("qA.2b.ui"),
          actionButton("q.a.next", "Continue")
        ),
        tabPanel(
          "Contamination",
          value = "questionnaire.contamination",
          
          # move to server
          radioButtons(
            "qC.1",
            label = paste("Is it possible that <treatment> could impact <impact",
                          "variable> other <unit>s?"),
            choices = c("Yes", "No"),
            selected = "No",
            inline = T
          ),
          
          actionButton("q.c.next", "Continue")
        ),
        tabPanel(
          "Save",
          p("Output summarizing experiment")
        )
      )
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
  
  # creation survey ----
  
  # . deliverability ----
  
  output$qD.1.ui <- renderUI({
     return(
       radioButtons(
         "qD.1",
         label = "Can you ensure every assigned <unit> will receive <treatment>?",
         choices = c("Yes", "No"),
         selected = "Yes",
         inline = T
       )
     )
  })
  
  output$qD.2.ui <- renderUI({
    if(!length(input$qD.1) || input$qD.1 == "Yes"){
      return(p(""))
    } else {
      return(
        selectInput(
          "qD.2",
          label = "What metric specifies who recieves <treatment>?",
          choices = c("dummy option 1", "dummy option 2", "Not Collected"),
          selected = "dummy option 1",
          multiple = F
        )
      )
    }
  })
  
  output$qD.3.ui <- renderUI({
    if(!length(input$qD.1) || input$qD.1 == "Yes"){
      return(p(""))
    } else if(!length(input$qD.2) || input$qD.2 != "Not Collected"){
      return(p(""))
    } else if(!length(input$qD.2) || input$qD.2 == "Not Collected"){
      return(
        radioButtons(
          "qD.3",
          label = "Do you know what percent of assigned <unit>s will receive <treatment>?",
          choices = c("Yes", "No"),
          selected = "Yes",
          inline = T
        )
      )
    }
  })
  
  output$qD.3b.ui <- renderUI({
    if(!length(input$qD.1) || input$qD.1 == "Yes"){
      return(p(""))
    } else if(!length(input$qD.2) || input$qD.2 != "Not Collected"){
      return(p(""))
    } else if(!length(input$qD.3) || input$qD.3 == "No"){
      return(p("Unable to account for compliance, will only report on intented treatment impact"))
    } else {
      return(
        numericInput(
          "qD.3b",
          label = "Enter percent of assigned <unit>s that will receive <treatment>",
          min = 0,
          max = 1,
          step = 0.01,
          value = 0.9,
          width = '33%'
        )
      )
    }
  })
  
  # . attrition ----
  
  output$qA.1.ui <- renderUI({
    radioButtons(
      "qA.1",
      label = paste("Is it possible that we won’t know <impact variable> for",
                    "some <unit>s at the end of the study?"),
      choices = c("Yes", "No"),
      selected = "No",
      inline = T,
      width = '100%'
    )
  })
  output$qA.2.ui <- renderUI({
    if(!length(input$qA.1) || input$qA.1 == "No"){
      return(p(""))
    } else {
      return(
        radioButtons(
          "qA.2",
          label = "Do we expect the treatment to impact attrition, either directly or indirectly?",
          choices = c("Yes", "No"),
          selected = "No",
          inline = T
        )
      )
    }
  })
  
  output$qA.2b.ui <- renderText({
    if(!length(input$qA.1) || input$qA.1 == "No"){
      return("")
    }
    if(!length(input$qA.2) || input$qA.2 == "No"){
      return(paste(
        "If there is no correlation between treatment and attrition,",
        "attrition will not bias measurement. We will confirm independence",
        "when analyzing results."
      ))
    } else {
      return(paste(
        "Warning: We will need to make some big assumptions about missing",
        "data to ensure results are not biased, uneven attrition may lead to",
        "extremely large confidence intervals"
      ))
    }
  })
  
  # local copy of experiment table
  rv$experiment = copy(experiment)
  
  # local copy of experiment_treatment table
  rv$experimentTreatment = copy(experiment_treatment)
  
  # holds treatments while creating new experiment
  rv$experimentTreatmentNew = data.table()
  
  # outputs ----
  
  # output$experiment <- renderDT(rv$experiment[, .(name)], selection = 'single')
  output$experiment_treatment <- renderDT(rv$experiment_treatment, selection = 'single')
  
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
  
  # . experimentTreatmentNew ----
  output$experimentTreatmentNew <- renderDT(
    format_experiment_treatment(
      rv$experimentTreatmentNew,
      rv$treatment
    ),
    options = list(paging = FALSE, searching = FALSE)
  )
  
  # . experimentCreateUI ----
  
  output$experimentCreateUI = renderUI({
    if(nrow(rv$experimentTreatmentNew)){
      return(
        actionButton(
          "createExperiment",
          "Select Audience and Save Experiment"
        )
      )
    } else {
      return(p(""))
    }
  })
  
  # events ----
  
  # . experiment_rows_selected ----
  output$experimentSelected = renderDT(
    rv$experimentTreatment[
      experiment_id == rv$experiment[
        name == input$selectedExperiment,
        experiment_id
      ]
    ]
  )

  # . createExperimentTreatmentModal ----
  createExperimentTreatmentModal = function(){
    modalDialog(
      selectInput(
        "createExperimentTreatmentSelected",
        label = "Select Treatment",
        choices = rv$treatment[, unique(name)]
      ),
      numericInput(
        "createExperimentTreatmentWeight",
        label = "Treatment Weight",
        value = 1,
        min = 0
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("createExperimentTreatmentOk", "OK")
      )
    )
  }
  
  observeEvent(input$createExperimentTreatment, {
    showModal(createExperimentTreatmentModal())
  })
  
  # . createExperimentTreatmentOk ----
  observeEvent(input$createExperimentTreatmentOk, {
    rv$experimentTreatmentNew = rbind(
      copy(rv$experimentTreatmentNew),
      data.table(
        treatment_id = rv$treatment[
          name == input$createExperimentTreatmentSelected,
          treatment_id
        ],
        weight = input$createExperimentTreatmentWeight
      ) 
    )
    removeModal()
  })
  
  # . createExperimentModal ----
  createExperimentModal = function(){
    modalDialog(
      textInput(
        "experimentName",
        "Experiment Name: "
      ),
      selectInput(
        "experimentAudienceNew",
        "Select Audience",
        choices = rv$audience[, unique(name)]
      ),
      dateRangeInput(
        "experimentDateRange",
        "Select Dates",
        start = Sys.Date() + days(1), 
        end = Sys.Date() + days(8), 
        min = Sys.Date() + days(1)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("createExperimentOk", "OK")
      )
    )  
  }
  observeEvent(input$createExperiment, {
    showModal(createExperimentModal())
  })
  
  # . createExperimentOk ----
  
  observeEvent(input$createExperimentOk, {
    removeModal()
    showModal(loadingModal("Creating Experiment ..."))
    create_experiment(
      name = input$experimentName,
      audience_id = rv$audience[which(name == input$experimentAudienceNew),
                                audience_id],
      experiment_treatment = rv$experimentTreatmentNew,
      start_datetime = input$experimentDateRange[1],
      end_datetime = input$experimentDateRange[2]
    )
    rv$experiment = get_table("experiment")
    rv$experimentTreatment = get_table("experiment_treatment")
    rv$experimentTreatmentNew = data.table()
    removeModal()
    updateTabsetPanel(
      session, "mainNav",
      selected = "viewExperiment"
    )
  })
  
  
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
  
}

#### run ----

shinyApp(ui, server)