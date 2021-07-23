#### set env ----

library(shiny)
library(shinythemes)
library(DT)

source("~/Documents/Career/ab_lab/act_lib.R")

#### global data ----

# query
treatment = get_treatment()
user = get_user()
audience = get_audience()
audience_filter = get_audience_filter()

# prepare
metadata = create_metadata(user)
user[, incl := T]

#### UI ----

css <- HTML(" body {
    background-color: #DDDDDD;
            }")

ui = navbarPage(
  "ACT",
  id = "mainNav",
  theme = shinytheme("flatly"),
  # tags$head(tags$style(css)),
  # . experiment ----
  navbarMenu(
    "Experiment",
    tabPanel(
      "View",
      h2("put view here")
    ),
    tabPanel(
      "Create",
      h2("put create here")
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
      value = "view",
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
      actionButton("createAudience",
                   "Save Audience"),
      p()
    )
  )
)



#### SERVER ----

server <- function(input, output, session) {
  
  rv = reactiveValues()
  
  # treatment ----
  
  # . outputs ----
  
  rv$treatment = copy(treatment)
  
  output$treatment <- renderDT(rv$treatment)
  
  # . modals ----
  
  createTreatmentModal <- function() {
    modalDialog(
      textInput("treatmentName", "Treatment Name: "
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("treatmentOk", "OK")
      )
    )
  }
  
  loadingModal = function(text = "Loading...") {
    modalDialog(
      p(text)
    )
  }
  
  # . events ----
  
  # . . createTreatment ----
  observeEvent(input$createTreatment, {
    showModal(createTreatmentModal())
  })
  
  # . . treatmentOk ----
  observeEvent(input$treatmentOk, {
    removeModal()
    showModal(loadingModal("Creating Treatment ..."))
    create_treatment(name = input$treatmentName)
    rv$treatment = get_treatment()
    removeModal()
  })
  
  # audience ----
  
  # . reactive values ----
  
  rv$audience = copy(audience)
  rv$audienceFilterAll = copy(audience_filter)
  
  rv$audienceFilter = data.table()

  # . . user ----
  rv$user = reactive({
    u = copy(user)
    if(nrow(rv$audienceFilter)){
      for(i in 1:nrow(rv$audienceFilter)){
        u = apply_filter(u, rv$audienceFilter[i, comparator_params])
      }
    }
    return(u)
  })
  
  # . outputs ----
  
  # . . audience -----
  output$audience <- renderDT(rv$audience, selection = 'single')
  
  # . . audienceFilter ----
  output$audienceFilter = renderDT(rv$audienceFilter)
  
  # . . audienceFilterMetricOptions ----
  rv$audienceFilterMetricOptions = reactive({
    metadata$user_metric[
      name == input$audienceFilterMetric, 
      c(metric_min, metric_max)
    ]
  })
  
  # . . audienceFilterMetricRange ----
  output$audienceFilterMetricRange = renderUI({
    sliderInput(
      "audienceFilterMetricRange",
      "Range: ", 
      rv$audienceFilterMetricOptions()[1],
      rv$audienceFilterMetricOptions()[2],
      rv$audienceFilterMetricOptions()
    )
  })
  
  # . . audienceFilterMetricPlot ----
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
  
  # . . audienceFilterVariableOptions ----
  rv$audienceFilterVariableOptions = reactive({
    metadata$user_variable_value[
      variable == input$audienceFilterVariable, 
      unique(value)
    ]
  })
  
  # . . audienceFilterVariableRange ----
  output$audienceFilterVariableRange = renderUI({
    selectInput(
      "audienceFilterVariableSelected",
      "Values: ", 
      choices = rv$audienceFilterVariableOptions(),
      multiple = T
    )
  })
  
  # . . audienceFilterVariablePlot ----
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
  
  # . modals ----
  
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
  
  # . events ----
  
  # . . audience_rows_selected ----
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
  
  # . . createAudienceFilter ----
  observeEvent(input$createAudienceFilter, {
    showModal(audienceFilterModal())
  })
  
  # . . audienceFilterMetricOk ----
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
  
  # . . audienceFilterVariableOk ----
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
  
  # . . createAudience ----
  observeEvent(input$createAudience,{
    showModal(createAudienceModal())
  })
  
  # . . createAudienceOk ----
  observeEvent(input$createAudienceOk, {
    removeModal()
    showModal(loadingModal("Creating Audience ..."))
    create_audience(
      name = input$audienceName, 
      audience_filter = rv$audienceFilter
    )
    rv$audience = get_audience()
    rv$audienceFilterAll = get_audience_filter()
    # reset rv$audienceFilter
    rv$audienceFilter = data.table()
    removeModal()
    updateTabsetPanel(
      session, "mainNav",
      selected = "view"
    )
  })
  
  
  
}

#### run ----

shinyApp(ui, server)