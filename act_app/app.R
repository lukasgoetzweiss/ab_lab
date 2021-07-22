#### set env ----

library(shiny)
library(shinythemes)
library(DT)

source("~/Documents/Career/ab_lab/act_lib.R")

#### global data ----

treatment = data.table(get_treatment())
user = get_user()
user[, incl := T]
metadata = create_metadata(user)

#### UI ----

css <- HTML(" body {
    background-color: #DDDDDD;
            }")

ui = navbarPage(
  "ACT", 
  theme = shinytheme("flatly"),
  # tags$head(tags$style(css)),
  # . experiment ----
  tabPanel(
    "Experiment",
    tabsetPanel(
      tabPanel(
        "View",
        h2("")
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
  tabPanel(
    "Audience",
    tabsetPanel(
      tabPanel(
        "View",
        h2("put audiences here")
      ),
      tabPanel(
        "Create",
        DTOutput('audienceFilter'),
        actionButton("createAudienceFilter", "Create New", icon = icon("fas fa-plus")),
        p()
      )
    )
  )
)



#### SERVER ----

server <- function(input, output) {
  
  rv = reactiveValues()
  
  # treatment ----
  
  # . outputs ----
  
  output$treatment <- renderDT(treatment)
  
  # . modals ----
  
  treatmentModal <- function() {
    modalDialog(
      textInput("treatmentName", "Treatment Name: "
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("treatmentOk", "OK")
      )
    )
  }
  
  # . events ----
  
  # . . createTreatment ----
  observeEvent(input$createTreatment, {
    showModal(treatmentModal())
  })
  
  # . . treatmentOk ----
  observeEvent(input$treatmentOk, {
    create_treatment(name = input$treatmentName)
    treatment = get_treatment()
    removeModal()
  })
  
  # audience ----
  
  # . reactive values ----
  
  rv$audienceFilter = data.table()
  # rv$audienceFilterMetric = c()
  
  # . . audienceFilterMetricOptions ----
  rv$audienceFilterMetricOptions = reactive({
    metadata$user_metric[
      name == input$audienceFilterMetric, 
      c(metric_min, metric_max)
    ]
  })
  
  # . . audienceFilterVariableOptions ----
  rv$audienceFilterVariableOptions = reactive({
    metadata$user_variable_value[
      variable == input$audienceFilterVariable, 
      unique(value)
      ]
  })
  
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
  
  # . . audienceFilter ----
  output$audienceFilter = renderDT(rv$audienceFilter)
  
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
  
  # . . audienceFilterVariableRange ----
  output$audienceFilterVariableRange = renderUI({
    selectInput(
      "audienceFilterVariableSelected",
      "Values: ", 
      choices = rv$audienceFilterVariableOptions(),
      multiple = T
    )
  })
  
  # . . audienceFilterMetricPlot ----
  output$audienceFilterMetricPlot = renderPlot({
    
    rv$user()[, incl_margin := get(input$audienceFilterMetric) %between% input$audienceFilterMetricRange]
    
    ggplot(rv$user(), aes(get(input$audienceFilterMetric),
                     fill = ifelse(incl, ifelse(incl_margin, "Included", "Removed"), "Filtered by prior filter"))) +
      geom_histogram(stat="count") + 
      ggtitle(glue("{rv$user()[, sum(incl & incl_margin)]} of {rv$user()[, sum(incl)]} users remain")) + 
      # guides(fill = F) + 
      scale_fill_manual(values = c("#6280F8", "#9EB0FA", "grey85"), 
                        breaks = c("Included", "Removed", "Filtered by prior filter"),
                        name = "") +
      xlab(input$audienceFilterMetric) + 
      ylab("Users") + 
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_line(color = "grey90"),
            legend.position = "bottom")
  })
  
  # . . audienceFilterVariablePlot ----
  output$audienceFilterVariablePlot = renderPlot({
    
    rv$user()[, incl_margin := get(input$audienceFilterVariable) %in% input$audienceFilterVariableSelected]
    
    ggplot(rv$user(), aes(get(input$audienceFilterVariable),
                          fill = ifelse(incl, ifelse(incl_margin, "Included", "Removed"), "Filtered by prior filter"))) +
      geom_histogram(stat="count") + 
      ggtitle(glue("{rv$user()[, sum(incl & incl_margin)]} of {rv$user()[, sum(incl)]} users remain")) + 
      # guides(fill = F) + 
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
  
  # . events ----
  
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
        comparator_sql = glue(
          "{input$audienceFilterMetric} >= {input$audienceFilterMetricRange[1]} 
            and {input$audienceFilterMetric} <= {input$audienceFilterMetricRange[2]}"
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
        comparator_sql = glue(
          "{input$audienceFilterVariable} in ('{comparator_sql_str}')"
        ),
        comparator_params = comparator_params
        )
    ))
    removeModal()
  })
  
  
}

#### run ----

shinyApp(ui, server)