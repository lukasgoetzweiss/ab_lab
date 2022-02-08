
# UI ----

# TODO - make treatment and audience reactive values
#        note: I might have fixed this, need to verify

#' create_experiment_ui
#'
#' UI for experiment creation survey
#'
#' @export
create_experiment_ui = function(rv, impact_variables){
  mainPanel(
    id = "create_experiment_tabset",
    # tabPanel(
      # "Basics",
      # value = "basics",
      textInput(
        "newExperimentName",
        "Experiment Name: "
      ),
      selectInput(
        "newExperimentTreatment",
        label = "Select Treatment",
        choices = setdiff(rv$treatment[, unique(name)], "Control")
      ),
      selectInput(
        "newExperimentAudience",
        "Select Audience",
        choices = rv$audience[, unique(name)],
        multiple = F
      ),
      selectInput(
        "newExperimentVariable",
        "Select Main Impact Variable",
        choices = impact_variables
      ),
      dateRangeInput(
        "newExperimentDateRange",
        "Select dates",
        start = Sys.Date() + days(1),
        end = Sys.Date() + days(8),
        min = Sys.Date() + days(1)
      ),
      numericInput(
        "newExperimentControlPercentage",
        "Control Percentage",
        0.5,
        min = 0,
        max = 1
      ),
      actionButton("newExperimentSave", "Save")
    )
    # tabPanel(
    #   "Sizing",
    #   value = "sizing",
    #   sidebarLayout(
    #     sidebarPanel(
    #       # dateRangeInput(
    #       #   "newExperimentDateRange",
    #       #   "Select dates",
    #       #   start = Sys.Date() + days(1),
    #       #   end = Sys.Date() + days(8),
    #       #   min = Sys.Date() + days(1)
    #       # ),
    #       actionButton(
    #         "loadSizingData",
    #         "Load sizing data",
    #       ),
    #       shinyjs::hidden(p(id = "text1", "Loading...")),
    #       numericInput(
    #         "newExperimentDeliveryTreatment",
    #         "Delivery percentage prior, treatment:",
    #         min = 0, max = 100, step = 1, value = 100
    #       ),
    #       numericInput(
    #         "newExperimentDeliveryControl",
    #         "Delivery percentage prior, control:",
    #         min = 0, max = 100, step = 1, value = 0
    #       ),
    #       radioButtons(
    #         "newExperimentAttritionMode",
    #         "Attrition",
    #         choices = c("None", "Independent"), selected = "None",
    #         inline = T
    #       ),
    #       uiOutput("newExperimentAttritionRateUI"),
    #     ),
    #     mainPanel(
    #       p("plots go here"),
    #       verbatimTextOutput("newExperimentMDE")
    #     )
    #   ),
    #
    #   actionButton("ecsSizingNext", "Next")
    # ),

    # tabPanel(
    #   "Finish",
    #   verbatimTextOutput("newExperimentDescription"),
    #   value = "finish",
    #   actionButton("newExperimentSave", "Save")
    # )
  # )
}

#### Server elements ----

# newExperimentAttritionRateUI.ui = function(input){renderUI(
#   if(length(input$newExperimentAttritonMode)
#      && input$newExperimentAttritonMode == "Independent"){
#     return(
#       numericInput(
#         "newExperimentAttritionRate",
#         "Expected attrition percentage",
#         min = 0, max = 100, step = 1, value = 1
#       )
#     )
#   } else {
#     return(p(""))
#   }
# )}

# . Sizing ----

# . Finish ----

get_experiment_description = function(input){
  renderText(
    describe_experiment(input_to_experiment_record(input))
  )
}

