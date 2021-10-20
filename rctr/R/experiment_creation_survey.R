
# UI ----

# TODO - make treatment and audience reactive values
#        note: I might have fixed this, need to verify

#' create_experiment_ui
#'
#' UI for experiment creation survey
#'
#' @export
create_experiment_ui = function(rv){
  tabsetPanel(
    id = "create_experiment_tabset",
    tabPanel(
      "Basics",
      value = "basics",
      textInput(
        "experimentName",
        "Experiment Name: "
      ),
      selectInput(
        "createExperimentTreatmentSelected",
        label = "Select Treatment",
        choices = setdiff(rv$treatment[, unique(name)], "Control")
      ),
      selectInput(
        "experimentAudienceNew",
        "Select Audience",
        choices = rv$audience[, unique(name)],
        multiple = F
      ),
      selectInput(
        "experimentVariableNew",
        "Select Main Impact Variable",
        choices = impact_variables
      ),
      actionButton("ecsBasicsNext", "Next")
    ),
    tabPanel(
      "Delivery",
      value = "delivery",
      uiOutput("q2.1.ui"),
      uiOutput("q2.2.ui"),
      actionButton("ecsDeliveryNext", "Next")
    ),
    tabPanel(
      "Attrition",
      value = "attrition",
      uiOutput("q3.1.ui"),
      uiOutput("q3.2.ui"),
      uiOutput("q3.3.ui"),
      uiOutput("q3.4.ui"),
      uiOutput("q3.5.ui"),
      actionButton("ecsAttritionNext", "Next")
    ),
    tabPanel(
      "Spillover",
      uiOutput("q4.1.ui"),
      uiOutput("q4.2.ui"),
      value = "spillover",
      actionButton("ecsSpilloverNext", "Next")
    ),

    tabPanel(
      "Sizing",
      value = "sizing",
      dateRangeInput(
        "experimentDateRange",
        "Select dates",
        start = Sys.Date() + days(1),
        end = Sys.Date() + days(8),
        min = Sys.Date() + days(1)
      ),
      numericInput(
        "newExperimentControlPercentage",
        "Select control percentage",
        min = 0, max = 100, step = 1, value = 50
      ),
      p("(add more tools for test sizing and timing)"),
      actionButton("ecsSizingNext", "Next")
    ),

    tabPanel(
      "Finish",
      verbatimTextOutput("newExperimentDescription"),
      value = "finish",
      actionButton("newExperimentSave", "Save")
    )
  )
}

#### Server elements ----

# . Deliverability ----

not_collected_str = "Not Collected"

ecs.q2.1.ui = function(input){
  renderUI({
    selectInput(
      "q2.1",
      label = glue(paste("What variable indicates that <unit> has successfully",
                         " received {input$createExperimentTreatmentSelected}?")
        ),
      choices = c("dummy option 1", "dummy option 2", not_collected_str),
      selected = "dummy option 1",
      multiple = F,
      width = '100%'
    )
})}

ecs.q2.2.ui = function(input){renderUI({
  if(length(input$q2.1) && input$q2.1 == not_collected_str){
    return(p(paste(glue(
      "Will not account for delivery of ",
      "{input$createExperimentTreatmentSelected} and will instead assume all ",
      "assigned <units> receive {input$createExperimentTreatmentSelected}."
    ))))
  } else {
    return(p(""))
  }
})}

# . Attrition ----

ecs.q3.1.ui = function(input){renderUI(
  return(
    radioButtons(
      "q3.1",
      label = glue(paste(
        "Are there any <unit>s that will need to be removed from the study?",
        "For instance, because {input$experimentVariableNew} will not be known?"
      )),
      choices = c("Yes", "No"),
      selected = "No",
      inline = T,
      width = '100%'
    )
  )
)}

ecs.q3.2.ui = function(input){renderUI(
  if(length(input$q3.1) && input$q3.1 == "Yes"){
    return(selectInput(
      "q3.2",
      glue(paste(
        "What field indicates that a <unit> should be removed from (or has",
        "left) the study?"
      )),
      choices = c("dummy choice 1", "dummy choice 2"),
      selected = "dummny choice 1",
      width = "100%",
      multiple = F
    ))
  } else {
    return(p(""))
  }
)}

ecs.q3.3.ui = function(input){renderUI(
  if(length(input$q3.1) && input$q3.1 == "Yes"){
    return(numericInput(
      "q3.3",
      glue(paste(
        "What percent of <unit>s do we expect to remove from the study?"
      )),
      min = 0, max = 100, step = 1, value = 0,
      width = "100%"
    ))
  } else {
    return(p(""))
  }
)}

ecs.q3.4.ui = function(input){renderUI(
  if(length(input$q3.1) && input$q3.1 == "Yes"){
    return(radioButtons(
      "q3.4",
      glue(paste(
        "Do we expect {input$createExperimentTreatmentSelected} to impact who",
        "will be removed from the study, either directly or indirectly? (note",
        "that the treatment could impact the number of <units> that leave the",
        "study, or just which <units> leave the study)"
      )),
      choices = c("Yes", "No"),
      selected = "No",
      inline = T,
      width = '100%'
    ))
  } else {
    return(p(""))
  }
)}

ecs.q3.5.ui = function(input){renderUI(
  if(length(input$q3.1) && input$q3.1 == "No"){
    return(p(""))
  }
  else if(length(input$q3.4) && input$q3.4 == "Yes"){
    return(p(glue(paste(
        "Warning: We will need to make large assumptions about missing data to",
        "ensure results are not biased, uneven attrition may lead to extremely",
        "large confidence intervals."
      ))))
  } else {
    return(p(paste(
      "We will measure attrition and confirm that there is no statistically",
      "significant difference between treatment group and control group."
      )))
  }
)}

# . Spillover ----

ecs.q4.1.ui = function(input){renderUI(
  return(radioButtons(
    "q4.1",
    label = glue(paste(
      "Is it possible that one <unit> receiving ",
      "{input$createExperimentTreatmentSelected} could impact",
      "{input$experimentVariableNew} of other <unit>s?"
    )),
    choices = c("Yes", "No"),
    selected = "No",
    inline = T,
    width = "100%"
  ))
)}

ecs.q4.2.ui = function(input){renderUI(
  if(length(input$q4.1) && input$q4.1 == "Yes"){
    return(p(paste(
      "Spillover can introduce bias, consider methods to better separate ",
      "<unit>s to reduce spillover. One approach could be to change the unit ",
      "of observation."
    )))
  } else {
    return(p(""))
  }
)}

# . Finish ----

get_experiment_description = function(input){
  renderText(
    describe_experiment(input_to_experiment_record(input))
  )
}

