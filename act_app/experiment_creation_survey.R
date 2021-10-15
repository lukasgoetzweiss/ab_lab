
# UI ----

# to do - make treatment and audience reactive values

create_experiement_ui = function(){
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
        choices = setdiff(treatment[, unique(name)], "Control")
      ),
      selectInput(
        "experimentAudienceNew",
        "Select Audience",
        choices = audience[, unique(name)], 
        multiple = F
      ),
      selectInput(
        "experimentVariableNew",
        "Select Main Impact Variable",
        choices = impact_variables
      ),
      actionButton("ecsBasicsNext", "Next")
      # sidebarLayout(
      #   sidebarPanel(
      #     actionButton("createExperimentTreatment", "Add Treatment"), 
      #     p(),
      #     uiOutput("ecsTreatmentNextUI"),
      #     width = 3
      #   ),
      #   mainPanel(
      #     DTOutput("experimentTreatmentNew")
      #   )
      # )
    ),
    tabPanel(
      "Delivery",
      value = "delivery",
      uiOutput("qD.1.ui"),
      uiOutput("qD.2.ui"),
      uiOutput("qD.3.ui"),
      uiOutput("qD.3b.ui"),
      actionButton("ecsDeliveryNext", "Next")
    ),
    tabPanel(
      "Attrition",
      value = "attrition",
      uiOutput("qA.1.ui"),
      uiOutput("qA.2.ui"),
      textOutput("qA.2b.ui"),
      actionButton("ecsAttritionNext", "Next")
    ),
    tabPanel(
      "Contamination",
      value = "contamination",
      
      # move to server
      radioButtons(
        "qC.1",
        label = paste("Is it possible that <treatment> could impact <impact",
                      "variable> other <unit>s?"),
        choices = c("Yes", "No"),
        selected = "No",
        inline = T
      ),
      actionButton("ecsContaminationNext", "Next")
    ),
    
    tabPanel(
      "Sizing",
      value = "sizing",
      p("add test sizing and timing"),
      actionButton("ecsSizingNext", "Next")
    ),
    
    tabPanel(
      "Finish",
      value = "finish",
      p("Output summarizing experiment + save button")
    )
  )
}

#### Server elements ----

# . Deliverability ----

ecs.qD.1.ui <- function(input){renderUI({
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
}

ecs.qD.2.ui <- function(input){renderUI({
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
}

ecs.qD.3.ui <- function(input){
    renderUI({
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
}

ecs.qD.3b.ui <- function(input){renderUI({
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
}

# . Attrition ----

ecs.qA.1.ui <- function(input){renderUI({
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
}

ecs.qA.2.ui <- function(input){renderUI({
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
}

ecs.qA.2b.ui <- function(input){renderText({
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
}