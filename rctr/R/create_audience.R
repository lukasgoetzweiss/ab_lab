# This file contains functions to create new audiences and manage filters for
# new audience creation

# AUDIENCE FILTER ----

# . plotting ----

create_audience_filter_metric_plot = function(input, rv){

  rv$user()[, incl_margin := get(input$audienceFilterMetric) %between% input$audienceFilterMetricRange]
  rv$user()[is.na(incl_margin), incl_margin := FALSE]

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

}

create_audience_filter_variable_plot = function(input, rv){

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
    coord_flip() +
    theme(panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "grey90"),
          legend.position = "bottom")

}
# . UI  ----

audienceFilterMetricRangeUI = function(input, metadata){

  metric_range = metadata$user_metric[
    name == input$audienceFilterMetric,
    c(metric_min, metric_max)
  ]

  sliderInput(
    "audienceFilterMetricRange",
    "Range: ",
    metric_range[1],
    metric_range[2],
    metric_range
  )

}

audienceFilterVariableRangeUI = function(input, metadata){
  variable_range = metadata$user_variable_value[
    variable == input$audienceFilterVariable,
    unique(value)
  ]
  selectInput(
    "audienceFilterVariableSelected",
    "Values: ",
    choices = variable_range,
    multiple = T
  )
}

# . modals ----

audienceFilterModal <- function(input, output, metadata) {
  modalDialog(
    tabsetPanel(
      # metric tab
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

      # variable tab
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

# AUDIENCE ----


#' Create records in audience and audience_filter tables
#'
#' @param name Audience name
#' @param audience_filter Audience filters
#' @param dataset BigQuery dataset
#'
#' @return audience_id
#' @import data.table
#' @export

create_audience = function(name, audience_filter,
                           dataset = Sys.getenv("bq_dataSet")){

  # get next audience_id
  audience_id = pull_data(glue::glue(
    "select max(audience_id)  from {dataset}.audience"
  ))$f0_[1] + 1
  if(is.na(audience_id)){ audience_id = 1 }

  # get next audience_filter_id(s)
  audience_filter_id = pull_data(glue::glue(
    "select max(audience_filter_id)  from {dataset}.audience_filter"
  ))$f0_[1]
  if(is.na(audience_filter_id)){ audience_filter_id = 0 }

  audience_filter_id = audience_filter_id + (1:audience_filter[, .N])

  # push data to DB
  message("creating audience ", name, ", audience_id = ", audience_id)

  push_data(
    "audience",
    data.table(
      audience_id = as.integer(audience_id),
      name = name,
      create_datetime = Sys.time()
    )
  )

  push_data(
    "audience_filter",
    audience_filter[, .(audience_filter_id = as.integer(audience_filter_id),
                        audience_id = as.integer(audience_id),
                        filter_on,
                        comparator_sql,
                        comparator_params,
                        create_datetime = Sys.time())]
  )

  return(audience_id)

}

# . event handlers ----

audienceFilterMetricOkObs = function(input, rv){
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
}

audienceFilterVariableOkObs = function(input, rv){

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
}

# . modals ----

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

# . event handlers ----

createAudienceOkObs = function(input, rv, session){
  observeEvent(input$createAudienceOk, {
    removeModal()
    showModal(loadingModal("Creating Audience ..."))
    create_audience(
      name = input$audienceName,
      audience_filter = rv$audienceFilter
    )

    # update local instance of db tables
    rv$audience = get_table("audience")
    rv$audienceFilterAll = get_table("audience_filter")

    # reset rv$audienceFilter
    rv$audienceFilter = data.table()

    # reset app
    removeModal()
    updateTabsetPanel(
      session, "mainNav",
      selected = "viewAudience"
    )
  })
}
