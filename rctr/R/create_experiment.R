#' Maps shiny inputs to format in experiment database table
#'
#' @param input shiny input object
#' @param audience local version of audience table
#'
#' @return experiment parameters in a format consistent with database
#' @export
input_to_experiment_record = function(input){

  # check input fields
  required_fields = c(
    "experimentName",
    "createExperimentTreatmentSelected",
    "experimentAudienceNew",
    "experimentVariableNew",
    "q2.1",
    "q3.1",
    "q4.1",
    "newExperimentControlPercentage",
    "experimentDateRange"
  )

  missing_fields = setdiff(required_fields, names(input))
  if(length(missing_fields)){
    stop(paste("Missing fields: ", paste(missing_fields, collapse = ", ")))
  }

  return(data.table(
    name = input$experimentName,
    audience = input$experimentAudienceNew,
    primary_impact_variable = input$experimentVariableNew,

    delivery_variable = ifelse(input$q2.1 == not_collected_str,
                               NA, input$q2.1),

    attrition_variable = ifelse(input$q3.1 == "Yes", input$q3.2, NA_character_),
    attrition_rate_prior = ifelse(
      is.null(input$q3.3), NA_real_, input$q3.3
    ),
    attrition_independence_prior = ifelse(
      is.null(input$q3.4), NA_character_, input$q3.4
    ),

    spillover_prior = input$q4.1,

    start_datetime = as.POSIXct(input$experimentDateRange[1]),
    end_datetime = as.POSIXct(input$experimentDateRange[2]),
    create_datetime = Sys.time(),
    modify_datetime = Sys.time()
  ))

}

#' Create record in experiment table
#'
#' @param name experiment name
#' @param audience_id audience to target with experiment
#' @param experiment_treatment treatments in the experiment, with weightings
#' @param start_datetime when the experiment will start
#' @param end_datetime when the experiment will end
#' @param dataset BigQuery dataset
#'
#' @return experiment_id
#' @import data.table
#' @export
create_experiment = function(input,
                             audience_id,
                             treatment_id,
                             # name,
                             # audience_id, experiment_treatment,
                             # start_datetime, end_datetime,
                             dataset = Sys.getenv("bq_dataSet")){

  experiment_record = input_to_experiment_record(input)

  # determine next experiment_id
  experiment_id = pull_data(glue::glue(
    "select max(experiment_id)  from {dataset}.experiment"
  ))$f0_[1] + 1
  if(is.na(experiment_id)){ experiment_id = 1 }

  message(Sys.time(), ": creating experiment ", experiment_record$name,
          ", experiment_id = ", experiment_id)

  experiment_record[, experiment_id := as.integer(experiment_id)]

  # drop audience name and replace with audience_id
  experiment_record[, audience := audience_id]
  setnames(experiment_record, "audience", "audience_id")

  push_data("experiment", experiment_record)

  # create experiment treatment
  message(Sys.time(), ": creating experiment_treatment records")

  # determine next treatment_id
  experiment_treatment_id = pull_data(glue::glue(
    "select max(experiment_treatment_id)  from {dataset}.experiment_treatment"
  ))$f0_[1]
  if(is.na(experiment_treatment_id)){ experiment_treatment_id = 0 }

  experiment_treatment_id = experiment_treatment_id + (1:2)

  experiment_treatment_records = data.table(
    experiment_treatment_id = as.integer(experiment_treatment_id),
    experiment_id = as.integer(experiment_id),
    treatment_id = as.integer(c(1, treatment_id)),
    sample_weight = c(input$newExperimentControlPercentage / 100,
                      1 - input$newExperimentControlPercentage / 100),
    create_datetime = Sys.time()
  )

  push_data("experiment_treatment", experiment_treatment_records)

  return(experiment_id)

}
