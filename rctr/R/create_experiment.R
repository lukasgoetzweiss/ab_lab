#' Maps shiny inputs to format in experiment database table
#'
#' @param input shiny input object
#' @param audience local version of audience table
#'
#' @return experiment parameters in a format consistent with database
#' @export
input_to_experiment_record = function(rv, input){

  # check input fields
  required_fields = c(
    "newExperimentName",
    "newExperimentTreatment",
    "newExperimentControlTreatment",
    "newExperimentAudience",
    "newExperimentVariable",
    "newExperimentControlPercentage",
    "newExperimentDateRange"
  )

  missing_fields = setdiff(required_fields, names(input))
  if(length(missing_fields)){
    stop(paste("Missing input field(s): ",
               paste(missing_fields, collapse = ", ")))
  }

  audience_id = rv$audience[
    name == input$newExperimentAudience, audience_id
  ]

  control_treatment_id = rv$treatment[
    name == input$newExperimentControlTreatment, treatment_id
  ]

  return(data.table(
    name = input$newExperimentName,
    audience_id = audience_id,
    control_treatment_id = control_treatment_id,
    primary_impact_variable = input$newExperimentVariable,
    start_datetime = as.POSIXct(input$newExperimentDateRange[1]),
    end_datetime = as.POSIXct(input$newExperimentDateRange[2]),
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
create_experiment = function(input, rv, session,
                             dataset = Sys.getenv("bq_dataSet")){

  # cast input data to experiment record
  experiment_record = input_to_experiment_record(rv, input)

  # get treatment_id
  treatment_id = rv$treatment[
    name == input$newExperimentTreatment, treatment_id
  ]

  # determine next experiment_id
  experiment_id = pull_data(glue::glue(
    "select max(experiment_id)  from {dataset}.experiment"
  ))$f0_[1] + 1
  if(is.na(experiment_id)){ experiment_id = 1 }

  message(Sys.time(), ": creating experiment ", experiment_record$name,
          ", experiment_id = ", experiment_id)

  experiment_record[, experiment_id := as.integer(experiment_id)]

  # create experiment treatment
  message(Sys.time(), ": creating experiment_treatment records")

  # determine next experiment_treatment_ids
  experiment_treatment_id = pull_data(glue::glue(
    "select max(experiment_treatment_id)  from {dataset}.experiment_treatment"
  ))$f0_[1]
  if(is.na(experiment_treatment_id)){ experiment_treatment_id = 0 }

  # add two treatments to experiment_treatment
  experiment_treatment_id = experiment_treatment_id + (1:2)

  # create new experiment_treatment records
  experiment_treatment_records = data.table(
    experiment_treatment_id = as.integer(experiment_treatment_id),
    experiment_id = as.integer(experiment_id),
    treatment_id = as.integer(c(experiment_record$control_treatment_id,
                                treatment_id)),
    sample_weight = c((input$newExperimentControlPercentage / 100),
                      1 - (input$newExperimentControlPercentage / 100)),
    create_datetime = Sys.time()
  )

  # create randomized audience
  experiment_audience_records = randomize_new_experiment_segment(
    rv, input, experiment_id
  )

  # write everything to the BD
  message(now(), ": writing to experiment")
  push_data("experiment", experiment_record)

  message(now(), ": writing to experiment_treatment")
  push_data("experiment_treatment", experiment_treatment_records)

  message(now(), " writing to experiment_audience")
  push_data("experiment_audience", experiment_audience_records)

  # refresh local instances of experiment tables
  rv$experiment = get_table("experiment")
  rv$experimentTreatment = get_table("experiment_treatment")
  rv$experimentAudience = get_table("experiment_audience")

  return(experiment_id)

}

randomize_new_experiment_segment = function(rv, input, experiment_id){

  # get sql logic for target segment
  seg_sql = paste(
    rv$audienceFilterAll[
      audience_id == rv$audience[name == input$newExperimentAudience,
                                 audience_id],
      comparator_sql
    ],
    collapse = " and "
  )

  # pull eligible users
  segment_units = pull_data(glue(
    "select {Sys.getenv('unit_pk')} as unit_id
         from {Sys.getenv('bq_dataSet')}.{Sys.getenv('segment_table')}
        where {seg_sql}"
  ))

  # get parameters for randomization
  n_units = segment_units[, .N]
  p_ctrl = input$newExperimentControlPercentage / 100
  tx_id = rv$treatment[name == input$newExperimentTreatment, treatment_id]
  ctrl_id = rv$treatment[name == input$newExperimentControlTreatment,
                         treatment_id]

  # create random draw for each user
  unit_ru = runif(n_units, 0, 1)

  # split users based on random draw
  unit_treatment =
    ctrl_id + (unit_ru > quantile(unit_ru, p_ctrl)) * (tx_id - ctrl_id)

  # prepare experiment_audience_id
  experiment_audience_id = pull_data(glue(
    "select max(experiment_audience_id)
         from {Sys.getenv('bq_dataSet')}.experiment_audience"
  ))[, f0_]
  if(is.na(experiment_audience_id)){ experiment_audience_id = 0}
  experiment_audience_id = experiment_audience_id + (1:n_units)

  message(lubridate::now(), " randomization complete, treatment distribution:")
  message("treatment_id = ", tx_id, " : ", sum(unit_treatment == tx_id))
  message("treatment_id = ", ctrl_id, " : ", sum(unit_treatment == ctrl_id))

  return(
    segment_units[, .(
      experiment_audience_id = as.integer(experiment_audience_id),
      experiment_id = as.integer(experiment_id),
      unit_id = as.character(unit_id),
      treatment_id = as.integer(unit_treatment),
      active_fg = 'Y',
      create_datetime = now(),
      modified_datetime = now()
    )]
  )

}
