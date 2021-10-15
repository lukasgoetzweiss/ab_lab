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
create_experiment = function(name,
                             audience_id, experiment_treatment,
                             start_datetime, end_datetime,
                             dataset = Sys.getenv("bq_dataSet")){

  experiment_id = pull_data(glue::glue(
    "select max(experiment_id)  from {dataset}.experiment"
  ))$f0_[1] + 1
  if(is.na(experiment_id)){ experiment_id = 1 }

  message("creating experiment ", name, ", experiment_id = ", experiment_id)

  push_data(
    "experiment",
    experiment_treatment[, .(
      experiment_id = as.integer(experiment_id),
      audience_id = as.integer(audience_id),
      name = name,
      start_datetime = as.POSIXct(start_datetime),
      end_datetime = as.POSIXct(end_datetime),
      create_datetime = Sys.time()
    )]
  )

  experiment_treatment_id = pull_data(glue::glue(
    "select max(experiment_treatment_id)  from {dataset}.experiment_treatment"
  ))$f0_[1]
  if(is.na(experiment_treatment_id)){ experiment_treatment_id = 0 }

  experiment_treatment_id = experiment_treatment_id + (1:experiment_treatment[, .N])

  push_data(
    "experiment_treatment",
    experiment_treatment[, .(
      experiment_treatment_id = as.integer(experiment_treatment_id),
      experiment_id = as.integer(experiment_id),
      treatment_id,
      sample_weight = weight / sum(weight),
      create_datetime = Sys.time()
    )]
  )

  return(experiment_id)

}
