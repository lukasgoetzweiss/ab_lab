#' Create record in treatment table
#'
#' @param name name for new treatment
#' @param dataset BigQuery project ID
#'
#' @return treatment_id
#' @import data.table
#' @export
create_treatment = function(name, dataset = Sys.getenv("bq_dataSet")){

  treatment_id = pull_data(glue::glue(
    "select max(treatment_id)  from {dataset}.treatment"
  ))$f0_[1] + 1
  if(is.na(treatment_id)){ treatment_id = 1 }

  message("creating treatment ", name, ", treatment_id = ", treatment_id)

  push_data(
    "treatment",
    data.table(
      treatment_id = as.integer(treatment_id),
      name = name,
      create_datetime = Sys.time()
    )
  )

  return(treatment_id)

}
