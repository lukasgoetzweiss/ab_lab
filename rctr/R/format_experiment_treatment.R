#' Format experiment_treatment
#'
#' @param experiment_treatment experiment_treatment table from database
#' @param treatment treatment table from database
#'
#' @return experiment_treatment formatted to be more readable
#' @import data.table
#' @export
format_experiment_treatment = function(experiment_treatment, treatment){

  if(!nrow(experiment_treatment)){
    return(NULL)
  }

  experiment_treatment_fmt = merge(
    experiment_treatment,
    treatment,
    by = "treatment_id"
  )

  return(experiment_treatment_fmt[, .(
    ` ` = name,
    Weight = weight,
    `Sample Percent` = scales::percent(weight / sum(weight)))]
  )

}
