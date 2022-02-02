#' Get summary of experiment
#'
#' @param experiment_name experiment name
#' @param experiment_audience experiment audience from database
#' @param experiment experiment from database
#'
#' @return String with time live (or until start) and users assigned each treatment
#' @import data.table
#' @export
get_experiment_summary = function(experiment_name,
                                  experiment_audience,
                                  experiment,
                                  treatment){

  exp_id = experiment[name == experiment_name, experiment_id]
  exp_vol = experiment_audience[experiment_id == exp_id, .N, treatment_id]
  exp_vol = merge(exp_vol, treatment[, .(name, treatment_id)])
  exp_start = experiment[name == experiment_name, start_datetime]
  days_live = as.numeric(today() - as_date(exp_start))

  if(days_live > 0){
    days_live_str = str_c("Live since ",
                          as_date(exp_start),
                          " (", days_live, " days)\n")
  } else {
    days_live_str = str_c("Launches on ", as_date(exp_start),"\n")
  }

  return(str_c(
    days_live_str,
    "Treatments:\n",
    paste(exp_vol[, str_c("  ", name, " (N = ", N, ")")], collapse = "\n")
  ))

}
