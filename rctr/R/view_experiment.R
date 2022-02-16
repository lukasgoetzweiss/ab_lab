# functions to support experiment visualization

#' Get summary of experiment
#'
#' @param experiment_name experiment name
#' @param experiment_audience experiment audience from database
#' @param experiment experiment from database
#'
#' @return String with time live (or until start) and users assigned each treatment
#' @import data.table
#' @export
get_experiment_summary = function(experiment_name, rv){

  if(is.null(experiment_name) || experiment_name == ""){
    return("No experiments found")
  }

  exp_id = rv$experiment[name == experiment_name, experiment_id]
  exp_vol = rv$experimentAudience[experiment_id == exp_id, .N, treatment_id]
  exp_vol = merge(exp_vol, rv$treatment[, .(name, treatment_id)])
  exp_start = rv$experiment[name == experiment_name, start_datetime]
  days_live = as.numeric(today() - as_date(exp_start))

  audience_name = rv$audience[
    audience_id == rv$experiment[experiment_id == exp_id, audience_id],
    name
  ]

  if(days_live > 0){
    days_live_str = str_c("Live since ",
                          as_date(exp_start),
                          " (", days_live, " days)\n")
  } else {
    days_live_str = str_c("Launches on ", as_date(exp_start),"\n")
  }

  return(str_c(
    days_live_str,
    "Audience: ", audience_name, "\n",
    "Treatments:\n",
    paste(exp_vol[, str_c("  ", name, " (N = ", N, ")")], collapse = "\n")
  ))

}

analysis_horizon_ui = function(input, rv){
  if(is.null(rv$cumulative_impact_data) ||
     !any(complete.cases(rv$cumulative_impact_data))){
    return(p(""))
  }

  numericInput(
    "analysisHorizon",
    "Impact Horizon", min = 1,
    max = rv$cumulative_impact_data[, max(horizon)],
    value = rv$cumulative_impact_data[, max(horizon)]
  )

}
