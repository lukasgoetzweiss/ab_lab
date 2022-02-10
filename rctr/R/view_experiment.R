
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


describe_experiment = function(experiment_record){
  paste(
    "               Experiment name:", experiment_record$name, "\n",
    "                     Audience:", experiment_record$audience, "\n",
    "      Primary impact variable:", experiment_record$primary_impact_variable,
    "\n \n",

    "Delivery estimate (treatment):", experiment_record$delivery_treatment_prior,
    "\n",
    "  Delivery estimate (control):", experiment_record$delivery_control_prior,

    "\n \n",
    ifelse(
      experiment_record$attrition_mode_prior == "None",
        "        No attrition expected",
      paste(
        "Even attrition of", experiment_record$attrition_rate_prior, "%",
        "expected in treatment and control."
      )

    )
  )
}
