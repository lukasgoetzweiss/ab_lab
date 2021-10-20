describe_experiment = function(experiment_record){
  paste(
    "         Experiment name:", experiment_record$name, "\n",
    "               Audience:", experiment_record$audience, "\n",
    "Primary impact variable:", experiment_record$primary_impact_variable,
    "\n \n",
    ifelse(
      is.na(experiment_record$delivery_variable),
      "Delivery not tracked",
      paste("Delivery tracked via", experiment_record$delivery_variable)
    ), "\n",
    ifelse(
      is.na(experiment_record$attrition_variable),
      "No attrition assumed",
      paste(
        "Attrition tracked via", experiment_record$attrition_variable,
        " assumed to be ", ifelse(
          experiment_record$attrition_independence_prior == "Yes",
          "Independent", "Dependent"
        ),
        " of treatment"
      )
    ), "\n",
    ifelse(
      experiment_record$spillover_prior == "No",
      "No spillover assumed",
      "Spillover assumed"
    )
  )
}
