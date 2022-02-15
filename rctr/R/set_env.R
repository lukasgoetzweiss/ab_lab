# configure environment from context.yml, see README for a context.yml example
set_env = function(config_path = "~/context.yml"){

  # load yaml and check for expected fields
  config = yaml::read_yaml(config_path)

  expected_fields = c("bq_project", "bq_user", "bq_dataset",
                      "unit_pk", "segment_table", "timeseries_table",
                      "timeseries_timestamp")

  missing_fields = setdiff(expected_fields, names(config))
  if(length(missing_fields)){
    stop(paste("missing fields in config file: ",
               paste(missing_fields, collapse = ", ")))
  }

  # set environment variables
  Sys.setenv(bq_projectID = config$bq_project)
  Sys.setenv(bq_dataSet = config$bq_dataset)

  Sys.setenv(unit_pk = config$unit_pk)
  Sys.setenv(segment_table = config$segment_table)

  Sys.setenv(timeseries_table = config$timeseries_table)
  Sys.setenv(timeseries_timestamp = config$timeseries_timestamp)

  # authenticate bigQueryR (bigrquery is authenticated with an environment
  # passed in at run time)
  if(!is.null(config$bq_verif)){
    message("authenticating bigQueryR using json file")
    bigQueryR::bqr_auth(json_file = config$bq_verif)
  } else {
    message("authenticating bigQueryR using email")
    bigQueryR::bqr_auth(email = config$bq_user)
  }

}
