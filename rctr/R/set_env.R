set_env = function(config_path = "~/rctr.yml"){

  config = yaml::read_yaml(config_path)

  expected_fields = c("bq_project", "bq_user", "bq_dataset", "bq_verif",
                      "unit_pk", "segment_table", "timeseries_table",
                      "timeseries_timestamp")

  missing_fields = setdiff(expected_fields, names(config))
  if(length(missing_fields)){
    stop(paste("missing fields in config file: ",
               paste(missing_fields, collapse = ", ")))
  }

  Sys.setenv(bq_projectID = config$bq_project)
  Sys.setenv(bq_dataSet = config$bq_dataset)

  Sys.setenv(unit_pk = config$unit_pk)
  Sys.setenv(segment_table = config$segment_table)

  Sys.setenv(timeseries_table = config$timeseries_table)
  Sys.setenv(timeseries_timestamp = config$timeseries_timestamp)

  # bigrquery::bq_auth(config$bq_user, cache = T)
  bigQueryR::bqr_auth(json_file = config$bq_verif)
}
