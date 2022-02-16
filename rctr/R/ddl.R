
# overhead ----

# define built in tables
ddl.built_in_tables = function(){
  tbls = c(
      "treatment",
      "experiment",
      "experiment_treatment",
      "audience",
      "audience_filter",
      "experiment_audience"
  )
  return(tbls)
}

# get user-defined tables
ddl.user_defined_tables = function(){
  tbls = c(
    Sys.getenv("segment_table"),
    Sys.getenv("timeseries_table")
  )
  return(tbls)
}

# function to check if DB is setup correctly, if a table is missing this will
# attempt to create
ddl.check = function(hard_reset = F){

  message(Sys.time(), ": checking tables in ", Sys.getenv('bq_dataSet'))

  # check built-in tables
  schema_tbls = pull_data(glue::glue(
    "select * from {Sys.getenv('bq_dataSet')}.INFORMATION_SCHEMA.TABLES"
  ))[, table_name]
  built_in_tables = ddl.built_in_tables()
  for(i in 1:length(built_in_tables)){

    i_tbl = built_in_tables[i]

    if(i_tbl %in% schema_tbls){

      if(hard_reset == T){
        message(Sys.time(), ": removing ", i_tbl)
        bigQueryR::bqr_delete_table(Sys.getenv("bq_projectID"),
                                    Sys.getenv("bq_dataSet"),
                                    i_tbl)
        message(Sys.time(), ": creating ", i_tbl)
        get(paste("ddl.", i_tbl, sep = ""))()
      } else{
        message(Sys.time(), ": found ", i_tbl)
      }
    } else {
      message(Sys.time(), ": did not found ", built_in_tables[i],
              " creating now ...")

      # run appropriate ddl function
      get(paste("ddl.", i_tbl, sep = ""))()

      # add table to list of tables in schema
      schema_tbls = c(schema_tbls, i_tbl)
    }

  }

  # check user-defined tables
  user_defined_tables = ddl.user_defined_tables()
  for(i in 1:length(user_defined_tables)){
    i_tbl = user_defined_tables[i]
    if(i_tbl %in% schema_tbls){
      message(Sys.time(), ": found ", i_tbl)
    } else {
      warning(paste("could not find user-defined table:", i_tbl))
    }
  }

  return(schema_tbls)

}

# table ddl ----

# this section defines built in tables both in sql and using bigQueryR

ddl.treatment = function(){

  # -- treatment
  # CREATE OR REPLACE TABLE
  # example_act.treatment (
  #   treatment_id              INT64
  #   , name                      STRING
  #   , create_datetime           TIMESTAMP
  # );

  bigQueryR::bqr_create_table(
    projectId = Sys.getenv("bq_projectID"),
    datasetId = Sys.getenv("bq_dataSet"),
    tableId = "treatment",
    template_data = data.table(
      treatment_id = as.integer(0),
      name = "template",
      create_datetime = Sys.time()
    )
  )

}

ddl.experiment = function(){
  # -- experiment
  # CREATE OR REPLACE TABLE
  # example_act.experiment (
  #   experiment_id INT64
  #   , name STRING
  #   , audience_id INT64
  #   , control_treatment_id INT64
  #   , primary_impact_variable STRING
  #   , start_datetime TIMESTAMP
  #   , end_datetime TIMESTAMP
  #   , create_datetime TIMESTAMP
  #   , modify_datetime TIMESTAMP
  # );

  bigQueryR::bqr_create_table(
    projectId = Sys.getenv("bq_projectID"),
    datasetId = Sys.getenv("bq_dataSet"),
    tableId = "experiment",
    template_data = data.table(
      experiment_id = as.integer(0),
      name = "template",
      audience_id = as.integer(0),
      control_treatment_id = as.integer(0),
      primary_impact_variable = "template",
      start_datetime = Sys.time(),
      end_datetime = Sys.time(),
      create_datetime = Sys.time(),
      modify_datetime = Sys.time()
    )
  )
}

ddl.experiment_treatment = function(){

  # CREATE OR REPLACE TABLE
  # example_act.experiment_treatment (
  #   experiment_treatment_id   INT64
  #   , experiment_id             INT64
  #   , treatment_id              INT64
  #   , sample_weight             FLOAT64
  #   , create_datetime           TIMESTAMP
  # );

  bigQueryR::bqr_create_table(
    projectId = Sys.getenv("bq_projectID"),
    datasetId = Sys.getenv("bq_dataSet"),
    tableId = "experiment_treatment",
    template_data = data.table(
      experiment_treatment_id = as.integer(0),
      experiment_id = as.integer(0),
      treatment_id = as.integer(0),
      sample_weight = 0.5,
      create_datetime = Sys.time()
    )
  )

}

ddl.audience = function(){

  # -- audience
  # CREATE OR REPLACE TABLE
  # example_act.audience (
  #   audience_id               INT64
  #   , name                      STRING
  #   , create_datetime           TIMESTAMP
  # );

  bigQueryR::bqr_create_table(
    projectId = Sys.getenv("bq_projectID"),
    datasetId = Sys.getenv("bq_dataSet"),
    tableId = "audience",
    template_data = data.table(
      audience_id = as.integer(0),
      name = "template",
      create_datetime = Sys.time()
    )
  )

}

ddl.audience_filter = function(){

  # -- audience_filter
  # CREATE OR REPLACE TABLE
  # example_act.audience_filter (
  #   audience_filter_id        INT64
  #   , audience_id               INT64
  #   , filter_on                 STRING
  #   , comparator_sql            STRING
  #   , comparator_params         STRING
  #   , create_datetime           TIMESTAMP
  # );

  bigQueryR::bqr_create_table(
    projectId = Sys.getenv("bq_projectID"),
    datasetId = Sys.getenv("bq_dataSet"),
    tableId = "audience_filter",
    template_data = data.table(
      audience_filter_id = as.integer(0),
      audience_id = as.integer(0),
      filter_on = "template",
      comparator_sql = "template",
      comparator_params = "template",
      create_datetime = Sys.time()
    )
  )

}

ddl.experiment_audience = function(){

  # -- experiment_audience
  # CREATE OR REPLACE TABLE
  # example_act.experiment_audience (
  #   experiment_audience_id    INT64
  #   , experiment_id             INT64
  #   , user_id                   INT64
  #   , treatment_id              INT64
  #   , active_fg                 STRING
  #   , create_datetime           TIMESTAMP
  #   , modified_datetime         TIMESTAMP
  # );
  #

  bigQueryR::bqr_create_table(
    projectId = Sys.getenv("bq_projectID"),
    datasetId = Sys.getenv("bq_dataSet"),
    tableId = "experiment_audience",
    template_data = data.table(
      experiment_audience_id = as.integer(0),
      experiment_id = as.integer(0),
      unit_id = "template",
      treatment_id = as.integer(0),
      active_fg = "template",
      create_datetime = Sys.time(),
      modified_datetime = Sys.time()
    )
  )

}
