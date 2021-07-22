# act_lib.R

library(bigrquery)
library(data.table)
library(glue)
library(ggplot2)

#### definitions ----

Sys.setenv(bq_projectID = "hazel-champion-318400")
Sys.setenv(bq_dataSet = "example_act")

#### data util ----

pull_data = function(sql, projectID = Sys.getenv("bq_projectID")){
  tb <- bq_project_query(projectID, sql)
  res = bq_table_download(tb)
  return(res)
}

load_data = function(data, dataTable, 
                     dataSet = Sys.getenv("bq_dataSet"), 
                     projectID = Sys.getenv("bq_projectID")){
  
  # insert_upload_job("your-project-id", "test_dataset", "stash", stash)
  
  tmpFile = tempfile(tmpdir = "~/Downloads/tmp", fileext = ".csv")
  fwrite(data, tmpFile, col.names = F)
  
  bash_cmd = glue(
    "bq load --source_format=CSV {projectID}:{dataSet}.{dataTable} {tmpFile}"
  )
  
  message(bash_cmd)
  message("rm ", tmpFile)
  
  return()
  
  # this would be the better way, but doesn't seem to be working...
  
  # system2(bash_cmd)
  # file.remove(tmpFile)
  
}

create_metadata = function(user = NULL){
  
  if(is.null(user)){
    user = get_user()
  }
  
  user_metric = NULL
  user_variable = NULL
  user_variable_value = NULL
  
  for(col_name in setdiff(names(user), "user_id")){
    if(user[, is.Date(get(col_name))]){
      message("parsing ", col_name, " as date")
    } else if (user[, mode(get(col_name))] == "character"){
      message("parsing ", col_name, " as character")
      user_variable = rbind(user_variable, data.table(name = col_name))
      user_variable_value = rbind(
        user_variable_value,
        data.table(
          variable = col_name,
          value = user[, unique(get(col_name))]
        )
      )
    } else if (user[, mode(get(col_name))] == "numeric") {
      message("parsing ", col_name, " as numeric") 
      user_metric = rbind(
        user_metric,
        data.table(
          name = col_name,
          metric_min = user[, min(get(col_name))],
          metric_max = user[, max(get(col_name))]
        )
      )
    } else {
      message("failed to parse ", col_name)
    }
  }
  
  return(list(
    "user_metric" = user_metric,
    "user_variable" = user_variable,
    "user_variable_value" = user_variable_value
  ))
  
}

#### treatment ----

create_treatment = function(name, database = Sys.getenv("bq_dataSet")){
  treatment_id = pull_data(glue(
    "select max(treatment_id)  from {database}.treatment"
  ))$f0_[1] + 1
  if(is.na(treatment_id)){ treatment_id = 1 }
  message("creating treatment ", name, ", treatment_id = ", treatment_id)
  load_data(
    data.table(
      treatment_id = treatment_id,
      name = name,
      create_datetime = Sys.time()
      ), 
    "treatment"
  )
  return(treatment_id)
}

get_treatment = function(database = Sys.getenv("bq_dataSet")){
  data.table(pull_data(glue("select * from {database}.treatment")))
}

#### audience ----

create_audience_metric_filter = function(metric, filter_variable, filter_range){
  
  return(
    list(
      plot = ggplot(metric[variable == filter_variable],
             aes(value, fill = value %between% filter_range)) +
        geom_histogram() + 
        scale_fill_manual(name = "", values = c("grey80", "grey40")),
      filter = data.table(
        source_table = "user_metric", 
        filter_variable = filter_variable, 
        comparator = glue("value >= {filter_range[1]} and value <= {filter_range[2]}")
      )
    )
  )
  
  
  # ex_audience_filter = data.table(
  #   source_table = "user_variable", 
  #   source_table_id = 1, 
  #   comparator = "in ('Here', 'There')"
  # )
  
}

apply_filter = function(user, filter_params){
  message("applying filter ", filter_params)
  
  fp = strsplit(filter_params, ":")[[1]]
  filter_type = fp[1]
  
  if(filter_type == "m"){
    user[get(fp[2]) < fp[3] | get(fp[2]) > fp[4], incl := F]
    
  } else if(filter_type == "v"){
    user[!(get(fp[2]) %in% fp[-c(1,2)]), incl := F]
  } else {
    warning(paste("unable to parse filter_params:", filter_params))
  }
  
  message(user[, scales::percent(mean(incl))], " of users remain")
  
  return(user)
}

create_audience = function(){
  #...
  return(audience_id)
}

get_audience = function(){
  #...
  return(audience)
}

#### experiment ----

create_experiment = function(name, start_datetime, end_datetime){
  
  load_data(
    data.table(
      name = name,
      start_datetime = start_datetime,
      end_datetime = end_datetime
    ), 
    "experiment"
  )
  
  return(experiment_id)
}

get_experiment = function(){
  
  return(experiment)
  
}

# util ----

get_user = function(){
  return(data.table(pull_data("select * from example_data.user")))
}
