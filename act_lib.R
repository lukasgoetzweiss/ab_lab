# act_lib.R

library(bigrquery)
library(bigQueryR)
library(data.table)
library(lubridate)
library(glue)
library(ggplot2)

#### set environment ----

Sys.setenv(bq_projectID = "hazel-champion-318400")
Sys.setenv(bq_dataSet = "example_act")

# authenticate
bigQueryR::bqr_auth(json_file = "~/Downloads/hazel-champion-318400-412f14ac362f.json")

#### data util ----

pull_data = function(sql, projectID = Sys.getenv("bq_projectID")){
  tb <- bq_project_query(projectID, sql)
  res = bq_table_download(tb)
  return(data.table(res))
}

load_data = function(tableId, upload_data, 
                     dataSet = Sys.getenv("bq_dataSet"), 
                     projectID = Sys.getenv("bq_projectID"),
                     writeDisposition = "WRITE_APPEND",
                     schema = NULL){
  
  # insert_upload_job("your-project-id", "test_dataset", "stash", stash)
  
  bqr_upload_data(
    projectId = projectID,
    datasetId = dataSet, 
    tableId = tableId,
    upload_data = upload_data,
    writeDisposition = writeDisposition, 
    schema = schema,
    autodetect = F
  )
  
  
  # tmpFile = tempfile(tmpdir = "~/Downloads/tmp", fileext = ".csv")
  # fwrite(data, tmpFile, col.names = F)
  # 
  # bash_cmd = glue(
  #   "bq load --source_format=CSV {projectID}:{dataSet}.{dataTable} {tmpFile}"
  # )
  # 
  # message(bash_cmd)
  # message("rm ", tmpFile)
  
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

get_treatment = function(dataset = Sys.getenv("bq_dataSet")){
  data.table(pull_data(glue("select * from {dataset}.treatment")))
}

create_treatment = function(name, dataset = Sys.getenv("bq_dataSet")){
  
  treatment_id = pull_data(glue(
    "select max(treatment_id)  from {dataset}.treatment"
  ))$f0_[1] + 1
  if(is.na(treatment_id)){ treatment_id = 1 }
  
  message("creating treatment ", name, ", treatment_id = ", treatment_id)
  
  load_data(
    "treatment",
    data.table(
      treatment_id = as.integer(treatment_id),
      name = name,
      create_datetime = Sys.time()
      )
  )
  
  return(treatment_id)
  
}

#### audience ----

get_audience = function(dataset = Sys.getenv("bq_dataSet")){
  data.table(pull_data(glue("select * from {dataset}.audience")))
}

get_audience_filter = function(dataset = Sys.getenv("bq_dataSet")){
  data.table(pull_data(glue("select * from {dataset}.audience_filter")))
}

create_audience = function(name, audience_filter,
                           dataset = Sys.getenv("bq_dataSet")){
  audience_id = pull_data(glue(
    "select max(audience_id)  from {dataset}.audience"
  ))$f0_[1] + 1
  if(is.na(audience_id)){ audience_id = 1 }
  
  message("creating audience ", name, ", audience_id = ", audience_id)
  
  load_data(
    "audience",
    data.table(
      audience_id = as.integer(audience_id),
      name = name,
      create_datetime = Sys.time()
    )
  )
  
  audience_filter_id = pull_data(glue(
    "select max(audience_filter_id)  from {dataset}.audience_filter"
  ))$f0_[1]
  if(is.na(audience_filter_id)){ audience_filter_id = 0 }
  
  audience_filter_id = audience_filter_id + (1:audience_filter[, .N])
  
  load_data(
    "audience_filter",
    audience_filter[, .(audience_filter_id = as.integer(audience_filter_id),
                        audience_id = as.integer(audience_id),
                        filter_on,
                        comparator_sql,
                        comparator_params,
                        create_datetime = Sys.time())]
  )
  
  return(audience_id)
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




#### experiment ----

get_experiment = function(dataset = Sys.getenv("bq_dataSet")){
  data.table(pull_data(glue("select * from {dataset}.experiment")))
}

get_experiment_treatment = function(dataset = Sys.getenv("bq_dataSet")){
  data.table(pull_data(glue("select * from {dataset}.experiment_treatment")))
}

get_experiment_audience = function(dataset = Sys.getenv("bq_dataSet")){
  data.table(pull_data(glue("select * from {dataset}.experiment_audience")))
}

create_experiment = function(name, 
                             audience_id, experiment_treatment,
                             start_datetime, end_datetime,
                             dataset = Sys.getenv("bq_dataSet")){
  
  experiment_id = pull_data(glue(
    "select max(experiment_id)  from {dataset}.experiment"
  ))$f0_[1] + 1
  if(is.na(experiment_id)){ experiment_id = 1 }
  
  message("creating experiment ", name, ", experiment_id = ", experiment_id)
  
  load_data(
    "experiment",
    experiment_treatment[, .(
      experiment_id = as.integer(experiment_id),
      audience_id = as.integer(audience_id),
      name = name,
      start_datetime = as.POSIXct(start_datetime),
      end_datetime = as.POSIXct(end_datetime),
      create_datetime = Sys.time()
    )]
  ) 
  
  experiment_treatment_id = pull_data(glue(
    "select max(experiment_treatment_id)  from {dataset}.experiment_treatment"
  ))$f0_[1]
  if(is.na(experiment_treatment_id)){ experiment_treatment_id = 0 }
  
  experiment_treatment_id = experiment_treatment_id + (1:experiment_treatment[, .N])
  
  load_data(
    "experiment_treatment",
    experiment_treatment[, .(
      experiment_treatment_id = as.integer(experiment_treatment_id),
      experiment_id = as.integer(experiment_id),
      treatment_id,
      sample_weight = weight / sum(weight),
      create_datetime = Sys.time()
    )]
  )
  
  return(experiment_id)
}

format_audience_experiment = function(experiment_treatment, treatment){
  
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

# analysis ----

gg_colors = c("#F18E7E", "#69AEDB", "#87D4C4", "#BAB5BA")

get_analysis_data = function(dataset = Sys.getenv("bq_dataSet")){
  return(pull_data(glue(
    " select ud.user_id
          ,  ea.treatment_id
          ,  case when ud.timestamp > e.start_datetime
                  then 'post'
                  else 'pre' end          as period
          ,  t.name                       as treatment
          ,  sum(ud.units_sold)
        from example_data.user_data ud
        join {dataset}.experiment_audience ea
          on ea.user_id = ud.user_id
         and ea.experiment_id = 1
        join {dataset}.experiment e
          on e.experiment_id = ea.experiment_id
         and ud.timestamp > date_add(e.start_datetime, INTERVAL -14 DAY)
         and ud.timestamp < e.end_datetime
        join {dataset}.treatment t
          on t.treatment_id = ea.treatment_id
       group by 1,2,3,4"
  )))
}

get_analysis_timeseries = function(dataset = Sys.getenv("bq_dataSet")){
  return(pull_data(glue(
    " select ud.timestamp
          ,  ea.treatment_id
          ,  t.name                       as treatment
          ,  avg(ud.units_sold)           as impact_metric
        from example_data.user_data ud
        join {dataset}.experiment_audience ea
          on ea.user_id = ud.user_id
         and ea.experiment_id = 1
        join {dataset}.experiment e
          on e.experiment_id = ea.experiment_id
         and ud.timestamp > date_add(e.start_datetime, INTERVAL -14 DAY)
         and ud.timestamp < e.end_datetime
        join {dataset}.treatment t
          on t.treatment_id = ea.treatment_id
       group by 1,2,3"
  )))
}

# analysis_timeseries = get_analysis_timeseries()
# 
# ggplot(analysis_timeseries, aes(timestamp, impact_metric, color = treatment)) + 
#   geom_line(size = 1) + 
#   xlab("") + 
#   scale_color_manual(name = "Treatment", values = gg_colors) + 
#   theme(panel.background = element_blank(),
#         panel.grid.major.y = element_line(color = "grey90"),
#         panel.grid.major.x = element_line(color = "grey90"))
# 
# analysis_data = get_analysis_data()
# 
# t.test(analysis_data[period == "pre" & treatment == "Control", f0_],
#        analysis_data[period == "pre" & treatment != "Control", f0_])
# 
# t.test(analysis_data[period == "post" & treatment == "Control", f0_],
#        analysis_data[period == "post" & treatment != "Control", f0_])

# util ----

get_user = function(){
  return(data.table(pull_data("select * from example_data.user")))
}
