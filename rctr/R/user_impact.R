#' Pulls user-level data to analyze test results
#'
#' @param dataset Big Query dataset
#' @param experiment_name experiment to analyze
#' @param impact_variable variable to be measured
#' @param pre_period_days how far into the past to pull pre-period data
#'
#' @return user_impact_data
#' @import data.table
#' @export
get_user_impact_data = function(experiment_id,
                                impact_variable,
                                pre_period_days = 14){

  ts_timestamp = glue(
    "cast(ts.{Sys.getenv('timeseries_timestamp')} as TIMESTAMP)"
  )

  return(pull_data(glue(
    " select ts.{Sys.getenv('unit_pk')}   as unit_id
          ,  ea.treatment_id
          ,  case when {ts_timestamp} > e.start_datetime
                  then 'post'
                  else 'pre' end          as period
          ,  t.name                       as treatment
          ,  avg(ts.{impact_variable})    as impact_variable
        from {Sys.getenv('bq_dataSet')}.{Sys.getenv('timeseries_table')} ts
        join {Sys.getenv('bq_dataSet')}.experiment_audience ea
          on ea.unit_id = ts.{Sys.getenv('unit_pk')}
         and ea.experiment_id = {experiment_id}
        join {Sys.getenv('bq_dataSet')}.experiment e
          on e.experiment_id = ea.experiment_id
         #and {ts_timestamp} > date_add(e.start_datetime, INTERVAL -{pre_period_days} DAY)
         and {ts_timestamp} < e.end_datetime
        join {Sys.getenv('bq_dataSet')}.treatment t
          on t.treatment_id = ea.treatment_id
       group by 1,2,3,4"
  )))

}

#' Analyzes output of get_user_impact_data
#'
#' @param user_impact_data Output of get_user_impact_data
#'
#' @return text description of test results
#' @export

measure_user_impact = function(user_impact_data){

  if(is.null(user_impact_data)){
    return("Press Measure Impact to get results")
  }

  pre.tt = stats::t.test(
    user_impact_data[period == "pre" & treatment == "Control", impact_variable],
    user_impact_data[period == "pre" & treatment != "Control", impact_variable]
  )

  post.tt = stats::t.test(
    user_impact_data[period == "post" & treatment == "Control", impact_variable],
    user_impact_data[period == "post" & treatment != "Control", impact_variable]
  )

  impact_point_est = stringr::str_c(
    "    Control: ",
    signif(pre.tt$estimate[1]), " -> ", signif(post.tt$estimate[1]),
    " (", fmt_lift(post.tt$estimate[1] / pre.tt$estimate[1] - 1), ")\n",
    "       Test: ",
    signif(pre.tt$estimate[2]), " -> ", signif(post.tt$estimate[2]),
    " (", fmt_lift(post.tt$estimate[2] / pre.tt$estimate[2] - 1), ")"
  )

  lift.pre = pre.tt$estimate[2] / pre.tt$estimate[1] - 1
  lift.post = post.tt$estimate[2] / post.tt$estimate[1] - 1

  tt_res = stringr::str_c(
    " Pre period: test ",
    scales::percent(abs(lift.pre)), " ", comp_str(lift.pre),
    ", p = ", signif(pre.tt$p.value, 4), "\n",
    "Post period: test ",
    scales::percent(abs(lift.post)), " ", comp_str(lift.post),
    ", p = ", signif(post.tt$p.value, 4), "\n\n",
    "95% confidence interval for impact: ", format_tt_ci(post.tt)
  )

  return(stringr::str_c(impact_point_est, "\n\n", tt_res))

}

# helper functions

fmt_lift = function(x){
  stringr::str_c(ifelse(x > 0, "+", ""), scales::percent(x))
}

comp_str = function(x){
  ifelse(x<0, "lower than control", "higher than control")
}

format_tt_ci = function(tt){
  stringr::str_c("(",
        scales::percent(-tt$conf.int[2] / tt$estimate[1]),
        ", ",
        scales::percent(-tt$conf.int[1] / tt$estimate[1]),
        ")"
  )
}
