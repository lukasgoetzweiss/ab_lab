#' Pulls timeseries data for analysis and plotting
#'
#' @param dataset BigQuery dataset
#' @param experiment_name experiment to analyze
#' @param impact_variable variable to be measured
#' @param pre_period_days how far into the past to pull pre-period data
#'
#' @return timeseries data
#' @import data.table ggplot2
#' @export
get_timeseries_impact_data = function(experiment_id,
                                      impact_variable,
                                      pre_period_days = 14){

  ts_timestamp = glue(
    "cast(ts.{Sys.getenv('timeseries_timestamp')} as TIMESTAMP)"
  )

  return(pull_data(glue::glue(
    " select {ts_timestamp}               as timestamp
          ,  ea.treatment_id
          ,  t.name                       as treatment
          ,  avg(ts.{impact_variable})    as impact_metric
        from {Sys.getenv('bq_dataSet')}.{Sys.getenv('timeseries_table')} ts
        join {Sys.getenv('bq_dataSet')}.experiment_audience ea
          on ea.unit_id = ts.{Sys.getenv('unit_pk')}
         and ea.experiment_id = {experiment_id}
        join {Sys.getenv('bq_dataSet')}.experiment e
          on e.experiment_id = ea.experiment_id
         and {ts_timestamp} > date_add(e.start_datetime, INTERVAL -{pre_period_days} DAY)
         and {ts_timestamp} < e.end_datetime
        join {Sys.getenv('bq_dataSet')}.treatment t
          on t.treatment_id = ea.treatment_id
       group by 1,2,3"
  )))
}

#' Plots timeseries impact
#'
#' @param timeseries_impact_data output of get_timeseries_impact_data
#' @param experiment_name experiment name (for plot)
#' @param impact_variable impact variable name (for plot_)
#' @param colors line colors for plot
#'
#' @return
#' @export
plot_timeseries_impact = function(timeseries_impact_data,
                                  experiment_name,
                                  experiment_start,
                                  impact_variable,
                                  colors = c("#F18E7E", "#69AEDB", "grey10")){

  if(is.null(timeseries_impact_data)){
    return(
      ggplot() + ggtitle("") + theme(panel.background = element_blank())
    )
  }

  return(
    ggplot(timeseries_impact_data, aes(timestamp, impact_metric, color = treatment)) +
      geom_line(size = 1.5) +
      geom_vline(linetype = "dashed", size = 1,
                 mapping = aes(color = "Intervention",
                               xintercept = experiment_start)) +
      guides(linetype = F) +
      xlab("") +
      ylab(impact_variable) +
      ggtitle(str_c("Impact on ", impact_variable, " from ", experiment_name)) +
      scale_color_manual(name = "", values = colors) +
      theme(legend.position = "bottom",
            panel.background = element_blank(),
            panel.grid.major.y = element_line(color = "grey90"),
            panel.grid.major.x = element_line(color = "grey90"),
            text = element_text(size = 20, family = "mono"))
  )
}
