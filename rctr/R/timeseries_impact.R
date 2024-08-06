#' Pulls time series data for analysis and plotting
#'
#' @param dataset BigQuery dataset
#' @param impact_variable variable to be measured
#' @param pre_period_days how far into the past to pull pre-period data
#'
#' @return timeseries data
#' @import data.table ggplot2
#' @export
get_timeseries_impact_data = function(experiment_id,
                                      impact_variable,
                                      pre_period_days = 14,
                                      end_date = "current_date - 1"){

  if(is.Date(end_date)){
    end_date = glue("'{end_date}'")
  }

  ts_vars = paste(
    glue::glue(
      "avg(coalesce(ts.{impact_variable}, 0)) as {impact_variable}"
    ), collapse = ", "
  )
  
  experiment_start = pull_data(glue::glue(
    'select cast(start_datetime as date) as start_date
       from experiment
      where experiment_id = {experiment_id}'
  ))[, lubridate::date(start_date)]
  
  experiment_end = pull_data(glue::glue(
    'select cast(end_datetime as date) as end_date
       from experiment
      where experiment_id = {experiment_id}'
  ))[, lubridate::date(end_date)]
  
  date_sql = stringr::str_c(
    "select '",
    paste(
      seq.Date(from = experiment_start - lubridate::days(pre_period_days), 
               to = experiment_end, 
               by = "day"),
      collapse = "' as date \n union all \n select '"
    ),
    "' as date"
  )

  return(pull_data(glue::glue(
    " with grid as (
          select ea.unit_id           as unit_id
              ,  t_ax.date
              ,  ea.treatment_id
              ,  t.name                       as treatment

            from experiment_audience ea

      cross join (select *
                    from ({date_sql})
                  ) t_ax

            join treatment t
              on t.treatment_id = ea.treatment_id

           where ea.experiment_id = {experiment_id}
      )

      select g.date
          ,  g.treatment_id
          ,  g.treatment
          ,  {ts_vars}
        from grid g

   left join {Sys.getenv('timeseries_table')} ts
          on g.unit_id = ts.{Sys.getenv('unit_pk')}
         and g.date = date('1970-01-01', '+' || ts.{Sys.getenv('timeseries_timestamp')} || ' days')

       group by 1,2,3"
  )))
}

#' Plots timeseries impact
#'
#' @param timeseries_impact_data output of get_timeseries_impact_data
#' @param impact_variable impact variable name (for plot_)
#' @param colors line colors for plot
#'
#' @return
#' @export
plot_timeseries_impact = function(timeseries_impact_data,
                                  experiment_start,
                                  colors = rctr_colors()){

  if(is.null(timeseries_impact_data)){
    return(
      ggplot() + ggtitle("") + theme(panel.background = element_blank())
    )
  }

  experiment_start = lubridate::as_date(experiment_start)

  ts_melt = melt(timeseries_impact_data[, !"treatment_id"],
                 id.vars = c("date", "treatment"))
  
  ts_melt[, date := lubridate::date(date)]

  return(
    ggplot(ts_melt, aes(date, value, color = treatment)) +
      geom_vline(linetype = "dashed", size = 1,
                 mapping = aes(color = "Intervention",
                               xintercept = experiment_start)) +
      geom_line(size = 1.5) +
      guides(linetype = F) +
      xlab("") +
      ylab("") +
      scale_color_manual(name = "", values = colors) +
      theme(legend.position = "bottom",
            panel.background = element_blank(),
            panel.grid.major.y = element_line(color = "grey90"),
            panel.grid.major.x = element_line(color = "grey90"),
            text = element_text(size = 14, family = "sans"),
            strip.background = element_blank()) +
      facet_wrap(~variable, ncol = 1, scales = "free")
  )
}
