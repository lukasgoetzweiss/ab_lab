#' Pulls timeseries data for analysis and plotting
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
                                      pre_period_days = 14){

  ts_vars = paste(
    glue::glue(
      "avg(coalesce(ts.{impact_variable}, 0)) as {impact_variable}"
    ), collapse = ", "
  )

  return(pull_data(glue::glue(
    " with grid as (
          select ea.unit_id           as unit_id
              ,  t_ax.date
              ,  ea.treatment_id
              ,  t.name                       as treatment

            from {Sys.getenv('bq_dataSet')}.experiment_audience ea

      cross join (select *
                    from UNNEST(
                      GENERATE_DATE_ARRAY(
                        date_add((select cast(start_datetime as date)
                                    from {Sys.getenv('bq_dataSet')}.experiment
                                   where experiment_id = {experiment_id}),
                                 interval {-pre_period_days} day),
                        current_date - 1
                      )
                    ) as date
                  ) t_ax

            join {Sys.getenv('bq_dataSet')}.treatment t
              on t.treatment_id = ea.treatment_id

           where ea.experiment_id = {experiment_id}
      )

      select g.date
          ,  g.treatment_id
          ,  g.treatment
          ,  {ts_vars}
        from grid g

   left join {Sys.getenv('bq_dataSet')}.{Sys.getenv('timeseries_table')} ts
          on g.unit_id = ts.{Sys.getenv('unit_pk')}
         and g.date = ts.{Sys.getenv('timeseries_timestamp')}

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
