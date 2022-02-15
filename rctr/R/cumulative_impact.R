# this file contains functions to pull, analyze, and visualize data on the
# cumulative impact of an experiments

# pulls cumulative impact data from DB
get_cumulative_impact_data = function(experiment_id,
                                      impact_variable,
                                      max_horizon = 14,
                                      horizon_step = 1){

  # format impact variables as a sql statement
  cm_vars = paste(
    glue::glue(
      "sum(coalesce(ts.{impact_variable}, 0)) as {impact_variable}"
    ), collapse = ", "
  )

  return(pull_data(glue::glue(

      " with grid as (
      select ea.unit_id           as unit_id
          ,  ea.treatment_id
          ,  h.horizon
          ,  e.start_datetime
        from {Sys.getenv('bq_dataSet')}.experiment_audience ea
        join {Sys.getenv('bq_dataSet')}.experiment e
          on ea.experiment_id = e.experiment_id
  cross join (select horizon
                from UNNEST(GENERATE_ARRAY(1, {max_horizon}, {horizon_step})) as horizon
             ) h
       where e.experiment_id = {experiment_id}
    )

  select g.unit_id
      ,  g.treatment_id
      ,  g.horizon
      ,  {cm_vars}
    from grid g
    left join {Sys.getenv('bq_dataSet')}.{Sys.getenv('timeseries_table')} ts
      on g.unit_id = ts.{Sys.getenv('unit_pk')}
     and ts.{Sys.getenv('timeseries_timestamp')} >= cast(g.start_datetime as date)
     and ts.{Sys.getenv('timeseries_timestamp')} <= cast(date_add(g.start_datetime, INTERVAL g.horizon day) as date)
   group by 1,2,3;"
  )))

}

# analyzes cumulative impact data, computes lift, significance, and confidence
# intervals
compute_cumulative_impact = function(cumulative_impact_data){

  # check if data is valide
  if(is.null(cumulative_impact_data) ||
     !any(complete.cases(cumulative_impact_data))){
    return(NULL)
  }

  # get a list of metrics to measure
  meas_vars = setdiff(
    names(cumulative_impact_data),
    c("unit_id", "treatment_id", "horizon")
  )

  # run a t test for each horizon-variable pair and store result
  meas_data = NULL
  for(j in meas_vars){
    for(i in cumulative_impact_data[, unique(horizon)]){
      i_tt = t.test(
        cumulative_impact_data[treatment_id == 1 & horizon == i, get(j)],
        cumulative_impact_data[treatment_id != 1 & horizon == i, get(j)]
      )
      meas_data = rbind(
        meas_data,
        data.table(
          var = j,
          horizon = i,
          control = i_tt$estimate[1],
          test = i_tt$estimate[2],
          p_val = i_tt$p.value,
          lift = i_tt$estimate[2] / i_tt$estimate[1] - 1,
          ci_l = -i_tt$conf.int[2] / i_tt$estimate[1],
          ci_u = -i_tt$conf.int[1] / i_tt$estimate[1]
        )
      )
    }
  }

  return(meas_data[order(var, horizon)])

}

# visualize results of cumulative impact analysis
plot_cumulative_impact = function(cumulative_impact){

  # return empty plot if cumulative impact data hasn't been loaded
  if(is.null(cumulative_impact)){
    return(
      ggplot() + ggtitle("Results will appear once experiment starts") +
        theme(panel.background = element_blank())
    )
  }

  return(
    ggplot(cumulative_impact, aes(horizon)) +
      geom_point(aes(y = lift, color = p_val < 0.05)) +
      geom_segment(aes(y = 0, yend = lift, xend = horizon, color = p_val < 0.05)) +
      geom_crossbar(aes(y = lift, ymin = ci_l, ymax = ci_u, fill = p_val < 0.05),
                    alpha = 0.2, color = NA) +
      geom_hline(yintercept = 0, color = "grey75") +
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_line(color = "grey90"),
            legend.position = "bottom",
            strip.background = element_blank(),
            text = element_text(size = 14, family = "sans")) +
      scale_x_continuous("Horizon (days)") +
      scale_y_continuous(name = "Lift", labels = scales::percent) +
      scale_fill_manual(name = "Stat Sign",
                        breaks = c(T, F),
                        values = c(rctr_colors()[2], "grey60")) +
      scale_color_manual(name = "Stat Sign",
                         breaks = c(T, F),
                         values = c(rctr_colors()[2], "grey60")) +
      facet_wrap(~var, nrow = 1)
  )

}

# formats cumulative impact analysis result for a specified horizon
format_cumulative_impact = function(cumulative_impact_data, horizon_select){

  if(is.null(cumulative_impact_data)){
    return(data.table())
  } else {
    return(
      compute_cumulative_impact(cumulative_impact_data)[
        horizon %in% horizon_select,
        .(var,
          control = signif(control, 4),
          test = signif(test, 4),
          lift = stringr::str_c(signif(test - control, 4),
                                " (",
                                scales::percent(test / control - 1, accuracy = 0.1),
                                ")"),
          conf_int = stringr::str_c(
            "(", scales::percent(ci_l, accuracy = 0.1), ", ",
            scales::percent(ci_u, accuracy = 0.1), ")"
          )
        )
      ]
    )
  }
}
