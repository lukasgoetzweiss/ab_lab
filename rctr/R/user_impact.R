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

  # format impact variables as a sql statement
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

      select g.unit_id
          ,  case when g.date > (select cast(start_datetime as date)
                                   from {Sys.getenv('bq_dataSet')}.experiment
                                  where experiment_id = {experiment_id})
                  then 'post'
                  else 'pre' end          as period
          ,  g.treatment_id
          ,  g.treatment
          ,  {ts_vars}
        from grid g

   left join {Sys.getenv('bq_dataSet')}.{Sys.getenv('timeseries_table')} ts
          on g.unit_id = ts.{Sys.getenv('unit_pk')}
         and g.date = ts.{Sys.getenv('timeseries_timestamp')}

       group by 1,2,3,4"
  )))

}

# plot either the population distribution (if estimate = F) or the distribution
# of our estimate for the mean
plot_distribution = function(user_impact_data,
                             estimate = F,
                             colors = rctr_colors()){

  # return empty plot is user_impact_data has not been loaded
  if(is.null(user_impact_data)){
    return(
      ggplot() + ggtitle("") + theme(panel.background = element_blank())
    )
  }

  # format data for plotting
  user_melt = melt(user_impact_data[, !"treatment_id"],
                   id.vars = c("unit_id", "period", "treatment"))

  if(estimate == F){
    return(
      ggplot(user_melt,
             aes(value, fill = treatment, color = treatment)) +
        geom_histogram(position = "identity", alpha = 0.5) +
        scale_fill_manual(name = "", values = colors) +
        scale_color_manual(name = "", values = colors) +
        theme(text = element_text(size = 20, family = "sans"),
              legend.position = "bottom",
              strip.background = element_blank(),
              panel.background = element_rect(fill = "grey98"),
              panel.grid.major.y = element_line(color = "grey90"),
              panel.grid.major.x = element_line(color = "grey90")) +
        facet_grid(factor(period, levels = c("pre", "post"))~variable,
                   scales = "free")
    )
  } else {

    # compute sample mean and variance
    estimate = user_melt[, .(mu = mean(value),
                             se = sd(value) / sqrt(.N)),
                         .(treatment, period, variable)]

    # create axis data for plot
    estimate_gg_data = NULL
    for(v in estimate[, unique(variable)]){
      for(pr in c("pre", "post")){
        estimate_gg_data = rbind(
          estimate_gg_data,
          data.table(variable = v,
                     period = pr,
                     x = estimate[variable == v,
                                  seq(min(mu - 3*se), max(mu + 3*se),
                                      length.out = 500)])
        )
      }
    }

    # join axis to estimates
    estimate_gg_data = merge(estimate_gg_data, estimate, allow.cartesian = T)

    return(
      ggplot(estimate_gg_data,
             aes(x, dnorm(x, mu, se) * se, color = treatment, fill = treatment)) +
        geom_line() + geom_area(alpha = 0.5, position = "identity") +
        xlab("Daily Average") +
        scale_fill_manual(name = "", values = colors) +
        scale_color_manual(name = "", values = colors) +
        scale_y_continuous("likelihood", labels = NULL) +
        theme(text = element_text(size = 14, family = "sans"),
              legend.position = "bottom",
              strip.background = element_blank(),
              panel.background = element_rect(fill = "grey98"),
              panel.grid.major.y = element_line(color = "grey90"),
              panel.grid.major.x = element_line(color = "grey90")) +
        facet_grid(factor(period, levels = c("pre", "post"))~variable,
                   scales = "free")
    )

  }

}
