load_sizing_data = function(impact_variable, start_datetime, end_datetime){
  return(pull_data(glue(
  # message(glue(
    "with cte as (
    select {Sys.getenv('unit_pk')}
            --, {Sys.getenv('timeseries_timestamp')}
            --, {impact_variable}
              , sum({impact_variable})    as iv
            --, stddev({impact_variable})   as sigma
          from {Sys.getenv('bq_dataSet')}.{Sys.getenv('timeseries_table')}
         where {Sys.getenv('timeseries_timestamp')} >= cast('{start_datetime}' as DATE)
           and {Sys.getenv('timeseries_timestamp')} > cast('{end_datetime}' as DATE)
         group by 1
    )
    select avg(iv) as mu, stddev(iv) as sigma from cte"
  )))
}

# compute_mdr(100, sizing_data_tmp$mu, sizing_data_tmp$sigma, 0.05, 0.8)

compute_mdr = function(N, mu, sigma, alpha, power, plot = T){


  # dev
  power.t.test(n = 50, sd = 1, sig.level = 0.05, power = 0.8)

  p_control = 0.5
  (pnorm(0.95) + pnorm(0.2)) *
    (sqrt(1 / (p_control*(1-p_control)))) *
    sqrt(1 / 50)


  mdr = power.t.test(n = N / 2, sd = sigma, sig.level = alpha, power = power)

  if(plot == F){
    
    return(mdr)
    
  } else {

    xax = seq(mu - 3 * sigma / sqrt(N/2),
              mu + mdr$delta + 3 * sigma / sqrt(N/2),
              length.out = 250)

    ggdata = rbind(
      data.table(x = xax, y = dnorm(xax, mu, sd = sigma / sqrt(N/2)),
                 gp = "H_0"),
      data.table(x = xax,
                 y = dnorm(xax, mu +  mdr$delta, sd = sigma / sqrt(N/2)),
                 gp = "H_1")
    )

    return(list(
      mdr = mdr,
      plot = ggplot(ggdata, aes(x, y, color = gp, fill = gp)) +
        geom_area(alpha = 0.5, position = "identity") +
        scale_fill_manual(name = "", values = c("grey60", rctr_colors()[1])) +
        scale_color_manual(name = "", values = c("grey60", rctr_colors()[1])) +
        scale_y_continuous(name = "Density", labels = NULL) +
        theme(legend.position = "bottom",
              panel.background = element_blank(),
              panel.grid.major.y = element_line(color = "grey90"),
              panel.grid.major.x = element_line(color = "grey90"),
              text = element_text(size = 20, family = "mono"))
    ))

  }

}

get_audience_size = function(auidence_filter_record){
  fltr_sql =
    auidence_filter_record[, paste(comparator_sql, collapse = " and \n")]

  audience_size = pull_data(glue(
    "select count(*) as n
       from {Sys.getenv('bq_dataSet')}.{Sys.getenv('segment_table')}
      where {fltr_sql}"
  ))[, n]

  return(audience_size)
}
