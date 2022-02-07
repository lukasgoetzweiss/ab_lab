# this file contains functions to support viewing audiences

audienceRowsSelectedObs = function(input, output, rv){
  observeEvent(input$audience_rows_selected, {
    if(input$audience_rows_selected > 0){

      output$audienceSelectedSql = renderText(
        format_audience_query(
          rv$audienceFilterAll[
            audience_id %in% rv$audience[
              input$audience_rows_selected,
              audience_id
            ]
          ]
        )
      )

    } else {
      output$audienceSelectedSql = renderText("")
    }

  })
}

format_audience_query = function(x){

  if(is.null(x) || nrow(x) == 0){
    return("")
  } else {

    x_ = x[, comparator_sql]

    where_claus = paste(x_, collapse = '\n     and ')

    glue(
      "  select {Sys.getenv('unit_pk')}
    from {Sys.getenv('segment_table')}
   where {where_claus}",
      .trim = F
    )

  }
}
