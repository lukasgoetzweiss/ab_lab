# this file contains functions to support viewing audiences

audienceRowsSelectedObs = function(input, output, rv){
  observeEvent(input$audience_rows_selected, {
    if(input$audience_rows_selected > 0){
      output$audienceSelected = renderDT(
        rv$audienceFilterAll[
          audience_id %in% rv$audience[
            input$audience_rows_selected,
            audience_id
          ]
        ]
      )
    } else {
      output$audienceSelected = renderDT(data.table())
    }

  })
}
