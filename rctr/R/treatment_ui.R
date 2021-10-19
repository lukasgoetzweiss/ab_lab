#' Treatment UI
#'
#' Main treatment panel UI
#'
#' @import shiny DT
#' @export
#'
#' @examples
treatment_ui = function(){
  tabPanel(
    "Treatment",
    DTOutput('treatment'),
    actionButton("createTreatment", "Create New", icon = icon("fas fa-plus"))
  )
}
