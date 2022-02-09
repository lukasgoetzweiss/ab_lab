#' Audience UI
#'
#' Main audience panel UI
#'
#' @import shiny DT
#' @export

audience_ui = function(){
  navbarMenu(
    "Audience",
    tabPanel(
      "View",
      value = "viewAudience",
      fluidRow(
        column(5,
               DTOutput('audience')),
        column(7,
               verbatimTextOutput('audienceSelectedSql'))
      )
    ),
    tabPanel(
      "Create",
      h2("Audience Filters"),
      verbatimTextOutput("audienceFilterSql"),
      actionButton("createAudienceFilter",
                   "Add Filter",
                   icon = icon("fas fa-plus")),
      actionButton("resetAudienceFilter",
                   "Reset"),
      p(),
      uiOutput("audienceCreateUI"),
      p()
    )
  )
}
