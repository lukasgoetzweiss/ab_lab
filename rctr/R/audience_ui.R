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
      DTOutput('audience'),
      DTOutput('audienceSelected')
    ),
    tabPanel(
      "Create",
      h2("Audience Filters"),
      DTOutput('audienceFilter'),
      actionButton("createAudienceFilter",
                   "Add Filter",
                   icon = icon("fas fa-plus")),
      p(),
      uiOutput("audienceCreateUI"),
      p()
    )
  )
}
