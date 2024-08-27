#' The application User-Interface
#'
#' This function creates the user interface for the Shiny application.
#' It defines the layout, input controls, and output elements that
#' users will interact with.
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources


app_ui <- function(request) {
  fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Field Activity Parser"),
    sidebarLayout(
      sidebarPanel(
        selectInput("language", "Select Language", choices = c("en", "fi", "sv")),
        # uiOutput("event_selector")
      ),
      mainPanel(
        uiOutput("dynamic_ui")
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "fieldactivityParser"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
