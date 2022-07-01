#' Run the Shiny Application
#'
#' @param json_file_path Path to a folder used to store the generated .json files
#' @param user_db_path Path to a Shinymanager user database
#' @param user_db_passphrase The passphrase of the user database
#' @param ... arguments to pass to golem_opts. 
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
run_app <- function(
  json_file_path,
  user_db_path,
  user_db_passphrase,
  onStart = NULL,
  options = list(), 
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  
  ui <- app_ui
  if (golem::app_prod()) {
    # set the language of the login UI before it is displayed
    set_login_language(init_lang)
    # wrap the UI in shinymanager to display the login UI
    ui <- shinymanager::secure_app(
      ui,
      tags_bottom =
        tags$div(
          selectInput("login_language",
                      label = "" ,
                      choices = languages),
          p(readLines(system.file("user_doc", "inst_frontpage.txt", package = "fieldactivity"), warn = F)[1],
            style="text-align: justify;"),

          br(), br(),
          readLines(system.file("user_doc", "inst_frontpage.txt", package = "fieldactivity"), warn = F)[3],
          br(),
          readLines(system.file("user_doc", "inst_frontpage.txt", package = "fieldactivity"), warn = F)[4]),

      theme = bslib::bs_theme(version = 4),
      enable_admin = TRUE,
      fab_position = "top-right")
  }
  
  with_golem_options(
    app = shinyApp(
      ui = ui,
      server = app_server,
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(json_file_path = json_file_path,
                      user_db_path = user_db_path,
                      user_db_passphrase = user_db_passphrase, ...)
  )
}
