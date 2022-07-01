#' Download UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#' @param label Label for the download button
#' @param purp This decides which download happens, this
#' could divided to several download ui functions as well.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_download_ui <- function(id, label, purp) {
  ns <- NS(id)
  
  if(purp == "inst"){
    tagList(
      column(width = 1,
      downloadButton(ns("report"), label, class = "butt", icon = icon("download"),
      tags$head(tags$style(".butt{width:85px;} .butt{display: flex;}
                          .butt{margin-top: 1.45em;}")))))
  } else {
    # Not decided
  }
}


#' Download Server Functions
#'
#' @noRd

mod_download_server_inst <- function(input, output, session) {
  ns <- session$ns

  output$report <- downloadHandler(
    # Name for the downloaded file
    filename = "guideFieldactivity.html",
    content = function(file) {
      params <- list(n = input$n)

      id <- showNotification(
        "Rendering report...",
        duration = NULL,
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)
      
      
      # Path to the instructions .md which will be rendered
      rmarkdown::render("./inst/user_doc/user_instructions.md",
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}







#' download Server Function
#'
#' @noRd
# mod_download_serverxx <- function(input, output, session){
#   ns <- session$ns
#   data_xi <- "string"
#   
#   output$report <- downloadHandler(
#     
#     filename = function(){
#       paste("sitemxt", "csv", sep = ".")
#     },
#     
#     content = function(file){
#       write.csv(data_xi, file)
#     }
#   )
# }