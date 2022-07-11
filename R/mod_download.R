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
#' @import rmarkdown
#' @import callr
#' @importFrom shiny NS tagList 

mod_download_ui <- function(id, label, purp) {
  ns <- NS(id)
  
  if(purp == "inst"){
    tagList(
      downloadButton(ns("report"), label, class = "butt", icon = icon("download"),
      tags$head(tags$style(".butt{width:85px;} .butt{display: flex;}
                          .butt{margin-top: 1.45em;}"))))
  } else {
    # Not decided
  }
}


#' Download Server Functions
#'
#' @noRd
mod_download_server_inst <- function(id, report_path) {
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$report <- downloadHandler(
      # Name for the downloaded file
      filename = "guideFieldactivity.html",
      content = function(file) {
        params <- list(n = input$n)
        
        if(dp()) message("Copying instructions to temp file")
        
        # Paths to the rendered document + used images
        report_path <- file.path(tempdir(), "user_instructions.md")
        report_img_1 <- file.path(tempdir(), "loginpage.png")
        report_img_2 <- file.path(tempdir(), "Layout.png")
        report_img_3 <- file.path(tempdir(), "Eventtable.png")
        report_img_4 <- file.path(tempdir(), "Addevent.png")
        report_img_5 <- file.path(tempdir(), "eventexample_1.png")
        
        # Copu the actual files to tmp folder. Images need to be on the same folder as instructions .md
        file.copy(system.file("user_doc", "user_instructions.md", package = "fieldactivity"), report_path, overwrite = TRUE)
        file.copy(system.file("user_doc/images_user_instructions", "loginpage.png", package = "fieldactivity"), report_img_1, overwrite = TRUE)
        file.copy(system.file("user_doc/images_user_instructions", "Layout.png", package = "fieldactivity"), report_img_2, overwrite = TRUE)
        file.copy(system.file("user_doc/images_user_instructions", "Eventtable.png", package = "fieldactivity"), report_img_3, overwrite = TRUE)
        file.copy(system.file("user_doc/images_user_instructions", "Addevent.png", package = "fieldactivity"), report_img_4, overwrite = TRUE)
        file.copy(system.file("user_doc/images_user_instructions", "eventexample_1.png", package = "fieldactivity"), report_img_5, overwrite = TRUE)
        
        # id <- showNotification(
        #   "Rendering report...",
        #   duration = 8,
        #   closeButton = FALSE
        # )
        # on.exit(removeNotification(id), add = TRUE)
        
        if (dp()) message("Moving to rendering the .md file")
        
        # Path to the instructions .md which will be rendered
        callr::r(
          render_report,
          list(input = report_path, output = file, params = params)
        )
      }
    )
  }) #Moduleserver close
}



render_report <- function(input, output, params) {
  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())

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
