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
#' @importFrom shiny NS tagList 
#' @importFrom callr r

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
mod_download_server_inst <- function(id) {
  
  
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
        
        # Copy the actual files to tmp folder. Images need to be on the same folder as instructions .md
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



#' UI for exporting the eventtable as csv
#'
#' @param id Internal parameters for {shiny}
#' @param label Label for download button
#'
#' @noRd
#' 

mod_download_table <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    downloadButton(ns("eventtable"), label, class = "butt", icon = icon("download")),
                   tags$head(tags$style(".butt{width:150px;} .butt{display: flex;}")))
}




#' Server side for downloading the csv export
#'
#' @noRd
#'
#' @importFrom utils write.csv
#'
mod_download_server_table <- function(id, user_auth, base_folder = json_file_base_folder()) {
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$eventtable <- downloadHandler(
      # Name for the downloaded file
      filename = "event_table_fa.csv",
      
      content = function(file) {
        
        if(dp()) message("Fetching the event table observations")
        user <- NULL
        # if (golem::app_dev()) {
        #   file_path <- "dev/dev_events"
        #   #user <- "qvidja"
        # } else {
        if(dp()) message("Data path changed to production")
        file_path <- base_folder
        #print(file_path)
        
        if(dp()) message("Checking current user")
          user <- user_auth
          #print(user)
        # }
        # Create the file path based on the production status and the user
        file_path <- file.path(file_path, user)
        events_file <- NULL
        if(length(list.files(file_path)) != 0){
          for(i in list.files(file_path)){
            if(is.null(events_file)){
              events_file <- cbind(block = i, jsonlite::read_json(file.path(file_path, i, "events.json"), simplifyVector = TRUE)[[1]]$events)
            } else {
              events_file <- merge(events_file, cbind(block = i, jsonlite::read_json(file.path(file_path, i, "events.json"), simplifyVector = TRUE)[[1]]$events), all = T)
            }
          }
          
          # Flattening the lists and removing extra "," that can cause parsing issues
          events_file <- apply(events_file, 2, as.character)
          events_file <- apply(events_file, 2, function(x) gsub(",", " ", x))
          
          if (dp()) message("Creating an export of the events")
          write.csv(events_file, file, row.names = FALSE, quote=FALSE)
        } else {
          write.csv("Error with a file path. Under maintenance", file, row.names = FALSE, quote = FALSE)
        }
        
      }
    )
  }) #Moduleserver close
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
