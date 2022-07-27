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
#' @importFrom zip zip

mod_download_ui <- function(id, label, purp) {
  ns <- NS(id)
  
  if(purp == "inst"){
    #tagList(
      downloadButton(ns("report"), label, class = "butt", icon = icon("download"), style = "width:85px;")
      # #tags$head(tags$style(".butt{width:85px;} .butt{display: flex;}
      #                     .butt{margin-top: 1.45em;}"))))
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



#' UI for exporting the eventtable as csv // json zip file
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

mod_download_json <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    downloadButton(ns("eventjson"), label, class = "butt", icon = icon("download")),
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
        #user <- NULL
        if (golem::app_dev()) {
          if(dp()) message("Development state")
          file_path <- "dev/dev_events"
          user <- "qvidja"
          
        } else {
          if(dp()) message("Data path on production")
          file_path <- base_folder
          
          if(dp()) message("Checking current user")
            user <- user_auth
        }
        # Create the file path based on the production status and the user
        file_path <- file.path(file_path, user)
        events_file <- NULL
        if(length(list.files(file_path)) != 0){
          for(i in list.files(file_path)){
            jsontable <- jsonlite::read_json(file.path(file_path, i, "events.json"), simplifyVector = TRUE)[[1]]$events
            if(is.null(events_file)){
              if(!identical(list(), jsontable)){
                events_file <- as.data.frame(cbind(block = i, jsontable))
              }
            } else {
              events_file <- merge(events_file,as.data.frame(cbind(block = i,jsontable)),all = T)
            }
          }
          
          # Flattening the lists and removing extra "," that can cause parsing issues
          events_file <- as.data.frame(lapply(events_file, as.character))
          # Might not be suitable, if there are only simple event management stored,
          # so only try this modification.
          events_file <- try(as.data.frame(lapply(events_file, function(x) gsub(",", " ", x))))
          
          if (dp()) message("Creating an export of the events")
          write.csv(events_file, file, row.names = FALSE, quote=FALSE)
        } else {
          write.csv("Error with a file path. Have you stored field management events? If yes, then this error should not occur.", file, row.names = FALSE, quote = FALSE)
        }
        
      }
    )
  }) #Moduleserver close
}


#' Server side for download button for json-files
#'
#' @param id Internal parameters for {shiny}
#' @param user_auth Site name in order to download correct site files
#' @param base_folder Location of directories in server
#'
#' @noRd
#'

mod_download_server_json <- function(id, user_auth, base_folder = json_file_base_folder()) {
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$eventjson <- downloadHandler(
      # Name for the downloaded file
      # With zip-files it has to be this way, otherwise it won't work
      filename = function() {
        paste("events_json", "zip", sep=".")
      },
      
      content = function(file) {
        
        if(dp()) message("Fetching the event table observations (for json export)")
        
        if (golem::app_dev()) {
          if(dp()) message("Development state")
          file_path <- "dev/dev_events"
          user <- "qvidja"
          
        } else {
          if(dp()) message("Data path on production")
          file_path <- base_folder
          
          if(dp()) message("Checking current user")
          user <- user_auth
        }
        # Create the file path based on the production status and the user
        file_path <- file.path(file_path, user)
        # tmp directory with sub directory for json files
        tmpdr <- tempdir()
        if(!file.exists(file.path(tmpdr, "json"))){
          dir.create(paste0(tmpdr, "/json"))
        } 
        
        # Path to subdirectory
        tmpdrjson <- file.path(tmpdr, "json")
        
        if(length(list.files(file_path)) != 0){
          for(block_name in list.files(file_path)){
            block_json <- file.path(tmpdrjson, paste0("events_", block_name, ".json"))
            file.copy(file.path(file_path, block_name, "events.json"), block_json)
          }
          
          if (dp()) message("Creating a zip file of the json files")
          zip::zip(zipfile=file, files="json", root = tmpdr)
        } else {
          #emptydir <- file.path(tmpdr, "Invalid_path.csv")
          if(dp()) message("Return a csv with an error")
          write.csv("Invalid file path", file.path(tmpdrjson, "Error.csv"), row.names = FALSE)
          zip::zip(zipfile=file, files="json", root = tmpdr)
        }
      },
      contentType = "application/zip"
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
