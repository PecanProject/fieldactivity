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
        
        if (dp()) message("Moving to rendering the .md file")
        
        # Path to the instructions .md which will be rendered
        callr::r(
          render_report,
          list(input = report_path, output = file, params = list())
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
#' @param id Internal parameters for {shiny}
#' @param user_auth Site name in order to download correct site files
#' @param base_folder Location of directories in server
#'
#' @noRd
#'
#' @importFrom utils write.csv
#'
mod_download_server_table <- function(id, user_auth, base_folder = json_file_base_folder()) {
  
  stopifnot(is.reactive(user_auth))
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$eventtable <- downloadHandler(
      # Name for the downloaded file
      filename = "event_table_fa.csv",
      
      content = function(file) {
        if(dp()) message("Creating an export of the events")
        
        site <- user_auth()
        blocks <- if (isTruthy(site)) {
          list.files(file.path(base_folder, site))
        }
        # events are read the same way as in the app, so legacy events are
        # exported in the canonical format too
        events <- unlist(lapply(blocks, function(block) {
          read_json_file(site, block, base_folder = base_folder)$events
        }), recursive = FALSE)
        
        if (length(events) == 0) {
          write.csv("Seems that there isn't any data? Try to create a management event.",
                    file, row.names = FALSE)
          return()
        }
        
        events_table <- events_to_table(events)
        # block first, as in the event list
        events_table <- events_table[c("block", setdiff(names(events_table), "block"))]
        write.csv(events_table, file, row.names = FALSE, na = "")
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
  
  stopifnot(is.reactive(user_auth))
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$eventjson <- downloadHandler(
      # Name for the downloaded file
      # With zip-files it has to be this way, otherwise it won't work
      filename = function() {
        paste("events_json", "zip", sep=".")
      },
      
      content = function(file) {
        if(dp()) message("Creating a zip file of the json files")
        
        # a fresh directory for each download, so no files are left over from
        # an earlier one
        zip_root <- tempfile()
        tmpdrjson <- file.path(zip_root, "json")
        dir.create(tmpdrjson, recursive = TRUE)
        on.exit(unlink(zip_root, recursive = TRUE), add = TRUE)
        
        site <- user_auth()
        blocks <- if (isTruthy(site)) {
          list.files(file.path(base_folder, site))
        }
        for (block_name in blocks) {
          file.copy(file.path(base_folder, site, block_name, "events.json"),
                    file.path(tmpdrjson, paste0("events_", block_name, ".json")))
        }
        
        if (length(list.files(tmpdrjson)) == 0) {
          if(dp()) message("Return a csv with an error")
          write.csv("Seems that there isn't any data? Try to create a management event.",
                    file.path(tmpdrjson, "Error.csv"), row.names = FALSE)
        }
        zip::zip(zipfile = file, files = "json", root = zip_root)
      },
      contentType = "application/zip"
    )
  }) #Moduleserver close
}
