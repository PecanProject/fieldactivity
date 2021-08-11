# This module wraps the standard Shiny fileInput widget and makes it actually
# usable. The responsibilities of this module are as follows:
# - store the relative path of currently stored file
# - recognize when a new file has been uploaded, check the file and supply its
#   temp path
# - update the displayed texts on the widget
# - manage the file delete button
#
# the module returns a two-element list:
# - filepath: the path of the file the module currently “holds”, either a
#   relative path to a previously saved file that it was given, or an absolute
#   path to a file in a temp folder. If this is NULL, this is an indication 
#   that no file is uploaded or if there was, that file should be deleted.
#   Note that this is not necessarily the path that should be saved in the main
#   app: if a file was uploaded previously but block has been changed, then the 
#   file still needs to be moved and renamed. This will be handled in the main 
#   app.
# - new_file: TRUE if the filepath points to a new file in a temp folder (and
#   the filepath is therefore absolute)


# when an event's information is given to the module, store the filepath
# somewhere. When saving the values, if a new file is uploaded, the temp
# path should be returned to the main app. It then moves the file and
# changes the path. If no new file is uploaded, return the old path (if
# the path should be updated, the main app will update the path and move the
# file). If the file is to be deleted, return an empty path and the main app 
# will delete the file


#' fileInput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fileInput_ui <- function(id, widget_structure) {
  ns <- NS(id)
  tagList(
    div(style = "display: flex;",
        
        div(style = "flex-grow: 1;", 
            fileInput(ns("file"), 
                      label = get_disp_name(widget_structure$label, init_lang),
                      accept = widget_structure$filetype)),
        
        div(style = "margin-left: 5px; padding-top: 26px",
            shinyjs::hidden(
              actionButton(ns("delete"),
                           label = get_disp_name("delete_uploaded_file_label", 
                                                 init_lang), 
                           class = "btn-warning")))
    )
  )
}
    
#' fileInput Server Functions
#'
#' @noRd 
mod_fileInput_server <- function(id, language, set_path, reset_path) {
  
  stopifnot(is.reactive(language))
  stopifnot(is.reactive(set_path))
  stopifnot(is.reactive(reset_path))
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # hold the latest path
    current_path <- reactiveVal()
    # is the value in current_path pointing to a new file in a temp directory?
    new_file <- reactiveVal(FALSE)
    
    # set new_text to NULL to clear the value
    update_value_text <- function(new_text) {
      if (is.null(new_text)) {
        # this clears the text on the widget, but not its value
        shinyjs::reset("file")
      } else {
        session$sendCustomMessage(type = "fileInput-value",
                                  message = list(id = ns("file"), 
                                                 value = new_text))
      }
    }
    
    update_label <- function(new_label) {
      session$sendCustomMessage(type = "fileInput-label",
                                message = list(id = ns("file"),
                                               value = new_label))
    }
    
    # Check the file immediately after it is uploaded, and if its extension
    # is not correct, delete the file.
    observeEvent(input$file, {
      
      # path to the uploaded file in temporary folder
      tmp_path <- input$file$datapath
      file_extension <- tools::file_ext(tmp_path)
      allowed_extensions <- c("jpg", "jpeg", "tif", "tiff", "png")
      # if the image format is not desired, delete file and clear field
      if (!(tolower(file_extension) %in% allowed_extensions)) {
        delete_file(tmp_path)
        
        showNotification(
          glue("This file extension is not supported. ",
               "Upload a file with one of the ",
               "following extensions: ",
               paste(allowed_extensions, collapse = ", ")), 
          type = "error", duration = NULL)
        
        return()
      }
      
      # the file is valid, so save its path
      current_path(tmp_path)
      new_file(TRUE)
      
    })
    
    # when language changes, update labels
    observeEvent(language(), {
      label <- structure_lookup_list[[id]]$label
      update_label(get_disp_name(label, language()))
      # update delete button label
      updateActionButton(session, "delete", 
                         label = get_disp_name("delete_uploaded_file_label",
                                               language()))
    })
    
    # when a new relative path is supplied, store it
    observeEvent(set_path(), {
      
      path <- set_path()
      if (identical(path, missingval) | identical(path, "")) {
        path <- NULL
      }
      
      current_path(path)
      new_file(FALSE)
      update_value_text(path)
      # reset set_path so that a new value can be supplied again through it
      set_path(NULL)
    })
    
    # when reset_path is signaled, do it
    observeEvent(reset_path(), {
      current_path(NULL)
      reset_path(FALSE)
    })
    
    # control the visibility of the delete button
    observeEvent(current_path(), ignoreNULL = FALSE, {
      shinyjs::toggle("delete", condition = !is.null(current_path()))
      if (is.null(current_path())) {
        update_value_text(NULL)
      }
    })
    
    # delete uploaded file when requested
    observeEvent(input$delete, {
      
      # this should not happen as the delete button should be hidden
      if (is.null(current_path())) {
        shinyjs::hide("delete")
        return()
      }
      
      current_path(NULL)
      new_file(FALSE)
      
      # TODO: if newly uploaded file, delete temp file right away?
      
      # there are two types of deletions: 
      # 1. the user has uploaded a new file and wants to delete it 
      # (non-saved file)
      # - there can be a previously saved file if we are editing an event,
      # that should then be displayed
      # 2. the user has not uploaded a new file, but the event has a 
      # previously saved file the user wants to delete.
      
      # the value of a fileInput cannot be reset, so we need to
      # compare the current value to the old one to figure out if
      # a new value has been entered
      # new_file_uploaded <- 
      #   !identical(input[[fileInput_code_name]],
      #              session$userData$
      #                previous_fileInput_value[[fileInput_code_name]])
      # event <- event_to_edit()
      # editing <- !is.null(event)
      # 
      # if (new_file_uploaded) {
      #   
      #   # # if we are editing, there might be a previous file
      #   # if (editing) {
      #   #     
      #   #     old_path <- event[[fileInput_code_name]]
      #   #     
      #   #     if (!is.null(old_path) & !identical(old_path, missingval)) {
      #   #         message(glue("Deleting new file and going back to ",
      #   #                      "{old_path}"))
      #   #     } else {
      #   #         message("Deleting new file, no previous files")
      #   #     }
      #   #     
      #   # } else {
      #   #     message("Deleting a newly uploaded file in add mode")
      #   # }
      #   
      #   # clear fileInput
      #   update_ui_element(session, fileInput_code_name, 
      #                     clear_value = TRUE)
      #   # TODO: delete the actual file? 
      #   
      # } else {
      #   
      #   # new file was not uploaded. Check if we are editing and there
      #   # is a previous file we should delete
      #   old_path <- event[[fileInput_code_name]]
      #   if (editing && !is.null(old_path) && 
      #       !identical(old_path, missingval)) {
      #     
      #     # there is an old file we should delete when the changes
      #     # to the event are saved. To signal this, let's add a flag
      #     # to session$userData$previous_fileInput_value
      #     update_ui_element(session, fileInput_code_name, 
      #                       clear_value = TRUE)
      #     session$userData$previous_fileInput_value[[
      #       fileInput_code_name]] <- list(clear_value = TRUE)
      #     
      #   } else {
      #     # the button should not be visible at this point
      #     message("No new file uploaded and no old file to delete")
      #   }
      #   
      # }
      
    })
    
    ################
    
    return(reactive(list(filepath = current_path(),
                         new_file = new_file())))
    
  })
}
