#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom glue glue
#' @noRd
app_server <- function(input, output, session) {
  # run interactive themer
  #bslib::bs_themer()
  
  if (dp()) message("Initializing server function")
  
  # check_credentials returns a function to authenticate users
  credential_checker <- shinymanager::check_credentials(
    db = golem::get_golem_options("user_db_path"),
    passphrase = golem::get_golem_options("user_db_passphrase")
  )
  
  # call the server part of shinymanager
  # weird observation: this has to be after the observeEvent block
  # which observes the auth_result$user. If it isn't the site selectInput
  # selection is not updated to match the username.
  # UPDATE: moved it back and doesn't do anything weird anymore
  auth_result <- shinymanager::secure_server(check_credentials = 
                                               credential_checker)
  
  # change login form language when requested
  observeEvent(input$login_language, {
    #str(reactiveValuesToList(input))
    #updateTextInput(session, "auth-user_id", value = "oma1")
    #updateTextInput(session, "auth-user_pwd", value = "oma2")
    #shinyjs::click("auth-go_auth") # doesn't work
    
    if (dp()) message("input$login_language")
    
    # this function is defined in fct_language.R
    set_login_language(input$login_language)
    
    # TODO: make the language setting communicate to main app
    # PROBLEM: shinymanager isolates the app pretty well, so for instance
    # communication through session$userData doesn't work
    
    # this seems to refresh the authentication UI
    auth_result <- shinymanager::secure_server(check_credentials = 
                                                 credential_checker)
    
    
    # Update the authentication page language
    mod_auth_page_server("auth_text", input$login_language)
  })
  
  # runs when logged in
  observeEvent(auth_result$user, {
    
    if (dp()) message("auth_result$user changed")
    
    if (auth_result$admin == "FALSE") {
      
      updateSelectInput(session, "site", selected = auth_result$user)
      shinyjs::disable("site")
      
      updateTextInput(session, "uservisible", value = auth_result$user)
      shinyjs::disable("uservisible")
      
      
      # shinyjs::show("usevisible")
    } else {
      shinyjs::enable("site")
      shinyjs::show("site")
      shinyjs::disable("uservisible")
    }

  })

  if (golem::app_dev()) {
    shinyjs::show("site")
  }
  
  ################
  
  # Module for download server, need to decide if ui is separated to
  # different functions, if more download buttons in required
  
  mod_download_server_inst("download_ui_1")
  
  mod_download_server_table("event_table", auth_result$user)
  
  mod_download_server_json("json_zip", auth_result$user)
  
  
  
  ################
  
  # lists of events by block on the currently viewed site
  # accessed like events$by_block[["0"]]
  # has to be done this way, because you can't remove values from reactiveValues
  events <- reactiveValues(by_block = list())

  # start server for the event list
  event_list <- mod_event_list_server("event_list",
                                      events = reactive(events$by_block),
                                      language = reactive(input$language),
                                      site = reactive(input$site))
  # a reactiveVal which holds the currently edited event
  event_to_edit <- event_list$current
  
  form <- list(
    # change this to change the values of widgets in the form
    set_values = reactiveVal(),
    # set to TRUE to reset the values of widgets in the form
    reset_values = reactiveVal(),
    init_signal = reactiveVal(),
    values = list(initialised = FALSE)
  )
  # start server for the form
  # form$values <- mod_form_server("form", 
  #                                site = reactive(input$site),
  #                                set_values = form$set_values,
  #                                reset_values = form$reset_values,
  #                                edit_mode = reactive(
  #                                  !is.null(event_to_edit())),
  #                                language = reactive(input$language))
  
  # update each of the text outputs automatically, including language changes
  # and the dynamic updating in editing table title etc. 
  lapply(text_output_code_names, FUN = function(text_output_code_name) {
    # render text
    output[[text_output_code_name]] <- renderText({
      if (dp()) message(glue("Rendering text for {text_output_code_name}"))
      
      get_disp_name(text_output_code_name, input$language)
    })
  })
  
  observeEvent(event_to_edit(), ignoreNULL = FALSE, ignoreInit = TRUE, {
    
    if (dp()) message("event_to_edit() changed")
    
    if (is.null(event_to_edit())) {
      # edit mode was disabled
      # hide the form and clear values
      exit_form()
      return()
    } 
    
    ### edit mode was enabled, or there was a switch from one event to  another
    
    # fill values on the form and show it
    form$set_values(event_to_edit())
    show_form(edit_mode = TRUE)
    
  })
  
  
  
  # exit sidebar mode
  # this is called when saving and when pressing cancel
  exit_form <- function() {
    if (dp()) message("Hiding form")
    
    # reset all input fields
    form$reset_values(TRUE)
    
    # hide sidebar
    shinyjs::hide("form_panel", anim = TRUE, animType = "slide")
    shinyjs::enable("add_event")
    shinyjs::disable("clone_event")
    if (golem::app_dev() || auth_result$admin == "TRUE") {
      shinyjs::enable("site")
    }
  }
  
  show_form <- function(edit_mode = FALSE) {
    if (dp()) message("Showing form")
    
    shinyjs::show("form_panel", anim = TRUE, animType = "slide")
    shinyjs::disable("add_event")
    if (edit_mode) shinyjs::enable("clone_event")
    if (golem::app_dev() || auth_result$admin == "TRUE") {
      shinyjs::disable("site")
    }
    
    # send the init signal. If this has not been done before, this will cause
    # the form to initialise table and fileInput server functions
    form$init_signal(TRUE)
  }
  
  # load data from all the json files corresponding to a site and store it in
  # separate lists in events$by_block
  load_json_data <- function(site1) {
    # clear possible previous data
    events$by_block <- list()
    
    # find all blocks on this site
    site_blocks <- subset(sites, sites$site == site1)$blocks[[1]]
    
    # go through the blocks and save events from the corresponding json file
    # to events$by_block
    for (block in site_blocks) {
      events$by_block[[block]] <- read_json_file(site1, block)
    }
  }
  
  # a function which calls the server function of the form module and adds
  # observers to its buttons. This is a separate function because we want to
  # delay calling it to improve start up speed.
  initialise_form <- function() {
    
    form$values <<- mod_form_server("form", 
                                    site = reactive(input$site),
                                    set_values = form$set_values,
                                    reset_values = form$reset_values,
                                    edit_mode = reactive(
                                      !is.null(event_to_edit())),
                                    language = reactive(input$language),
                                    init_signal = form$init_signal)
    
    # cancel means we exit edit mode and hide the form
    observeEvent(form$values$cancel(), {
      if (is.null(event_to_edit())) {
        exit_form()
      } else {
        event_to_edit(NULL)
      }
    })
    
    # save input to a file when save button is pressed
    # we are either creating a new event or editing an older one
    observeEvent(form$values$save(), {
      # fetch new values from the form
      event <- form$values$data()
      
      if (is.null(event)) {
        if (dp()) message("All validation rules have not been met")
        showNotification(paste("Some of the entered information is not valid.",
                               "Please check the fields highlighted in red."),
                         type = "warning")
        return()
      }
      
      # are we editing an existing event or creating a new one?
      orig_event <- event_to_edit()
      editing <- !is.null(orig_event)
      
      # if we are editing, find the index of the event in the original 
      # block data list. Also, if the block has been changed, update that 
      # file. If the block has not changed, we will need the index when
      # replacing the old event with the updated one.
      if (editing) {
        
        orig_block_data <- read_json_file(input$site, orig_event$block)
        event_index <- find_event_index(orig_event, orig_block_data)
        
        if (is.null(event_index)) {
          showNotification("Could not edit entry because it was not 
                                 found in the event files.", type = "error")
          return()
        }
        
        # if the block of the event has been changed, delete it from the 
        # original block file.
        # If the event has files associated with it (like images), those will be
        # handled later.
        if (event$block != orig_event$block) {
          orig_block_data[event_index] <- NULL
          write_json_file(input$site, orig_event$block, orig_block_data)
          events$by_block[[orig_event$block]] <- orig_block_data
        }
        
      }
      
      # Now we need to make sure associated files such as images are handled 
      # properly.
      #
      # scenarios:
      # 1. original had a file but the variable is no longer relevant
      # 2. original had a file and the variable is relevant:
      #   i.   no new file and block and date have not changed
      #   ii.  no new file but block or date has changed
      #   iii. new file
      # 3. original did not have a file:
      #   i.  new file
      #   ii. no new file
      
      
      
      for (fileInput_code_name in fileInput_code_names) {
        
        orig_path <- orig_event[[fileInput_code_name]]
        orig_path_exists <- !is.null(orig_path) & 
          !identical(orig_path, missingval)
        new_value <- event[[fileInput_code_name]]
        
        # if a fileInput variable is no longer relevant to an event, delete the
        # associated file (SCENARIO 1)
        if (fileInput_code_name %in% names(orig_event) &&
            !(fileInput_code_name %in% names(event)) &&
            orig_path_exists) {
          
          # delete the file
          tryCatch(expr = delete_file(orig_path, input$site, orig_event$block,
                                      filepath_relative = TRUE),
                   error = function(cnd) {
                     message(glue("Could not delete file related ",
                                  "to the edited event: {cnd}"))
                   })
          
          next
        }
        
        # if the event doesn't have a file with this fileInput_code_name, skip
        # to the next one
        if (is.null(new_value)) next
        
        # should currently saved file be deleted? 
        clear_value <- is.null(new_value$filepath) & orig_path_exists
        # is a new file uploaded?
        new_file_uploaded <- new_value$new_file
        
        if (new_file_uploaded) {
          filepath <- new_value$filepath
          # move the file into place and get the relative path
          # (SCENARIO 3i/2iii)
          relative_path <- 
            tryCatch(expr = 
                       copy_file(filepath, 
                                 variable_name = fileInput_code_name,
                                 site = input$site, 
                                 block = event$block, 
                                 date = event$date),
                     error = function(cnd) {
                       showNotification(
                         "Could not save the image file correctly.", 
                         type = "warning")
                       message(glue("Error when saving image to ",
                                    "{fileInput_code_name}: {cnd}"))
                       missingval
                     })
          
          # if the event already has a value in this field, we are replacing the
          # file with a new one. We should therefore delete the old file.
          # (SCENARIO 2iii)
          if (orig_path_exists) {
            # delete the file
            tryCatch(expr = delete_file(orig_path, input$site, 
                                        orig_event$block, 
                                        filepath_relative = TRUE),
                     error = function(cnd) {
                       message(glue("Could not delete file to ",
                                    "be replaced: {cnd}"))
                     })
          }
          
          event[[fileInput_code_name]] <- relative_path
        } else if (clear_value) {
          # the file was deleted by the user using the delete button.
          # Let's actually delete the file and save changes
          tryCatch(expr = delete_file(orig_path, input$site, 
                                      orig_event$block, filepath_relative = TRUE),
                   error = function(cnd) {
                     message(glue("Could not delete file related ",
                                  "to the event: {cnd}"))
                   })
          
          event[[fileInput_code_name]] <- missingval
        } else {
          
          # a new file was not uploaded. 
          # However, if there is already a file uploaded, we might 
          # have to rename/move it as the event date or block might
          # have changed. We will therefore move the current file
          # like it was a new file.
          
          if (editing && orig_path_exists) {
            
            if (event$block != orig_event$block | event$date != orig_event$date) {
              # we should rename and/or move the file
              
              # this path is relative to json_file_base_folder()
              orig_relative_path <- file.path(input$site, orig_event$block, 
                                              orig_path)
              
              relative_path <- 
                tryCatch(expr = copy_file(
                  orig_relative_path, 
                  variable_name = fileInput_code_name,
                  site = input$site, 
                  block = event$block, 
                  date = event$date,
                  filepath_is_relative = TRUE,
                  delete_original = TRUE),
                  error = function(cnd) {
                    showNotification(
                      "Could not rename the image file.", 
                      type = "warning")
                    message(glue("Error when renaming ",
                                 "image ",
                                 "{variable_name}: {cnd}"))
                    orig_path
                  })
              
              event[[fileInput_code_name]] <- relative_path
              
            } else {
              # we did not have to move the file and therefore do
              # not need to change the current file path
              event[[fileInput_code_name]] <- event[[fileInput_code_name]]$filepath
              next
            }
            
          } else {
            # we are not editing (or path is missingval), so save 
            # missingval
            event[[fileInput_code_name]] <- missingval
          }
          
        }
        
      }
      
      if (dp()) {
        message("The finished event:")
        utils::str(event)
      }
      
      # load the json file corresponding to the new block selection (new as in
      # the current event$block value). We load from the file because it might
      # have changed and events$by_block might be out of date
      new_block_data <- read_json_file(input$site, event$block)
      
      # if editing and block didn't change, replace event. 
      # Otherwise append event to the list
      if (editing && orig_event$block == event$block) {
        new_block_data[[event_index]] <- event
      } else {
        new_block_data[[length(new_block_data) + 1]] <- event
      }
      
      # save changes
      write_json_file(input$site, event$block, new_block_data)
      showNotification("Saved successfully.", type = "message")
      
      # update events$by_block
      events$by_block[[event$block]] <- new_block_data
      
      # exit sidebar mode
      if (editing) {
        event_to_edit(NULL)
      } else {
        exit_form()
      }
      
    })
    
    # delete entry when delete button is pressed
    observeEvent(form$values$delete(), {
      event <- event_to_edit()
      
      # retrieve up to date information from the json file
      block_data <- read_json_file(input$site, event$block)
      
      # find the index of the event to be deleted from the event list
      event_index <- find_event_index(event, block_data)
      
      if (is.null(event_index)) {
        showNotification("Could not delete entry because it 
                             was not found in the event files.", type = "error")
        return()
      }
      
      # if the event has image files associated with it (e.g. canopeo_image)
      # delete those. For this we go through all fileInput variables and check
      # whether they are defined in the event
      for (fileInput_code_name in fileInput_code_names) {
        value <- event[[fileInput_code_name]]
        path_exists <- !is.null(value) & !identical(value, missingval)
        
        if (fileInput_code_name %in% names(event) & path_exists) {
          # delete the file
          tryCatch(expr = delete_file(value, input$site, event$block, 
                                      filepath_relative = TRUE),
                   error = function(cnd) {
                     message(glue("Could not delete file related to ",
                                  "the deleted event: {cnd}"))
                     
                   })
        }
      }
      
      # delete
      block_data[event_index] <- NULL
      
      # write changes to json
      write_json_file(input$site, event$block, block_data)
      showNotification("Entry deleted.", type = "message")
      
      # update events list
      events$by_block[[event$block]] <- block_data
      
      # exit edit mode
      event_to_edit(NULL)
    })
    
  }
  
  # show add event UI when requested
  observeEvent(input$add_event, {
    # note: event_to_edit() should be NULL at this point as the add button is
    # disabled during editing
    if (!is.null(event_to_edit())) return()
      
    # if we were not editing previously, copy current table view settings
    # (block and activity) into the widgets if they are set to a specific
    # value (not "all")
    new_values <- list()
    activity <- event_list$filters()$activity
    block <- event_list$filters()$block
    
    if (!is.null(activity) && activity != "activity_choice_all") {
      new_values$mgmt_operations_event <- activity
    }
    if (!is.null(block) && block != "block_choice_all") {
      new_values$block <- block
    }
    
    if (!identical(new_values, list())) form$set_values(new_values)
    
    show_form()
  })
  
  observeEvent(input$clone_event, {
    # fetch the event to be cloned
    event <- event_to_edit()
    
    # if the event has files associated with it, those need to be duplicated
    for (fileInput_code_name in fileInput_code_names) {
      
      path <- event[[fileInput_code_name]]
      path_exists <- !is.null(path) & !identical(path, missingval)
      
      if (path_exists) {
        # path is relative to events.json. This path is relative to 
        # json_file_base_folder() :
        relative_path <- file.path(input$site, event$block, path)
        
        clone_file_path <- copy_file(orig_filepath = relative_path,
                                     variable_name = fileInput_code_name,
                                     site = input$site,
                                     block = event$block,
                                     date = event$date,
                                     filepath_is_relative = TRUE)
        event[[fileInput_code_name]] <- clone_file_path
      }
      
    }
    
    block_data <- read_json_file(input$site, event$block)
    
    block_data[[length(block_data) + 1]] <- event
    
    # save changes
    write_json_file(input$site, event$block, block_data)
    showNotification("Cloned successfully.", type = "message")
    
    # update events data
    events$by_block[[event$block]] <- block_data
  })
  
  # load the site event data into memory (events$by_block)
  observeEvent(input$site, ignoreNULL = FALSE, {
    
    if (dp()) message("input$site was changed")
    
    if (!isTruthy(input$site)) {
      shinyjs::disable("add_event")
      return()
    } 
    
    shinyjs::enable("add_event")

    if (dp()) message(glue::glue("Loading data for site {input$site}"))
    # load the events corresponding to this site into memory
    load_json_data(input$site)
    
    # initialise the form module if it has not been done yet
    if (identical(form$values$initialised, FALSE)) {
      initialise_form()
    }
    
  })
  
  # change language when user requests it
  observeEvent(input$language, ignoreInit = TRUE, {
    
    if (dp()) message("input$language changed")
    
    # get a list of all input elements which we have to relabel
    input_element_names <- names(reactiveValuesToList(input))
    
    for (code_name in input_element_names) {
      
      # TODO: update to use the update_ui_element function
      
      # find element in the UI structure lookup list
      element <- structure_lookup_list[[code_name]]  
      
      # didn't find the element corresponding to code_name
      # this should not happen if the element is in 
      # sidebar_ui_structure.json
      if (is.null(element$type)) next
      
      label <- get_disp_name(element$label, input$language)
      
      if (element$type == "selectInput") {
        
        # fetch choices for the selectInput
        choices <- get_selectInput_choices(code_name, input$language)
        
        # make sure we don't change the selected value
        current_value <- input[[code_name]]
        
        if (is.null(choices)) {
          updateSelectInput(session, 
                            code_name,
                            label = ifelse(is.null(label),"",label),
                            selected = current_value) 
        } else {
          updateSelectInput(session, 
                            code_name,
                            label = ifelse(is.null(label),"",label),
                            choices = choices,
                            selected = current_value)
        }
        
      } else if (element$type == "actionButton") {
        updateActionButton(session,
                           code_name,
                           label = label)
      }
      
    }

  })

}
