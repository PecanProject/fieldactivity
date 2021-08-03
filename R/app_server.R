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
  
  # change login form language when requested
  observeEvent(input$login_language, {
    #str(reactiveValuesToList(input))
    #updateTextInput(session, "auth-user_id", value = "oma1")
    #updateTextInput(session, "auth-user_pwd", value = "oma2")
    #shinyjs::click("auth-go_auth") # doesn't work
    
    # yes we are overwriting the English language. This is by far
    # the simplest method
    
    if (dp()) message("input$login_language")
    
    if (input$login_language == "disp_name_fin") {
      shinymanager::set_labels(
        language = "en",
        # the \U codes are UTF-8 codes for Finnish letters a and o with dots
        "Please authenticate" = "Kirjaudu sy\U00f6tt\U00e4\U00e4ksesi tapahtumia",
        "Username:" = "Sijainti",
        "Password:" = "Salasana",
        "Login" = "Kirjaudu",
        "Logout" = "Kirjaudu ulos"
      )
    } else if (input$login_language == "disp_name_eng") {
      shinymanager::set_labels(
        language = "en",
        "Please authenticate" = "Log in to enter management events",
        "Username:" = "Site",
        "Password:" = "Password",
        "Login" = "Login",
        "Logout" = "Logout"
      )
    }
    
    # TODO: make the language setting communicate to main app
    # PROBLEM: session userData doesn't seem to be saved after
    # logging in is complete
    
    # change the value of the session-specific variable default_language to
    # match the login language selector
    # session$userData$default_language <- input$login_language
    
    # this seems to refresh the authentication UI
    auth_result <- shinymanager::secure_server(check_credentials = 
                                                 credential_checker)
  })
  
  # runs when logged in
  observeEvent(auth_result$user, {
    
    if (dp()) message("auth_result$user")
    
    if (auth_result$admin == "FALSE") {
      updateSelectInput(session, "site", selected = auth_result$user)
      shinyjs::disable("site")
    } else {
      shinyjs::enable("site")
      shinyjs::show("site")
    }
    
    # here would be good to somehow fetch the language selection from the
    # login UI, but it's difficult
  })
  
  # call the server part of shinymanager
  # weird observation: this has to be after the observeEvent block
  # which observes the auth_result$user. If it isn't the site selectInput
  # selection is not updated to match the username.
  auth_result <- shinymanager::secure_server(check_credentials = 
                                               credential_checker)
  
  if (golem::app_dev()) {
    shinyjs::show("site")
  }
  
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
    reset_values = reactiveVal()
  )
  # start server for the form
  form$values <- mod_form_server("form", 
                                 site = reactive(input$site),
                                 set_values = form$set_values,
                                 reset_values = form$reset_values,
                                 edit_mode = reactive(
                                   !is.null(event_to_edit())),
                                 language = reactive(input$language))
  
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
    show_form()
    
  })
  
  # exit sidebar mode
  # this is called when saving and when pressing cancel
  exit_form <- function() {
    # reset all input fields
    form$reset_values(TRUE)
    
    # hide sidebar
    shinyjs::hide("form_panel")
    shinyjs::enable("add_event")
    shinyjs::disable("clone_event")
    if (golem::app_dev() || auth_result$admin == "TRUE") {
      shinyjs::enable("site")
    }
  }
  
  show_form <- function() {
    shinyjs::show("form_panel")
    shinyjs::disable("add_event")
    shinyjs::enable("clone_event")
    if (golem::app_dev() || auth_result$admin == "TRUE") {
      shinyjs::disable("site")
    }
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
      events$by_block[[block]] <- retrieve_json_info(site1, block)
    }
  }
  
  # cancel means we exit edit mode and hide the form
  observeEvent(form$values$cancel(), {
    if (is.null(event_to_edit())) {
      exit_form()
    } else {
      event_to_edit(NULL)
    }
  })
  
  # show add event UI when requested
  observeEvent(input$add_event, {
    # note: event_to_edit() should be NULL at this point as the add button is
    # disabled during editing
    if (!is.null(event_to_edit())) return()
      
    # if we were not editing previously, copy current table view settings
    # (block and activity) into the widgets if they are set to a specific
    # value (not "all")
    new_values <- list()
    table_activity <- event_list$filters()$activity
    table_block <- event_list$filters()$block
    
    if (!is.null(table_activity) && 
        table_activity != "activity_choice_all") {
      new_values$mgmt_operations_event <- table_activity
    }
    if (!is.null(table_block) &&
        table_block != "block_choice_all") {
      new_values$block <- table_block
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
    
    block_data <- retrieve_json_info(input$site, event$block)
    
    block_data[[length(block_data) + 1]] <- event
    
    # save changes
    write_json_file(input$site, event$block, block_data)
    showNotification("Cloned successfully.", type = "message")
    
    # update events data
    events$by_block[[event$block]] <- block_data
  })
  
  # save input to a file when save button is pressed
  # we are either creating a new event or editing an older one
  observeEvent(form$values$save(), {
    # fetch new values from the form
    event <- form$values$data()
    # are we editing an existing event or creating a new one?
    orig_event <- event_to_edit()
    editing <- !is.null(orig_event)
    
    # if we are editing, find the index of the event in the original 
    # block data list. Also, if the block has been changed, update that 
    # file. If the block has not changed, we will need the index when
    # replacing the old event with the updated one.
    if (editing) {
      
      orig_block_data <- retrieve_json_info(input$site, orig_event$block)
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
    
    #str(event)
    
    # load the json file corresponding to the new block selection (new as in
    # the current event$block value). We load from the file because it might
    # have changed and events$by_block might be out of date
    new_block_data <- retrieve_json_info(input$site, event$block)
    
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
    block_data <- retrieve_json_info(input$site, event$block)
    
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
  })
  
  # update each of the text outputs automatically, including language changes
  # and the dynamic updating in editing table title etc. 
  lapply(text_output_code_names, FUN = function(text_output_code_name) {
    
    # render text
    output[[text_output_code_name]] <- renderText({
      
      if (dp()) message(glue("Rendering text for {text_output_code_name}"))
      
      text_to_show <- get_disp_name(text_output_code_name, input$language)
      
      #get element from the UI structure lookup list
      element <- structure_lookup_list[[text_output_code_name]]
      #if the text should be updated dynamically, do that
      if (!is.null(element$dynamic)) {
        
        # there are currently two modes of dynamic text
        if (element$dynamic$mode == "input") {
          # the -1 removes the mode element, we don't want it
          patterns <- names(element$dynamic)[-1]
          # use lapply here to get the dependency on input correctly
          replacements <- lapply(patterns, function(pattern) {
            replacement <- input[[ element$dynamic[[pattern]] ]]
            replacement <- get_disp_name(replacement,
                                         input$language)
            text_to_show <<- gsub(pattern, replacement, 
                                  text_to_show)
            replacement
          })
          
          # if one of the replacements is empty, we don't want to
          # see the text at all
          if ("" %in% replacements) { text_to_show <- "" }
          
        } else if (element$dynamic$mode == "edit_mode") {
          
          text_to_show <- if (!is.null(event_to_edit())) {
            element$dynamic[["TRUE"]]
          } else {
            element$dynamic[["FALSE"]]
          }
          text_to_show <- get_disp_name(text_to_show, input$language)
          
        }
      }
      text_to_show
    })
    
  })
  
  # change language when user requests it
  observeEvent(input$language, {
    
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
        choices <- get_selectInput_choices(element, input$language)
        
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
        
        
      } else if (element$type == "dateInput") {
        #language_code <- if (input$language == "disp_name_fin") {
        #    "fi"
        #} else {
        #    "en"
        #}
        updateDateInput(session, 
                        code_name, 
                        label = label,
                        #language = language_code
        )
      } else if (element$type == "textAreaInput") {
        updateTextAreaInput(session,
                            code_name,
                            label = label,
                            placeholder = 
                              get_disp_name(
                                element$placeholder, 
                                input$language))
      } else if (element$type == "actionButton") {
        updateActionButton(session,
                           code_name,
                           label = label)
      } else if (element$type == "checkboxInput") {
        updateCheckboxInput(session,
                            code_name,
                            label = label)
      } else if (element$type == "textInput") {
        updateTextInput(session, 
                        code_name, 
                        label = label,
                        placeholder = 
                          get_disp_name(
                            element$placeholder, 
                            input$language))
      } else if (element$type == "numericInput") {
        updateNumericInput(session,
                           code_name,
                           label = label)
      } else if (element$type == "dateRangeInput") {
        updateDateRangeInput(session,
                             code_name,
                             label = label)
      } else if (element$type == "fileInput") {
        #update_ui_element(session, code_name, label = label)
      }
      
    }

  })
}
