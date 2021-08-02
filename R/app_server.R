#' Find the index of an event in a list
#' 
#' Find the first index corresponding to the given event in a list of events.
#' An event is considered equal to another if they have exactly the same
#' variables (though not necessarily in the same order) and these variables
#' have exactly the same values.
#' 
#' @param event The event whose index to identify
#' @param event_list The list of events where event is to be found
#' 
#' @return The index if found, NULL otherwise
find_event_index <- function(event, event_list) {
  
  if (length(event_list) == 0) { return(NULL) }
  
  # sort the items in the lists to the same order (alphabetical)
  event <- event[order(names(event))]
  
  # go through all rows in the event list and check if any of them match
  for (i in 1:length(event_list)) {
    
    list_event <- event_list[[i]]
    # sort the items in this particular list event, to allow comparison
    list_event <- list_event[order(names(list_event))]
    
    if (identical(event, list_event)) {
      return(i)
    }
    
  }
  
  # We didn't find a match, so return NULL
  return(NULL)
}

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
  # might have to use the hand-typed passphrase option for now when deploying
  # the app
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
  
  # initialise in the normal (non-edit) mode
  event_to_edit <- reactiveVal()
  # lists of events by block on the currently viewed site
  # accessed like events$by_block[["0"]]
  events <- reactiveValues(by_block = list())
  # per session global variable, indicates whether the currently edited event
  # is visible in the front page table
  edited_event_visible <- TRUE
  # what were the table view choices (table_block, table_activity, table_year)
  # before we started editing?
  pre_edit_table_view <- NULL
  
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
      shinyjs::disable("clone_event")
      # clear event list selection (might be clear already though)
      DT::selectRows(proxy = DT::dataTableProxy("mgmt_events_table"), 
                     selected = NULL)
      # hide form and clear values
      exit_form()
      
      # restore table view settings
      updateSelectInput(session, "table_activity", 
                        selected = pre_edit_table_view$activity)
      updateSelectInput(session, "table_block", 
                        selected = pre_edit_table_view$block)
      updateSelectInput(session, "table_year", 
                        selected = pre_edit_table_view$year)
      # clear table view settings
      pre_edit_table_view <<- NULL
      
      return()
    } 
    
    ### edit mode was enabled, or there was a switch from one event to  another
    
    # fill values on the form
    form$set_values(event_to_edit())
    
    # save table view (to be restored when editing is over) if no settings
    # have been saved previously
    if (is.null(pre_edit_table_view)) {
      pre_edit_table_view <<- list(activity = input$table_activity,
                                   block = input$table_block,
                                   year = input$table_year)
    }
    
    # change view of the front page table
    updateSelectInput(session, "table_activity", 
                      selected = event_to_edit()$mgmt_operations_event)
    
    shinyjs::show("form_panel")
    shinyjs::disable("add_event")
    shinyjs::enable("clone_event")
    if (golem::app_dev() || auth_result$admin == "TRUE") {
      shinyjs::disable("site")
    }
    
  })
  
  # exit sidebar mode
  # this is called when saving and when pressing cancel
  # TODO: make obsolete
  exit_form <- function() {
    # reset all input fields
    form$reset_values(TRUE)
    
    # hide sidebar
    shinyjs::hide("form_panel")
    shinyjs::enable("add_event")
    if (golem::app_dev() || auth_result$admin == "TRUE") {
      shinyjs::enable("site")
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
  
  #' Update year choices in event list filter
  #' 
  #' Adds as choices all the years for which events have been recorded
  update_table_year_choices <- function() {
    
    years <- NULL
    
    # find years present in event dates
    for (event_list in events$by_block) {
      for (event in event_list) {
        
        # this shouldn't happen
        if (is.null(event$date)) next
        
        year <- format(as.Date(event$date, date_format_json), "%Y")
        
        if (!(year %in% years)) { years <- c(years, year) }
      }
    }
    
    years <- sort(years, decreasing = TRUE)
    table_year_choices <- c("year_choice_all", years)
    names(table_year_choices) <- get_disp_name(table_year_choices, 
                                               input$language)
    
    # retain current selection if possible
    current_choice <- input$table_year
    if (!isTruthy(current_choice) || !(current_choice %in% years)) {
      current_choice <- "year_choice_all"
    }
    
    updateSelectInput(session, "table_year", selected = current_choice,
                      choices = table_year_choices)
    
  }
  
  #' Update block choices in event list filter
  #' 
  #' Add all blocks of the current site as choices
  update_table_block_choices <- function() {
    
    if (!isTruthy(input$site)) { return() }
    
    block_choices <- c("block_choice_all",
                       subset(sites, sites$site == input$site)$blocks[[1]])
    # the following assumes that no block name is a code name for something
    # else
    names(block_choices) <- get_disp_name(block_choices, input$language)
    
    current_choice <- input$table_block
    if (!isTruthy(current_choice) || !(current_choice %in% block_choices)) {
      current_choice <- "block_choice_all"
    }
    
    updateSelectInput(session, "table_block", selected = current_choice,
                      choices = block_choices)
  }
  
  #' Update activity choices in event list filter
  #' 
  #' Only used when the language is changed.
  update_table_activity_choices <- function() {
    
    choices_for_table_activity <- 
      c("activity_choice_all", get_category_names(
        "mgmt_operations_event_choice"))
    names(choices_for_table_activity) <- 
      get_disp_name(choices_for_table_activity, input$language)
    
    current_choice <- input$table_activity
    if (!isTruthy(current_choice)) {
      current_choice <- "activity_choice_all"
    }
    
    updateSelectInput(session, "table_activity", selected = current_choice,
                      choices = choices_for_table_activity)
  }
  
  # update year choices when events change
  observeEvent(events$by_block, {
    update_table_year_choices()
  })
  
  # data to display in the table
  frontpage_table_data <- reactive({
    
    if (dp()) message("frontpage_table_data reactive running")
    
    if (!(isTruthy(input$table_activity) & 
          isTruthy(input$table_block) &
          isTruthy(input$table_year))) {
      default_variables <- c("block", "mgmt_operations_event",
                             "date", "mgmt_event_notes")
      return(get_data_table(list(), default_variables))
    }
    
    # determine the columns displayed in the table
    table_variables <- c("date", "mgmt_event_notes")
    if (input$table_activity == "activity_choice_all") {
      table_variables <- c("mgmt_operations_event", table_variables)
    }
    if (input$table_block == "block_choice_all") {
      table_variables <- c("block", table_variables)
    }
    
    # if we are only looking at a specific event type, show columns
    # appropriate to it
    if (input$table_activity != "activity_choice_all") {
      hidden_widget_types <- c("textOutput", "dataTable", "fileInput", 
                               "actionButton")
      activity_variables <- unlist(rlapply(
        activity_options[[input$table_activity]],
        fun = function(x) {
          if (is.null(x$type) || x$type %in% hidden_widget_types ||
              identical(x$hide_in_table, TRUE)) {
            NULL
          } else {
            x$code_name
          }
        }))
      table_variables <- c(table_variables, activity_variables)
    }
    
    # create an event list filtered by user choices
    # filter by block
    if (input$table_block == "block_choice_all") {
      event_list <- list()
      for (block_data in events$by_block) {
        event_list <- c(event_list, block_data)
      }
    } else {
      event_list <- events$by_block[[input$table_block]]
    }
    
    # filter by activity type
    if (input$table_activity != "activity_choice_all") {
      event_list <- rlapply(event_list, fun = function(x)
        if (x$mgmt_operations_event == input$table_activity) {x})
    }
    
    # filter by year
    if (input$table_year != "year_choice_all") {
      event_list <- rlapply(event_list, fun = function(x) {
        event_year <- format(as.Date(x$date, date_format_json), "%Y")
        if (event_year == input$table_year) {x}
      })
    }
    
    # make event list into a table
    data <- get_data_table(event_list, table_variables)
    data
    
    # update currently displayed data
    #new_data_to_display <- replace_with_display_names(data, input$language)
    #DTproxy <- DT::dataTableProxy("mgmt_events_table", session = session)
    #DT::replaceData(DTproxy, new_data_to_display, rownames = FALSE, 
    #                clearSelection = "none")
  })
  
  # enable editing of old entries
  observeEvent(input$mgmt_events_table_rows_selected, ignoreNULL = FALSE,
               ignoreInit = TRUE, {
    
    if (dp()) message("Change of row selection in event list")
    
    row_index <- input$mgmt_events_table_rows_selected
    
    if (is.null(row_index)) {
      # if it was the user de-selecting the event, exit edit mode
      # (the other alternative is that the currently edited event is not
      # visible in the table and therefore no element can be selected)
      if (edited_event_visible) {
        event_to_edit(NULL)
      }
      return()
    }
    
    # fetch the event data of the selected row
    selected_event_data <- frontpage_table_data()[[row_index, "event"]]
    
    # set edit mode on. This saves the event we want to edit so that it is
    # preserved even if front page table view is changed
    event_to_edit(selected_event_data)
    
  })
  
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
    shinyjs::disable("add_event")
    
    # if we were not editing previously, copy current table view settings
    # (block and activity) into the widgets if they are set to a specific
    # value (not "all")
    # note: event_to_edit() should be NULL at this point as the add button is
    # disabled during editing

    if (is.null(event_to_edit())) {
      
      new_values <- list()
      
      if (!is.null(input$table_activity) && 
          input$table_activity != "activity_choice_all") {
        new_values$mgmt_operations_event <- input$table_activity
      }
      if (!is.null(input$table_block) &&
          input$table_block != "block_choice_all") {
        new_values$block <- input$table_block
      }
      
      if (!identical(new_values, list())) form$set_values(new_values)
    }
    
    # exit edit mode if we were in it
    event_to_edit(NULL)
    
    # show the form
    shinyjs::show("form_panel")
    
    if (golem::app_dev() || auth_result$admin == "TRUE") {
      shinyjs::disable("site")
    }
  })
  
  # TODO: what if the event has a file associated with it?
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
  
  # change available blocks depending on the site and load the site event
  # data into memory (events$by_block)
  observeEvent(input$site, ignoreNULL = FALSE, {
    
    if (dp()) message("input$site was changed")
    
    if (!isTruthy(input$site)) {
      shinyjs::disable("table_block")
      shinyjs::disable("add_event")
      return()
    } 
    
    shinyjs::enable("table_block")
    shinyjs::enable("add_event")
    
    update_table_block_choices()

    if (dp()) message(glue::glue("Loading data for site {input$site}"))
    # load the events corresponding to this site into memory
    load_json_data(input$site)
    
  })

  # render frontpage table when input$language or table data changes
  output$mgmt_events_table <- DT::renderDataTable(server = FALSE, {
    
    if (dp()) message("Rendering event list")
    
    new_data_to_display <- replace_with_display_names(
      frontpage_table_data(), input$language)
    n_cols <- ncol(new_data_to_display)
    
    # select the row which we are currently editing
    row_number <- NULL
    if (!is.null(isolate(event_to_edit()))) {
      row_number <- find_event_index(isolate(event_to_edit()), 
                                     new_data_to_display$event)
      # if we couldn't find the currently edited event in the table,
      # prevent clearing the event
      edited_event_visible <<- !is.null(row_number)
    }
    
    DT::datatable(new_data_to_display, 
              # allow selection of a single row
              selection = list(mode = "single", 
                               selected = row_number),
              rownames = FALSE, # hide row numbers
              class = "table table-hover",
              #autoHideNavigation = TRUE, doesn't work properly with dom
              colnames = get_disp_name(names(new_data_to_display),
                                       language = input$language,
                                       is_variable_name = TRUE),
              options = list(dom = 'tp', # hide unnecessary controls
                             # order chronologically by hidden column
                             order = list(n_cols - 1, 'desc'), 
                             columnDefs = list(
                               # hide all other columns except
                               # event, date and notes
                               list(visible = FALSE, 
                                    targets = (n_cols - 2):(n_cols - 1)),
                               # hide sorting arrows
                               list(orderable = FALSE, targets = 
                                      0:(n_cols - 2))),
                             pageLength = 15,
                             language = list(
                               emptyTable = get_disp_name(
                                 "table_empty_label", input$language),
                               paginate = list(
                                 "next" = get_disp_name(
                                   "table_next_label", input$language), 
                                 previous = get_disp_name(
                                   "table_previous_label", input$language))
                             )
              ))
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
    
    # update table selector choices separately
    update_table_block_choices()
    update_table_activity_choices()
    update_table_year_choices()
  })
}
