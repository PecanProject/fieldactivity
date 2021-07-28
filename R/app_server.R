# whether to print debug information
log <- function() TRUE && golem::app_dev()

if (log()) message("beginning of app_server.R")

#' Reset the value of input fields
#' 
#' Set the specified input fields to their default empty values.
#' 
#' @param session The current Shiny session
#' @param fields_to_clear The names of the variables whose corresponding fields
#'   should be cleared
#' @param exceptions Optional vector of variable names which should not be
#'   cleared. This is useful if fields_to_clear is supplied with all variable
#'   names but there are a few that should not be cleared.
#' @return None, used for side effects.
#' @note This doesn't reset the tables (e.g. harvest_crop_table) -- they reset 
#'   themselves every time they become hidden.
# TODO: make sure this is always used
# TODO: is exceptions necessary?
reset_input_fields <- function(session, fields_to_clear, exceptions = c("")) {
  
  # we never want to clear the site or block
  exceptions <- c(exceptions, "site", "block")
  
  for (code_name in fields_to_clear) {
    if (code_name %in% exceptions) next
    update_ui_element(session, code_name, clear_value = TRUE)
  }
  
}

# takes a list of events and makes a data table with given variables in columns.
# also adds a column with the complete event list and a final column for
# ordering the list by date.
# The function doesn't replace code names with display names. That is done
# separately so that when the app language is switched, we can change the
# table display names without having to create it again.
get_data_table <- function(events, variable_names) {
  # initialise table
  display_data_table <- data.frame()
  for (variable_name in variable_names) {
    # get corresponding element and initialise the corresponding table column
    element <- structure_lookup_list[[variable_name]]
    display_data_table[[variable_name]] <- list()
  }
  # the event column will hold the complete event information as a list
  display_data_table$event <- list()
  # the date_ordering column will hold dates for ordering table
  display_data_table$date_ordering <- character()
  
  row_number <- 1
  for (event in events) {
    
    for (variable_name in variable_names) {
      
      value <- event[[variable_name]]
      if (is.null(value)) {
        value <- ""
      }
      # if value is a vector, it will be turned into a single string
      # when the table is converted to a table with display names
      
      display_data_table[[row_number, variable_name]] <- value
    }
    
    # double brackets allow saving a list nicely
    display_data_table[[row_number, "event"]] <- event
    display_data_table[row_number, "date_ordering"] <- 
      as.Date(event$date, format = date_format_json)
    
    row_number <- row_number + 1
  }
  
  return(display_data_table)
}

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
    list_event <- list_event[order(names(list_event))]
    
    if (identical(event, list_event)) {
      return(i)
    }
    
  }
  
  # We didn't find a match, so return NULL
  return(NULL)
}

# if a variable is in a table (e.g. planting_depth is in a table when 
# planted_crop has multiple values), return the code name of that table. 
# Otherwise return NULL.
# only_values determines which variables we seek:
# TRUE: only return the table code name if the variable is one whose
# value is entered in the table, e.g. all variables in fertilizer_element_table
# but not harvests_crop in harvest_crop_table, since harvest_crop's value is
# entered in a regular widget.
# FALSE: return table code name if variable is present in the table in any
# form, e.g. harvest_crop also returns harvest_crop_table since it is on the 
# rows of that table
get_variable_table <- function(variable_name, only_values = FALSE) {
  
  for (table_code_name in data_table_code_names) {
    table <- structure_lookup_list[[table_code_name]]
    
    # determine the variable names to check if they match variable_name
    names_to_check <- if (is.null(table$columns)) {
      # if columns is not defined, the table is in custom mode (e.g. 
      # fertilizer_element_table) and all variables (stored in rows) are
      # to be checked
      unlist(table$rows)
    } else {
      # in a normal table, the “value variables” are given in the columns
      if (!only_values) {
        c(table$rows, table$columns)
      } else {
        table$columns
      }
    }
    
    if (variable_name %in% names_to_check) {
      return(table_code_name)
    }
  }
  
  return(NULL)
}



#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom glue glue
#' @noRd
app_server <- function( input, output, session ) {
  # run interactive themer
  #bslib::bs_themer()
  if (log()) message("Initializing server function")
  
  # check_credentials returns a function to authenticate users
  # might have to use the hand-typed passphrase option for now when deploying
  # the app
  credential_checker <- shinymanager::check_credentials(
    system.file("extdata", "database.sqlite", package = "fieldactivity"),
    # passphrase = key_get("FO-mgmt-events-key", "FO-mgmt-events-user")
    passphrase = "salasana"
  )
  
  # change login form language when requested
  observeEvent(input$login_language, {
    #str(reactiveValuesToList(input))
    #updateTextInput(session, "auth-user_id", value = "oma1")
    #updateTextInput(session, "auth-user_pwd", value = "oma2")
    #shinyjs::click("auth-go_auth") # doesn't work
    
    # yes we are overwriting the English language. This is by far
    # the simplest method
    
    if (log()) message("input$login_language")
    
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
    
    if (log()) message("auth_result$user")
    
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
  
  # go through all fields and set maxLength if requested in ui_structure.json
  for (element in structure_lookup_list) {
    if (!is.null(element$maxlength)) {
      js_message <- "$('##code_name').attr('maxlength', #maxlength)"
      js_message <- gsub("#code_name", element$code_name, js_message)
      js_message <- gsub("#maxlength", element$maxlength, js_message)
      shinyjs::runjs(js_message)
    }
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
  # as fileInput values cannot be reset (even with shinyjs), we need to store 
  # previous values here (by code_name) to check whether the value has changed
  session$userData$previous_fileInput_value <- list()
  
  observeEvent(event_to_edit(), ignoreNULL = FALSE, ignoreInit = TRUE, {
    
    if (log()) message("event_to_edit() changed")
    
    if (is.null(event_to_edit())) {
      # edit mode was disabled
      shinyjs::hide("delete")
      shinyjs::disable("clone_event")
      
      DT::selectRows(proxy = DT::dataTableProxy("mgmt_events_table"), 
                     selected = NULL)
      exit_sidebar_mode()
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
    
    # edit mode was enabled, or there was a switch from one event to
    # another
    
    # populate the input widgets with the values corresponding to the 
    # event, and clear others
    for (variable_name in get_category_names("variable_name")) {
      
      # get the value corresponding to this variable from the event.
      # might be NULL
      value <- event_to_edit()[[variable_name]]
      
      # determine if this value should be filled in a table
      # for now this is a sufficient condition
      variable_table <- get_variable_table(variable_name, 
                                           only_values = TRUE)
      value_in_table <- !is.null(variable_table) & length(value) > 1
      
      if (!(variable_name %in% names(event_to_edit())) | value_in_table) {
        # clear widget if the event does not contain a value for it
        # or value should be shown in a table instead
        update_ui_element(session, variable_name, clear_value = TRUE)
      } else {
        update_ui_element(session, variable_name, value = value)
      }
    }
    
    # then go through all the variables in the event and see if any of 
    # them should be displayed in the table. If yes, fill the table.
    # Other tables do not need to be cleared, as they do that by 
    # themselves when they become hidden.
    for (variable_name in names(event_to_edit())) {
      variable_table <- get_variable_table(variable_name, 
                                           only_values = TRUE)
      
      if (!is.null(variable_table)) {
        prefill_values[[variable_table]](event_to_edit())
        break
      }
    }
    
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
    
    shinyjs::show("delete")
    shinyjs::show("sidebar")
    shinyjs::disable("add_event")
    shinyjs::enable("clone_event")
    if (golem::app_dev() || auth_result$admin == "TRUE") {
      shinyjs::disable("site")
    }
    
  })
  
  # exit sidebar mode
  # this is called when saving and when pressing cancel
  # TODO: make obsolete
  exit_sidebar_mode <- function() {
    # reset all input fields
    reset_input_fields(session, get_category_names("variable_name"))
    # hide sidebar
    shinyjs::hide("sidebar")
    shinyjs::enable("add_event")
    if (golem::app_dev() || auth_result$admin == "TRUE") {
      shinyjs::enable("site")
    }
  }
  
  # load data from all the json files corresponding to a site and store it in
  # separate lists in events$by_block
  # TODO: move elsewhere, return the list instead of saving it here
  # maybe fuse with retrieve_json_info?
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
  
  # TODO: this can be sped up by keeping a up-to-date list of event years
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
  
  # takes a condition written in javascript notation (visibility conditions
  # in ui_structure.json) and evaluates it in R.
  # Returns either TRUE or FALSE. If the condition could not be evaluated, 
  # returns NULL. 
  # Might not be best coding practice, but works as long as the js_condition
  # doesn't have any typos. I know eval(parse(...)) is dangerous if it is used
  # directly with user input, but here that is not the case. The user has no 
  # access to the ui_structure.json file.
  evaluate_condition <- function(js_condition) {
    
    if (is.null(js_condition) || !is.character(js_condition)) {
      return(NULL)
    }
    
    # substitute dots with dollar signs 
    # (fixed = TRUE means we don't use regex)
    condition <- gsub("input.", "input$", js_condition, fixed = TRUE)
    
    # if the condition relates to the length of something, modify it
    # to look like R. e.g. change "thing.length > 1" to "length(thing) > 1"
    if (stringr::str_detect(condition, ".length")) {
      length_index <- stringr::str_locate(condition, ".length")
      start <- stringr::str_sub(condition, end = length_index[,"start"]-1)
      condition <- paste0("length(", start, ")", 
                          stringr::str_sub(condition, 
                                  start = length_index[,"end"] + 1))
    }
    
    # replace 'true' with 'TRUE' and 'false' with 'FALSE'
    if (condition == "true") {
      condition <- "TRUE"
    } else if (condition == "false") {
      condition <- "FALSE"
    }
    
    #message(glue("Evaluating condition {condition}"))
    
    # parse string into an expression and evaluate it
    tryCatch(
      expr = eval(parse(text = condition)),
      error = function(cnd) {
        message(glue("Condition {condition} could not be evaluated"))
        NULL
      }
    )
  }
  
  # update year choices when events change
  observeEvent(events$by_block, {
    update_table_year_choices()
  })
  
  # data to display in the table
  frontpage_table_data <- reactive({
    
    if (log()) message("frontpage_table_data reactive running")
    
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
    
    if (log()) message("Change of row selection in event list")
    
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
    selected_event_data <- frontpage_table_data()[[row_index,"event"]]
    
    # set edit mode on. This saves the event we want to edit so that it is
    # preserved even if front page table view is changed
    event_to_edit(selected_event_data)
    
  })
  
  # cancel means we exit edit mode and hide sidebar controls
  observeEvent(input$cancel, {
    if (is.null(event_to_edit())) {
      exit_sidebar_mode()
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
    # note: event_to_edit() should be NULL as the add button is disabled
    # during editing
    if (is.null(event_to_edit())) {
      if (!is.null(input$table_activity) && 
          input$table_activity != "activity_choice_all") {
        update_ui_element(session, "mgmt_operations_event", 
                          value = input$table_activity)
      }
      if (!is.null(input$table_block) &&
          input$table_block != "block_choice_all") {
        update_ui_element(session, "block", value = input$table_block)
      }
    }
    
    # exit edit mode if we were in it
    event_to_edit(NULL)
    
    # show sidebar
    shinyjs::show("sidebar")
    
    if (golem::app_dev() || auth_result$admin == "TRUE") {
      shinyjs::disable("site")
    }
  })
  
  observeEvent(input$clone_event, {
    # fetch the event to be cloned
    event <- event_to_edit()
    
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
  observeEvent(input$save, {
    
    # are we editing an existing event or creating a new one?
    editing <- !is.null(event_to_edit())
    # let's create a list which we will update to match the event info
    event <- if (editing) {
      event_to_edit()
    } else {
      list()
    }
    orig_block <- event$block
    orig_date <- event$date
    
    # if we are editing, find the index of the event in the original 
    # block data list. Also, if the block has been changed, update that 
    # file. If the block has not changed, we will need the index when
    # replacing the old event with the updated one.
    if (editing) {
      
      orig_block_data <- retrieve_json_info(input$site, orig_block)
      event_index <- find_event_index(event, orig_block_data)
      
      if (is.null(event_index)) {
        showNotification("Could not edit entry because it was not 
                                 found in the event files.", type = "error")
        return()
      }
      
      # if the block of the event has been changed, delete it from the 
      # original block file
      if (orig_block != input$block) {
        orig_block_data[event_index] <- NULL
        write_json_file(input$site, orig_block, orig_block_data)
        events$by_block[[orig_block]] <- orig_block_data
      }
      
    }
    
    # fill out current_event to match new / updated data.
    # find variables that correspond to the selected activity and save
    # only those
    widget_list <- activity_options[[input$mgmt_operations_event]]
    
    relevant_variables <- unlist(rlapply(widget_list,
                                         fun = function(x) x$code_name))
    relevant_variables <- c("block", 
                            "mgmt_operations_event",
                            "date",
                            "mgmt_event_notes",
                            relevant_variables)
    # if a variable group's visibility condition evaluates to FALSE, 
    # the variables in the group are not relevant and they are added to 
    # the list of variables to be skipped.
    # This list may include variables which we don't want to skip but
    # will actually want to read from a table, not from a regular widget.
    # That will be handled later.
    skip_variables <- unlist(rlapply(widget_list, fun = function(x) {
      if (!is.null(x$condition)) {
        relevant <- evaluate_condition(x$condition)
        if (!is.null(relevant) && is.na(relevant) || !relevant) {
          rlapply(x, fun = function(x) x$code_name)
        }
      }
    }))
    
    # determine whether we need to read some variables from a table or not
    table_to_read <- NULL
    for (table_code_name in data_table_code_names) {
      if (visible[[table_code_name]]) {
        table_to_read <- structure_lookup_list[[table_code_name]]
        break
      }
    }
    
    # fill / update information
    for (variable_name in get_category_names("variable_name")) {
      
      # will be needed later with fileInputs
      widget <- structure_lookup_list[[variable_name]]
      
      # should the variable's value be read from a table?
      read_from_table <- if (!is.null(table_to_read)) {
        # if it is in the table's columns, yes
        if (variable_name %in% table_to_read$columns) {TRUE}
        # if it is in a custom mode table (=fertilizer_element_table)
        # then yes too
        else if (variable_name %in% unlist(table_to_read$rows)) {TRUE}
        # if not, then it is in the rows of the table and we will read
        # the value instead from a regular widget
        else {FALSE}
      } else {FALSE}
      
      # if this variable is not relevant, make sure it is not included
      # in the event data
      if (!(variable_name %in% relevant_variables) ||
          # variable might be in skip_variables if it is read from table
          variable_name %in% skip_variables & !read_from_table) {
        
        # if this variable is a path to a file and it has a value 
        # stored, we need to delete the file as it is no longer 
        # relevant to the event
        value <- event[[variable_name]]
        if (widget$type == "fileInput" && 
            !is.null(value) && !identical(value, missingval)) {
          # delete the file
          tryCatch(expr = delete_file(value, input$site, event$block),
                   error = function(cnd) {
                     message(glue("Could not delete file related ",
                                  "to the edited event: {cnd}"))
                   })
        } 
        
        event[variable_name] <- NULL
        next
      }
      
      # read value from table if it is available there, otherwise
      # read the value from a regular input widget
      value_to_save <- if (read_from_table) {
        table_data[[table_to_read$code_name]]()[[variable_name]]
      } else {
        input[[variable_name]]
      }
      
      # if value is character, trim any whitespace around it
      if (is.character(value_to_save)) {
        value_to_save <- trimws(value_to_save)
      }
      
      # format Date value to character string and replace with ""
      # if that fails for some reason
      if (class(value_to_save) == "Date") {
        value_to_save <- tryCatch(
          expr = format(value_to_save, date_format_json),
          error = function(cnd) {
            message(glue("Unable to format date {value_to_save}",
                         "into string when saving event,",
                         "replaced with missingval")) 
            ""
          }
        )
      }
      
      # handle fileInputs that are relevant to the event
      if (widget$type == "fileInput") {
        
        # the value of a fileInput cannot be reset, so we need to
        # compare the current value to the old one to figure out if
        # a new value has been entered
        prev_value <- 
          session$userData$previous_fileInput_value[[variable_name]]
        # should currently saved file be deleted? We know this by
        # the flag we set to session$userData when pressing the delete
        # button
        clear_value <- !is.null(prev_value$clear_value)
        # is a new file uploaded?
        new_file_uploaded <- !is.null(value_to_save) & 
          !identical(value_to_save, prev_value) &
          !clear_value
        old_path <- event[[variable_name]]
        
        if (new_file_uploaded) {
          filepath <- value_to_save$datapath
          # move the file into place and get the relative path
          relative_path <- 
            tryCatch(expr = 
                       move_uploaded_file(filepath, 
                                          variable_name,
                                          input$site, 
                                          input$block, 
                                          format(input$date, 
                                                 date_format_json)),
                     error = function(cnd) {
                       showNotification(
                         "Could not save the image file correctly.", 
                         type = "warning")
                       message(glue("Error when saving image to ",
                                    "{variable_name}: {cnd}"))
                       ""
                     })
          
          # if the event already has a value in this field, we are 
          # replacing the file with a new one. We should therefore 
          # delete the old file.
          if (!is.null(old_path) && !identical(old_path, missingval)) {
            # delete the file
            tryCatch(expr = delete_file(old_path, input$site, 
                                        event$block),
                     error = function(cnd) {
                       message(glue("Could not delete file to ",
                                    "be replaced: {cnd}"))
                     })
          }
          
          value_to_save <- relative_path
        } else if (clear_value) {
          # the file was deleted by the user using the delete button.
          # Let's actually delete the file and save changes
          tryCatch(expr = delete_file(old_path, input$site, 
                                      event$block),
                   error = function(cnd) {
                     message(glue("Could not delete file related ",
                                  "to the event: {cnd}"))
                   })
          
          value_to_save <- NULL
        } else {
          # a new file was not uploaded. 
          # However, if there is already a file uploaded, we might 
          # have to rename/move it as the event date or block might
          # have changed. We will therefore move the current file
          # like it was a new file.
          
          new_date <- format(input$date, date_format_json)
          if (editing && !identical(old_path, missingval)) {
            
            if (input$block != orig_block | new_date != orig_date) {
              # we should rename and/or move the file
              
              old_path <- file.path(input$site, orig_block, 
                                    old_path)
              
              relative_path <- 
                tryCatch(expr = move_uploaded_file(
                  old_path, 
                  variable_name,
                  input$site, 
                  input$block, 
                  new_date,
                  filepath_is_relative = TRUE),
                  error = function(cnd) {
                    showNotification(
                      "Could not rename the image file.", 
                      type = "warning")
                    message(glue("Error when renaming ",
                                 "image ",
                                 "{variable_name}: {cnd}"))
                    old_path
                  })
              
              value_to_save <- relative_path
              
            } else {
              # we did not have to move the file and therefore do
              # not need to change the current file path
              next
            }
            
          } else {
            # we are not editing (or path is missingval), so save 
            # missingval
            value_to_save <- NULL
          }
        }
      }
      
      
      # if the value is not defined or empty, replace with missingval
      if (length(value_to_save) == 0) {
        value_to_save <- missingval
      } else {
        missing_indexes <- is.na(value_to_save) | value_to_save == ""
        if (any(missing_indexes)) {
          value_to_save[missing_indexes] <- missingval
        }
      }
      
      # if value has multiple values (e.g. selectInput with possibility
      # of selecting multiple values), then make that into a list so that
      # it saves nicely into the event list
      # if we didn't do this, we'd get an error saying we have too many
      # replacement values
      #if (length(value_to_save) > 1) {
      #    value_to_save <- list(value_to_save)
      #}
      # EDIT: not needed as I changed what's below from [] to [[]]
      
      event[[variable_name]] <- value_to_save
    }
    
    #message("ALL THE DATA FILLED:")
    #str(event)
    
    # load the json file corresponding to the new block selection (new as in
    # the current input$block value). We load from the file because it might
    # have changed and events$by_block might be out of date
    new_block_data <- retrieve_json_info(input$site, input$block)
    
    # if editing and block didn't change, replace event. 
    # Otherwise append event to the list
    if (editing && orig_block == input$block) {
      new_block_data[[event_index]] <- event
    } else {
      new_block_data[[length(new_block_data) + 1]] <- event
    }
    
    # save changes
    write_json_file(input$site, input$block, new_block_data)
    showNotification("Saved successfully.", type = "message")
    
    # update events$by_block
    events$by_block[[input$block]] <- new_block_data
    
    # exit sidebar mode
    if (editing) {
      event_to_edit(NULL)
    } else {
      exit_sidebar_mode()
    }
    
  })
  
  # delete entry when delete button is pressed
  observeEvent(input$delete, {
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
    # delete those. For this we have to go through all the variables in
    # the event and check if they correspond to a fileInput
    for (variable in names(event)) {
      widget <- structure_lookup_list[[variable]]
      value <- event[[variable]]
      
      if (widget$type == "fileInput" & !identical(value, missingval)) {
        # delete the file
        tryCatch(expr = delete_file(value, input$site, event$block),
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
  observeEvent(input$site, ignoreNULL = FALSE, ignoreInit = TRUE, {
    
    if (log()) message("input$site was changed")
    
    if (is.null(input$site) || input$site == "") {
      shinyjs::disable("table_block")
      shinyjs::disable("block")
      shinyjs::disable("add_event")
      return()
    } 
    
    shinyjs::enable("table_block")
    shinyjs::enable("block")
    shinyjs::enable("add_event")
    
    # update table_block choices.
    # table_block choices are also updated in the observeEvent for
    # input$language to make the block_choice_all name translate
    block_choices <- subset(sites, sites$site == input$site)$blocks[[1]]
    update_table_block_choices()
    updateSelectInput(session, "block", choices = block_choices)
    
    if (log()) message(glue::glue("Loading data for site {input$site}"))
    # load the events corresponding to this site into memory
    load_json_data(input$site)
    
  })
  
  required_variables <- reactiveVal(list("site", "block", "date",
                                         "mgmt_operations_event"))
  
  # change required variables when activity is changed
  observeEvent(input$mgmt_operations_event, {
    
    required_checker <- function(element) {
      # if required is defined, it is true
      if (!is.null(element$required)) {
        return(element$code_name)
      }
    }
    
    # find the variables that are compulsory for this activity type
    variables <- rlapply(
      activity_options[[input$mgmt_operations_event]],
      fun = required_checker)
    variables <- c(list("site", 
                        "block", 
                        "mgmt_operations_event",
                        "date"),
                   variables)
    # save to a reactiveval. The inputs are compared against this list of
    # variables in an observe()
    required_variables(variables)
  })
  
  # disable the save button if not all necessary info has been filled
  observe({
    
    if (log()) message("Required variables observe")
    
    #if (!dev_mode) {req(auth_result$admin)}
    #req(auth_result$admin)
    
    # run whenever any of the inputs change. I know this is not ideal, but
    # reactivity to input values doesn't work when we dynamically generate
    # which inputs we want to access
    reactiveValuesToList(input)
    
    # if (dev_mode || auth_result$admin == "TRUE") {
    # if we are in admin or dev mode, 
    # we don't care about required variables
    # return()
    # }
    
    for (required_variable in required_variables()) {
      
      table_code_name <- get_variable_table(required_variable)
      
      # read the value from a table or a widget, depending on which one
      # is appropriate
      current_val <- if (!is.null(table_code_name) && 
                         visible[[table_code_name]]) {
        table_data[[table_code_name]]()[[required_variable]]
      } else {
        input[[required_variable]]
      }
      
      # is.Truthy essentially checks whether input is NULL, NA or "".
      # Turning current_val into a list ensures that NULLs are evaluated
      # correctly.
      is_filled <- sapply(list(current_val), isTruthy)
      
      if (!all(is_filled)) {
        shinyjs::disable("save")
        return()
      }
    }
    
    shinyjs::enable("save")
  })
  
  # render frontpage table when input$language or table data changes
  output$mgmt_events_table <- DT::renderDataTable(server = FALSE, {
    
    if (log()) message("Rendering event list")
    
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
  
  # holds boolean values which indicate whether the conditions for the 
  # visibility of data tables are met
  visible <- reactiveValues()
  
  # changing these overrides the values in the table
  prefill_values <- list()
  
  # initialise the table server for each of the dynamically added tables
  # sapply with simplify = FALSE is equivalent to lapply
  table_data <- sapply(data_table_code_names,
                       FUN = function(data_table_code_name) {
                         table_structure <- structure_lookup_list[[data_table_code_name]]
                         
                         # are we in static mode, i.e. are all row groups of
                         # type 'static'? If yes, we won't need to supply the
                         # row_variable_value reactive. Currently this only
                         # happens with fertilizer_element_table (the columns
                         # are not defined)
                         static_mode <- is.null(table_structure$columns)
                         
                         # If we have row groups which depend on widget values
                         # in the main app, create a reactive from those values.
                         # This can either be determined by the choices of
                         # selectInput with multiple selections, or a
                         # numericInput which represents the number of rows.
                         row_variable_value <- reactive({
                           
                           if (static_mode) {
                             NULL
                             
                           } else {
                             
                             # find the row variable
                             for (row_group in table_structure$rows) {
                               # there is only one dynamic row group
                               if (row_group$type == 'dynamic') {
                                 row_variable <- row_group$row_variable
                                 break
                               }
                             }
                             
                             row_variable <- structure_lookup_list[[row_variable]]
                             if (row_variable$type == "numericInput") {
                               
                               number_of_rows <- input[[row_variable$code_name]]
                               
                               if (!isTruthy(number_of_rows)) {
                                 NULL
                               } else {
                                 as.integer(number_of_rows)
                               }
                               
                             } else if (row_variable$type == "selectInput") {
                               input[[row_variable$code_name]]
                             }
                             
                           }
                         })
                         
                         # Determine when the table is visible. The table module
                         # needs this information.
                         if (static_mode) {
                           # since fertilizer_element_table is currently the
                           # only table utilising static mode, we can use this
                           # condition to determine its visibility.
                           observeEvent(input$mgmt_operations_event, ignoreNULL = FALSE, {
                             visible[[data_table_code_name]] <- 
                               identical(input$mgmt_operations_event, "fertilizer")
                           })
                         } else {
                           # table is visible if the length of the variable
                           # presented on the rows of the table is more than 1,
                           # or if its numeric value is greater than 1
                           observeEvent(row_variable_value(), ignoreNULL = FALSE, {
                             visible[[data_table_code_name]] <- 
                               if (is.numeric(row_variable_value())) {
                                 row_variable_value() > 1
                               } else { 
                                 length(row_variable_value()) > 1 
                               }
                             
                             #message(glue("Visibility for {data_table_code_name} is {visible[[data_table_code_name]]}"))
                           })
                         }
                         
                         prefill_values[[data_table_code_name]] <<- reactiveVal()
                         mod_table_server(data_table_code_name, row_variable_value, reactive(input$language),
                                     visible = reactive(visible[[data_table_code_name]]),
                                     override_values = prefill_values[[data_table_code_name]])
                       }, USE.NAMES = TRUE, simplify = FALSE)
  
  # observe({
  #     for (value in table_data) {
  #         str(value())
  #     }
  # })
  
  # update each of the text outputs automatically, including language changes
  # and the dynamic updating in editing table title etc. 
  lapply(text_output_code_names, FUN = function(text_output_code_name) {
    
    # render text
    output[[text_output_code_name]] <- renderText({
      
      if (log()) message(glue("Rendering text for {text_output_code_name}"))
      
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
  
  # show file delete button when a new file is uploaded by the user.
  # Also check the file immediately after it is uploaded, and if its extension
  # is not correct, delete the file.
  lapply(fileInput_code_names, FUN = function(fileInput_code_name) {
    observeEvent(input[[fileInput_code_name]], {
      # path to the uploaded file in temporary folder
      tmp_path <- input[[fileInput_code_name]]$datapath
      file_extension <- tools::file_ext(tmp_path)
      allowed_extensions <- c("jpg", "jpeg", "tif", "tiff", "png")
      # if the image format is not desired, delete file and clear field
      if (!(file_extension %in% allowed_extensions)) {
        delete_file(tmp_path, filepath_relative = FALSE)
        
        showNotification(
          glue("This file extension is not supported. ",
               "Upload a file with one of the ",
               "following extensions: ",
               paste(allowed_extensions, collapse = ", ")), 
          type = "error", duration = NULL)
        update_ui_element(session, fileInput_code_name, 
                          clear_value = TRUE)
        return()
      }
      
      # a new file was uploaded, so show delete button
      delete_button_name <- 
        structure_lookup_list[[fileInput_code_name]]$delete_button
      shinyjs::show(delete_button_name)
    })
  })
  
  # add observers to fileInput delete buttons
  lapply(fileInput_delete_code_names, FUN = function(button_code_name) {
    observeEvent(input[[button_code_name]], {
      fileInput_code_name <- 
        structure_lookup_list[[button_code_name]]$fileInput
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
      new_file_uploaded <- 
        !identical(input[[fileInput_code_name]],
                   session$userData$
                     previous_fileInput_value[[fileInput_code_name]])
      event <- event_to_edit()
      editing <- !is.null(event)
      
      if (new_file_uploaded) {
        
        # # if we are editing, there might be a previous file
        # if (editing) {
        #     
        #     old_path <- event[[fileInput_code_name]]
        #     
        #     if (!is.null(old_path) & !identical(old_path, missingval)) {
        #         message(glue("Deleting new file and going back to ",
        #                      "{old_path}"))
        #     } else {
        #         message("Deleting new file, no previous files")
        #     }
        #     
        # } else {
        #     message("Deleting a newly uploaded file in add mode")
        # }
        
        # clear fileInput
        update_ui_element(session, fileInput_code_name, 
                          clear_value = TRUE)
        # TODO: delete the actual file? 
        
      } else {
        
        # new file was not uploaded. Check if we are editing and there
        # is a previous file we should delete
        old_path <- event[[fileInput_code_name]]
        if (editing && !is.null(old_path) && 
            !identical(old_path, missingval)) {
          
          # there is an old file we should delete when the changes
          # to the event are saved. To signal this, let's add a flag
          # to session$userData$previous_fileInput_value
          update_ui_element(session, fileInput_code_name, 
                            clear_value = TRUE)
          session$userData$previous_fileInput_value[[
            fileInput_code_name]] <- list(clear_value = TRUE)
          
        } else {
          # the button should not be visible at this point
          message("No new file uploaded and no old file to delete")
        }
        
      }
      
    })
  })
  
  # change language when user requests it
  observeEvent(input$language, {
    
    if (log()) message("input$language changed")
    
    # we have to handle input and output elements in different ways
    
    # OUTPUT ELEMENTS:
    
    # change textOutputs when the language is changed
    # one has to use lapply here, for-loop does not work! See
    # https://community.rstudio.com/t/how-do-i-use-for-loop-in-rendering-outputs/35761/2
    
    # function to render text outputs. Note the pattern matching which
    # is used for the editing table title (shown in text_output_handler)
    #lapply(text_output_code_names, text_output_handler, session = session,
    #       input = input,
    #       output = output)
    
    # no need to update data tables, their updating is defined in
    # update_tables and they update reactively when language changes
    
    # INPUT ELEMENTS:
    
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
        update_ui_element(session, code_name, label = label)
      }
      
    }
    
    # update table selector choices separately
    update_table_block_choices()
    update_table_activity_choices()
    update_table_year_choices()
  })
}
