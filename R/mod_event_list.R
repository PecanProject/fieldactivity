# The function of the event list module is as follows:
# - receives an unfiltered list of event lists to display
# - displays these events along with and according to filter selectors
# - returns 
#   - the event that is currently selected for editing 
#   - the choices of the filters


#' event_list UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_event_list_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # selector to filter table data
    div(style="display: inline-block;vertical-align:middle;",
        textOutput(ns("table_filter_text_1"), inline = TRUE)),
    div(style="display: inline-block;vertical-align:middle;", 
        selectInput(ns("table_activity"), label = "", choices = c(""), 
                    width = "150px")),
    div(style="display: inline-block;vertical-align:middle;",
        textOutput(ns("table_filter_text_2"), inline = TRUE)),
    div(style="display: inline-block;vertical-align:middle;", 
        selectInput(ns("table_block"), label = "", choices = c(""),
                    width = "100px")),
    div(style="display: inline-block;vertical-align:middle;",
        textOutput(ns("table_filter_text_3"), inline = TRUE)),
    div(style="display: inline-block;vertical-align:middle;", 
        selectInput(ns("table_year"), label = "", choices = c(""),
                    width = "100px")),
    div(style="display: inline-block;vertical-align:middle;", "."),
    
    # front page data table
    DT::dataTableOutput(ns("table"))
  )
}
    
#' event_list Server Functions
#'
#' @noRd
mod_event_list_server <- function(id, events, language, site) {
  
  stopifnot(is.reactive(events))
  stopifnot(is.reactive(language))
  stopifnot(is.reactive(site))
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
 
    # what is the currently selected event in the event list
    current_event <- reactiveVal()
    # is the currently edited event visible in the event list (or has it been
    # filtered out)?
    current_event_visible <- TRUE
    # what were the table view choices (table_block, table_activity, table_year)
    # before we started editing?
    pre_edit_table_view <- NULL
    
    #' Update year choices in event list filter
    #' 
    #' Adds as choices all the years for which events have been recorded
    update_table_year_choices <- function() {
      
      years <- NULL
      
      # find years present in event dates
      for (event_list in events()) {
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
                                                 language())
      
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
      
      if (!isTruthy(site())) { return() }
      
      block_choices <- c("block_choice_all",
                         subset(sites, sites$site == site())$blocks[[1]])
      # the following assumes that no block name is a code name for something
      # else
      names(block_choices) <- get_disp_name(block_choices, language())
      
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
        get_disp_name(choices_for_table_activity, language())
      
      current_choice <- input$table_activity
      if (!isTruthy(current_choice)) {
        current_choice <- "activity_choice_all"
      }
      
      updateSelectInput(session, "table_activity", selected = current_choice,
                        choices = choices_for_table_activity)
    }
    
    # update year choices when events change
    observeEvent(events(), {
      update_table_year_choices()
    })
    
    # update table activity choices when the language changes
    observeEvent(language(), {
      update_table_activity_choices()
    })
    
    # update block choices when site changes
    observeEvent(site(), {
      if (!isTruthy(site())) {
        shinyjs::disable("table_block")
        return()
      } 
      
      shinyjs::enable("table_block")
      update_table_block_choices()
    })
    
    observeEvent(current_event(), ignoreNULL = FALSE, ignoreInit = TRUE, {
      if (dp()) message("selected_event() changed")
      
      if (is.null(current_event())) {
        # edit mode was disabled

        # clear event list selection
        if (!is.null(input$table_rows_selected)) {
          DT::selectRows(proxy = DT::dataTableProxy("table"), selected = NULL)
        }
        
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
      
      # save table view (to be restored when editing is over) if no settings
      # have been saved previously
      if (is.null(pre_edit_table_view)) {
        pre_edit_table_view <<- list(activity = input$table_activity,
                                     block = input$table_block,
                                     year = input$table_year)
      }
      
      # change view of the front page table by changing table_activity selector
      updateSelectInput(session, "table_activity", 
                        selected = current_event()$mgmt_operations_event)
    })
    
    # data to display in the table
    table_data <- reactive({
      
      if (dp()) message("event_list table_data reactive running")
      
      # if any of the filters does not have a value, return an empty table
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
        for (block_data in events()) {
          event_list <- c(event_list, block_data)
        }
      } else {
        event_list <- events()[[input$table_block]]
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
    })
    
    # enable editing of old entries
    observeEvent(input$table_rows_selected, ignoreNULL = FALSE,
                 ignoreInit = TRUE, {
                   
                   if (dp()) message("Change of row selection in event list")
                   
                   row_index <- input$table_rows_selected
                   
                   if (is.null(row_index)) {
                     # if it was the user de-selecting the event, exit edit mode
                     # (the other alternative is that the currently edited event
                     # is not visible in the table and therefore no element can
                     # be selected)
                     if (current_event_visible) {
                       current_event(NULL)
                     }
                     return()
                   }
                   
                   # fetch the event data of the selected row
                   selected_event_data <- table_data()[[row_index, "event"]]
                   
                   # set edit mode on. This saves the event we want to edit so
                   # that it is preserved even if front page table view is
                   # changed
                   current_event(selected_event_data)
                   
                 })
    
    
    # render table when language or table data changes
    output$table <- DT::renderDataTable(server = FALSE, {
      
      if (dp()) message("Rendering event list")
      
      new_data_to_display <- replace_with_display_names(
        table_data(), language())
      n_cols <- ncol(new_data_to_display)
      
      # select the row which we are currently editing
      row_number <- NULL
      if (!is.null(isolate(current_event()))) {
        row_number <- find_event_index(isolate(current_event()), 
                                       new_data_to_display$event)
        # if we couldn't find the currently edited event in the table,
        # prevent clearing the event
        current_event_visible <<- !is.null(row_number)
      }
      
      DT::datatable(new_data_to_display, 
                    # allow selection of a single row
                    selection = list(mode = "single", 
                                     selected = row_number),
                    rownames = FALSE, # hide row numbers
                    class = "table table-hover",
                    #autoHideNavigation = TRUE, doesn't work properly with dom
                    colnames = get_disp_name(names(new_data_to_display),
                                             language = language(),
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
                                       "table_empty_label", language()),
                                     paginate = list(
                                       "next" = get_disp_name(
                                         "table_next_label", language()), 
                                       previous = get_disp_name(
                                         "table_previous_label", language()))
                                   )
                    ))
    })
    
    # update each of the text outputs automatically, including language changes
    # and the dynamic updating in editing table title etc. 
    lapply(text_output_code_names, FUN = function(text_output_code_name) {
      
      # render text
      output[[text_output_code_name]] <- renderText({
        
        if (dp()) message(glue("Rendering text for {text_output_code_name}"))
        
        text_to_show <- get_disp_name(text_output_code_name, language())
        
        # get element from the UI structure lookup list
        element <- structure_lookup_list[[text_output_code_name]]
        # if the text should be updated dynamically, do that
        if (!is.null(element$dynamic)) {
          
          # there are currently two modes of dynamic text
          if (element$dynamic$mode == "input") {
            # the -1 removes the mode element, we don't want it
            patterns <- names(element$dynamic)[-1]
            # use lapply here to get the dependency on input correctly
            replacements <- lapply(patterns, function(pattern) {
              replacement <- input[[ element$dynamic[[pattern]] ]]
              replacement <- get_disp_name(replacement,
                                           language())
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
            text_to_show <- get_disp_name(text_to_show, language())
            
          }
        }
        text_to_show
      })
      
    })
    
    ############## TO RETURN
    
    list(
      current = current_event,
      filters = reactive(list(
        activity = input$table_activity,
        block = input$table_block,
        year = input$table_year))
    )
    
  })
}
