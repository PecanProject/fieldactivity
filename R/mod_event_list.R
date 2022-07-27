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
        selectInput(ns("event_list_activity_filter"), label = "", 
                    choices = get_disp_name(
                      c("activity_choice_all",
                        get_category_names("mgmt_operations_event_choice")),
                      init_lang, as_names = TRUE), 
                    width = "150px")),
    div(style="display: inline-block;vertical-align:middle;",
        textOutput(ns("table_filter_text_2"), inline = TRUE)),
    div(style="display: inline-block;vertical-align:middle;", 
        selectInput(ns("event_list_block_filter"), label = "", 
                    choices = get_disp_name("block_choice_all", init_lang,
                                            as_names = TRUE),
                    width = "100px")),
    div(style="display: inline-block;vertical-align:middle;",
        textOutput(ns("table_filter_text_3"), inline = TRUE)),
    div(style="display: inline-block;vertical-align:middle;", 
        selectInput(ns("event_list_year_filter"), label = "", 
                    choices = get_disp_name("year_choice_all", init_lang, 
                                            as_names = TRUE),
                    width = "100px")),
    div(style="display: inline-block;vertical-align:middle;", "."),
    
    div(style="display: inline-block;vertical-align:0.2em;position:absolute;right:15em;",
        mod_download_json("json_zip", label = textOutput("json_dl_label"))),
    
    div(style="display: inline-block;vertical-align:0.2em;position:absolute;right:5em;",
        mod_download_table("event_table", label = textOutput("csv_dl_label"))),
    
    # front page data table
    DT::dataTableOutput(ns("table"))
  )
}

#' event_list Server Functions
#'
#' @param id The id of the corresponding UI element
#' @param events A reactive expression holding a list of events to display in the event list
#' @param language A reactive expression holding the current UI language
#' @param site A reactive expression holding the current site name
#'

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
    # what were the table view choices (event_list_block_filter, 
    # event_list_activity_filter, event_list_year_filter)
    # before we started editing?
    pre_edit_table_view <- NULL
    
    #' Update year choices in event list filter
    #' 
    #' Adds as choices all the years for which events have been recorded
    update_event_list_year_filter_choices <- function() {
      
      if (dp()) message("Updating event_list_year_filter choices")
      
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
      choices <- get_disp_name(c("year_choice_all", years),
                               language(), as_names = TRUE)
      
      # retain current selection if possible
      current_choice <- input$event_list_year_filter
      if (!isTruthy(current_choice) || !(current_choice %in% years)) {
        current_choice <- "year_choice_all"
      }
      
      updateSelectInput(session, "event_list_year_filter", selected = current_choice,
                        choices = choices)
      
    }
    
    #' Update block choices in event list filter
    #' 
    #' Add all blocks of the current site as choices
    update_event_list_block_filter_choices <- function() {
      
      if (!isTruthy(site())) { return() }
      
      if (dp()) message("Updating event_list_block_filter choices")
      
      choices <- c("block_choice_all",
                   subset(sites, sites$site == site())$blocks[[1]])
      # the following assumes that no block name is a code name for something
      # else
      choices <- get_disp_name(choices, language(), as_names = TRUE)
      
      current_choice <- input$event_list_block_filter
      if (!isTruthy(current_choice) || !(current_choice %in% choices)) {
        current_choice <- "block_choice_all"
      }
      
      updateSelectInput(session, "event_list_block_filter", 
                        selected = current_choice,
                        choices = choices)
    }
    
    #' Update activity choices in event list filter
    #' 
    #' Only used when the language is changed.
    update_event_list_activity_filter_choices <- function() {
      
      if (dp()) message("Updating event_list_activity_filter choices")
      
      choices <- c("activity_choice_all",
                   get_category_names("mgmt_operations_event_choice"))
      choices <- get_disp_name(choices, language(), as_names = TRUE)
      
      current_choice <- input$event_list_activity_filter
      if (!isTruthy(current_choice)) {
        current_choice <- "activity_choice_all"
      }
      
      updateSelectInput(session, "event_list_activity_filter", 
                        selected = current_choice,
                        choices = choices)
    }
    
    # update year choices when events change
    observeEvent(events(), ignoreInit = TRUE, {
      update_event_list_year_filter_choices()
    })
    
    # update table activity, block and year choices when the language changes
    observeEvent(language(), ignoreInit = TRUE, {
      update_event_list_activity_filter_choices()
      update_event_list_block_filter_choices()
      update_event_list_year_filter_choices()
    })
    
    # update block choices when site changes
    observeEvent(site(), {
      if (!isTruthy(site())) {
        shinyjs::disable("event_list_block_filter")
        return()
      } 
      
      shinyjs::enable("event_list_block_filter")
      update_event_list_block_filter_choices()
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
        updateSelectInput(session, "event_list_activity_filter", 
                          selected = pre_edit_table_view$activity)
        updateSelectInput(session, "event_list_block_filter", 
                          selected = pre_edit_table_view$block)
        updateSelectInput(session, "event_list_year_filter", 
                          selected = pre_edit_table_view$year)
        # clear table view settings
        pre_edit_table_view <<- NULL
        
        return()
      } 
      
      ### edit mode was enabled, or there was a switch from one event to  another
      
      # save table view (to be restored when editing is over) if no settings
      # have been saved previously
      if (is.null(pre_edit_table_view)) {
        pre_edit_table_view <<- list(activity = input$event_list_activity_filter,
                                     block = input$event_list_block_filter,
                                     year = input$event_list_year_filter)
      }
      
      # change view of the front page table by changing the
      # event_list_activity_filter selector
      updateSelectInput(session, "event_list_activity_filter", 
                        selected = current_event()$mgmt_operations_event)
    })
    
    # data to display in the table
    table_data <- reactive({
      
      # if any of the filters does not have a value, return an empty table
      if (!(isTruthy(input$event_list_activity_filter) & 
            isTruthy(input$event_list_block_filter) &
            isTruthy(input$event_list_year_filter))) {
        default_variables <- c("block", "mgmt_operations_event",
                               "date", "mgmt_event_notes")
        return(get_data_table(list(), default_variables))
      }
      
      if (dp()) message("event list table_data reactive running")
      
      # determine the columns displayed in the table
      table_variables <- c("date", "mgmt_event_notes")
      if (input$event_list_activity_filter == "activity_choice_all") {
        table_variables <- c("mgmt_operations_event", table_variables)
      }
      if (input$event_list_block_filter == "block_choice_all") {
        table_variables <- c("block", table_variables)
      }
      
      # if we are only looking at a specific event type, show columns
      # appropriate to it
      if (input$event_list_activity_filter != "activity_choice_all") {
        hidden_widget_types <- c("textOutput", "dataTable", "fileInput", 
                                 "actionButton")
        activity_variables <- unlist(rlapply(
          activity_options[[input$event_list_activity_filter]],
          fun = function(x) {
            if (is.null(x$type) || x$type %in% hidden_widget_types ||
                identical(x$hide_in_event_list, TRUE)) {
              NULL
            } else {
              x$code_name
            }
          }))
        table_variables <- c(table_variables, activity_variables)
      }
      
      # create an event list filtered by user choices
      # filter by block
      if (input$event_list_block_filter == "block_choice_all") {
        event_list <- list()
        for (block_data in events()) {
          event_list <- c(event_list, block_data)
        }
      } else {
        event_list <- events()[[input$event_list_block_filter]]
      }
      
      # filter by activity type
      if (input$event_list_activity_filter != "activity_choice_all") {
        event_list <- rlapply(event_list, fun = function(x)
          if (x$mgmt_operations_event == input$event_list_activity_filter) {x})
      }
      
      # filter by year
      if (input$event_list_year_filter != "year_choice_all") {
        event_list <- rlapply(event_list, fun = function(x) {
          event_year <- format(as.Date(x$date, date_format_json), "%Y")
          if (event_year == input$event_list_year_filter) {x}
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
        
        get_disp_name(text_output_code_name, language())
      })
      
    })
    
    ############## TO RETURN
    
    list(
      current = current_event,
      filters = reactive(list(
        activity = input$event_list_activity_filter,
        block = input$event_list_block_filter,
        year = input$event_list_year_filter))
    )
    
  })
}
