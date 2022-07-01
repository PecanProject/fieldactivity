# The function of the form modoule is as follows:
# - contains the widgets for entering the actual information about the event
# - shows the correct widgets depending on the user's choices
# - allows prefilling the widgets with the desired values
# - verifies that the information has been supplied correctly
# - returns the values to the main app in the format they will be saved (e.g. 
#   "" replaced with missingval)
# - contains the save, cancel and delete buttons and sends their signals to
#   the main app


#' UI function for the form module
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_form_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    # the form contains the widgets for entering information
    # about the event
    fluidRow(
      column(width = 3,
             h3(textOutput(ns("form_title")), 
                style = "margin-bottom = 0px; margin-top = 0px; 
                   margin-block-start = 0px"),
             
             # in general the choices and labels don't have to be 
             # defined for  selectInputs, as they will be 
             # populated when the language is changed 
             # (which also happens when the app starts)
             
             span(textOutput(ns("required_variables_helptext")), 
                  style = "color:gray"),
             br(),
             
             selectInput(ns("block"), label = get_disp_name("block_label", 
                                                            init_lang),
                         choices = ""),
             
             selectInput(ns("mgmt_operations_event"), 
                         label = get_disp_name("mgmt_operations_event_label", 
                                               init_lang), 
                         choices = get_selectInput_choices(
                           "mgmt_operations_event", init_lang)
                         ),
             
             # setting max disallows inputting future events
             dateInput(
               ns("date"),
               format = "dd/mm/yyyy",
               label = get_disp_name("date_label", init_lang),
               max = Sys.Date(),
               value = Sys.Date(),
               weekstart = 1
             ),
             
             textAreaInput(
               ns("mgmt_event_notes"),
               label = get_disp_name("mgmt_event_notes_label", init_lang),
               placeholder = get_disp_name("mgmt_event_notes_placeholder", 
                                           init_lang),
               resize = "vertical",
               height = "70px"
             )
      ),
      
      column(width = 9, 
             # show a detailed options panel for the different activities
             # activity_options and create_ui is defined in utils_ui.R
             create_ui(activity_options, ns)
      )
    ),
    
    # the buttons for saving, canceling and deleting
    fluidRow(
      column(width = 12,
             actionButton(ns("save"), label = "Save"),
             
             actionButton(ns("cancel"), label = "Cancel"),
             
             shinyjs::hidden(actionButton(ns("delete"), label = "Delete", 
                                          class = "btn-warning"))
      )
    )
    
  )
}
    
#' form Server Functions
#'
#' @param id The id of the corresponding UI element
#' @param site A reactive expression holding the current site name
#' @param set_values Changing the value of this reactive expression sets the
#'   values in the form
#' @param reset_values A reactive expression. If set to TRUE, clears the values
#'   on the form
#' @param edit_mode A reactive expression holding a boolean value which
#'   indicates whether the app is editing an event (TRUE) or creating a purely
#'   new one (FALSE)
#' @param language A reactive expression holding the current UI language
#'
#' @import shinyvalidate
#' @noRd
mod_form_server <- function(id, site, set_values, reset_values, edit_mode, 
                            language, init_signal) {
  
  stopifnot(is.reactive(site))
  stopifnot(is.reactive(set_values))
  stopifnot(is.reactive(reset_values))
  stopifnot(is.reactive(edit_mode))
  stopifnot(is.reactive(language))
  stopifnot(is.reactive(init_signal))
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    if (dp()) message("Initialising form server function")
    
    # add input validators
    # the idea is that each widget has its own validator. This validator is
    # active whenever that widget is in relevant variables. These individual
    # validators are then added as subvalidators to main_iv
    main_iv <- InputValidator$new()
    lapply(get_category_names("variable_name"), FUN = function(variable) {
      
      widget <- structure_lookup_list[[variable]]
      iv <- InputValidator$new()
      added_rules <- FALSE
      
      # add required rule
      if (identical(widget$required, TRUE)) {
        
        if (widget$type == "dateRangeInput") {
          iv$add_rule(variable, sv_required(message = "", 
                                            test = valid_dateRangeInput))
        } else {
          iv$add_rule(variable, sv_required(message = "")) 
        }
        
        added_rules <- TRUE
      }
      
      # add minimum rule
      if (!is.null(widget$min)) {
        iv$add_rule(variable, sv_gte(widget$min, allow_na = TRUE, 
                                     message_fmt = ""))
        added_rules <- TRUE
      }
      
      # add maximum rule
      # using [[ here because $ does partial matching and catches onto
      # maxlength
      if (!is.null(widget[["max"]])) {
        iv$add_rule(variable, sv_lte(widget[["max"]], allow_na = TRUE,
                                     message_fmt = ""))
        added_rules <- TRUE
      }
      
      # add integer rule
      if (identical(widget$step, as.integer(1))) {
        iv$add_rule(variable, sv_integer(message = "", allow_na = TRUE))
        added_rules <- TRUE
      }
      
      # add max length rule
      if (!is.null(widget$maxlength)) {
        iv$add_rule(variable, function(x) {
          if (!isTruthy(x)) return(NULL)
          if (nchar(x) <= widget$maxlength) NULL 
          else glue("Max. {widget$maxlength} characters")
        })
        added_rules <- TRUE
      }
      
      if (added_rules) {
        # the validator is only active when it is in the current list of 
        # relevant, regular widgets
        iv$condition(reactive({ variable %in% relevant_variables()$regular }))
        # add widget validator to main validator
        main_iv$add_validator(iv)
      }
      
    })
    # start showing validation messages
    main_iv$enable()
    
    # go through all fields and set maxLength if requested in ui_structure.json
    # TODO: do with validation instead
    # for (element in structure_lookup_list) {
    #   if (!is.null(element$maxlength)) {
    #     js_message <- "$('##code_name').attr('maxlength', #maxlength)"
    #     js_message <- gsub("#code_name", ns(element$code_name), js_message)
    #     js_message <- gsub("#maxlength", element$maxlength, js_message)
    #     shinyjs::runjs(js_message)
    #   }
    # }
    
    # when site setting is changed, update the block choices on the form
    observeEvent(site(), ignoreNULL = FALSE, {
      
      if (!isTruthy(site())) {
        shinyjs::disable("block")
        shinyjs::disable("save")
        return()
      } 
      
      shinyjs::enable("block")
      shinyjs::enable("save")
      
      # update block choices
      block_choices <- subset(sites, sites$site == site())$blocks[[1]]
      updateSelectInput(session, "block", choices = block_choices)
      
    })
    
    # when set_values is changed, update the values in the form
    observeEvent(set_values(), {
      
      if (dp()) message("Filling the form with values")
      
      values <- set_values()
      
      # populate the input widgets with the values corresponding to the 
      # event, and clear others
      for (variable in get_category_names("variable_name")) {
        
        # get the value corresponding to this variable from the event.
        # might be NULL
        value <- values[[variable]]
        widget <- structure_lookup_list[[variable]]
        
        # determine if this value should be filled in a table
        # for now this is a sufficient condition
        variable_table <- get_variable_table(variable)
        value_in_table <- !is.null(variable_table) & length(value) > 1
        
        if (!(variable %in% names(values)) | value_in_table) {
          # clear widget if the event does not contain a value for it
          # or value should be shown in a table instead
          update_ui_element(session, variable, clear_value = TRUE)
        } else if (widget$type == "fileInput") {
          files[[variable]]$set_path(value)
        } else {
          update_ui_element(session, variable, value = value)
        }
      }
      
      # then go through all the variables in the event and see if any of 
      # them should be displayed in the table. If yes, fill the table.
      # Other tables do not need to be cleared, as they do that by 
      # themselves when they become hidden.
      for (variable in names(values)) {
        variable_table <- get_variable_table(variable)
        
        if (!is.null(variable_table)) {
          tables[[variable_table]]$set_values(values)
          # currently there is only one possible table per event
          break
        }
      }
      
      # change set_values back to NULL so that we can catch the next time its
      # value is changed. This doesn't re-trigger this observeEvent as
      # observeEvent ignores NULL values by default
      set_values(NULL)
    })
    
    # when reset_values is signaled, reset the values of all widgets
    observeEvent(reset_values(), {
      
      if (identical(reset_values(), FALSE)) return()
      
      if (dp()) message("Resetting form values")
      
      reset_input_fields(session, get_category_names("variable_name"))
      
      # clear fileInput fields separately
      for (fileInput_code_name in fileInput_code_names) {
        files[[fileInput_code_name]]$reset_path(TRUE)
      }
      
      # fertilizer_element_table is an exception in that it doesn't clear 
      # itself (it is in "static mode"). Let's clear it by hand:
      # TODO: make this automatic too
      tables[["fertilizer_element_table"]]$set_values(list())
      
      # set value back to FALSE so this can be triggered later as well
      # observeEvent ignores FALSE values by default
      reset_values(FALSE)
    })
    
    # show Delete button depending on edit mode
    observeEvent(edit_mode(), ignoreNULL = FALSE, {
      shinyjs::toggle("delete", condition = edit_mode())
    })
    
    # update each of the text outputs automatically, including language changes
    # and the dynamic updating in editing table title etc. 
    # TODO: refactor
    lapply(text_output_code_names, FUN = function(text_output_code_name) {
      
      # render text
      output[[text_output_code_name]] <- renderText({
        
        if (dp()) message(glue("Rendering text for {text_output_code_name}"))
        
        text_to_show <- get_disp_name(text_output_code_name, language())
        
        #get element from the UI structure lookup list
        element <- structure_lookup_list[[text_output_code_name]]
        #if the text should be updated dynamically, do that
        if (!is.null(element$dynamic)) {
          
          # there are currently two modes of dynamic text
          if (element$dynamic$mode == "input") {
            # # the -1 removes the mode element, we don't want it
            # patterns <- names(element$dynamic)[-1]
            # # use lapply here to get the dependency on input correctly
            # replacements <- lapply(patterns, function(pattern) {
            #   replacement <- input[[ element$dynamic[[pattern]] ]]
            #   replacement <- get_disp_name(replacement,
            #                                language())
            #   text_to_show <<- gsub(pattern, replacement, 
            #                         text_to_show)
            #   replacement
            # })
            # 
            # # if one of the replacements is empty, we don't want to
            # # see the text at all
            # if ("" %in% replacements) { text_to_show <- "" }
            
          } else if (element$dynamic$mode == "edit_mode") {
            
            text_to_show <- if (edit_mode()) {
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
    
    observeEvent(language(), ignoreInit = TRUE, {
      
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
        
        label <- get_disp_name(element$label, language())
        
        if (element$type == "selectInput") {
          
          # fetch choices for the selectInput
          choices <- get_selectInput_choices(code_name, language())
          
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
                                  language()))
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
                              language()))
        } else if (element$type == "numericInput") {
          updateNumericInput(session,
                             code_name,
                             label = label)
        } else if (element$type == "dateRangeInput") {
          updateDateRangeInput(session,
                               code_name,
                               label = label)
        } else if (element$type == "fileInput") {
          # fileInput modules do their own language changing
        }
        
      }
      
      
    })
    
    # initialise the tables list (wihout yet starting the table servers) so 
    # that we can pre-supply the values to the tables
    # sapply with simplify = FALSE is equivalent to lapply
    tables <- sapply(data_table_code_names, USE.NAMES = TRUE, simplify = FALSE,
                     FUN = function(table_code_name) {
                       set_values <- reactiveVal()
                       list(set_values = set_values)
                     })
    
    # do the same thing with fileInput modules
    files <- sapply(fileInput_code_names, USE.NAMES = TRUE, simplify = FALSE, 
                    FUN = function(fileInput_code_name) {
                      
                      set_path <- reactiveVal()
                      reset_path <- reactiveVal()
                      
                      list(set_path = set_path,
                           reset_path = reset_path)
                    })
    
    # when we get the init_signal, initialise table and fileInput module server
    # functions. This "staged" initialisation is done to improve startup speed.
    observeEvent(init_signal(), {
      
      if (dp()) message("Initialising table and fileInput server functions")
      
      # initialise the table server for each of the dynamically added tables
      # the values from the table can be accessed ilke
      # tables[[table_code_name]]$result$values()
      sapply(data_table_code_names, FUN = function(table_code_name) {
        
        table_structure <- 
          structure_lookup_list[[table_code_name]]
        
        # are we in static mode, i.e. are all row groups of
        # type 'static'? If yes, we won't need to supply the
        # row_variable_value reactive. Currently this only
        # happens with fertilizer_element_table (the columns
        # are not defined)
        static_mode <- is.null(table_structure$columns)
        
        # find the row variable. This will be used in the
        # reactive below
        if (!static_mode) {
          for (row_group in table_structure$rows) {
            # there is only one dynamic row group
            if (row_group$type == 'dynamic') {
              row_variable <- row_group$row_variable
              row_variable_type <- 
                structure_lookup_list[[row_variable]]$type
              break
            }
          }
        }
        
        # If we have row groups which depend on widget values
        # in the main app, create a reactive from those values.
        # This can either be determined by the choices of
        # selectInput with multiple selections, or a
        # numericInput which represents the number of rows.
        row_variable_value <- reactive({
          
          if (static_mode) {
            return(NULL)
          }
          
          if (row_variable_type == "numericInput") {
            number_of_rows <- input[[row_variable]]
            
            # check status of validator. If it is NULL all is
            # ok
            if (!isTruthy(number_of_rows) || 
                !is.null(isolate(
                  main_iv$validate()[[ns(row_variable)]]))) {
              NULL
            } else {
              as.integer(number_of_rows)
            }
            
          } else if (row_variable_type == "selectInput") {
            input[[row_variable]]
          }
          
        })
        
        # start the server function
        tables[[table_code_name]]$result <<- 
          mod_table_server(table_code_name, 
                           row_variable_value, 
                           language,
                           tables[[table_code_name]]$set_values)
      })
      
      # start server for all fileInput modules
      sapply(fileInput_code_names, FUN = function(fileInput_code_name) {
        
        files[[fileInput_code_name]]$value <<-  
          mod_fileInput_server(id = fileInput_code_name, 
                               language = language,
                               set_path = files[[fileInput_code_name]]$set_path,
                               reset_path = files[[fileInput_code_name]]$reset_path)
      })
      
    })
    
    # when requested, prepare the entered data
    form_data <- reactive({
      
      if (dp()) message("Calculating form data")
      
      relevant <- relevant_variables()
      relevant_table <- if (identical(relevant$table_code_name, character(0))) {
        NULL
      } else {
        tables[[relevant$table_code_name]]$result
      }
      
      # check that the form and table validation rules have been met
      if (!main_iv$is_valid() || 
          (!is.null(relevant_table) && !relevant_table$valid())) {
        return(NULL)
      }
      
      event <- list()
      # fill information
      for (variable in c(relevant$regular, relevant$table)) {

        # is the variable a fileInput?
        is_fileInput <- identical(structure_lookup_list[[variable]]$type, 
                                  "fileInput")
        
        # read value from table if it is available there, otherwise from either
        # a fileInput module or a regular input widget
        value_to_save <- if (variable %in% relevant$table) {
          relevant_table$values()[[variable]]
        } else if (is_fileInput) {
          files[[variable]]$value()
        } else {
          input[[variable]]
        }
        
        # if value is character, trim any whitespace around it
        if (is.character(value_to_save)) {
          value_to_save <- trimws(value_to_save)
        }
        
        # format Date value to character string and replace with "" if that
        # fails for some reason
        if (inherits(value_to_save, "Date")) {
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
        
        # if the value is not defined or empty, replace with missingval
        if (length(value_to_save) == 0) {
          value_to_save <- missingval
        } else {
          missing_indexes <- is.na(value_to_save) | value_to_save == ""
          if (any(missing_indexes)) {
            value_to_save[missing_indexes] <- missingval
          }
        }
        
        event[[variable]] <- value_to_save
      }
      
      # return event data
      event
    })
    
    # When requested, calculate a vector with the names of relevant variables. A
    # variable is relevant when it is visible in some form, either as a regular
    # widget or in a table module. Relevant variables are ones which we want to
    # save to a json file given the current choices of the user. For example,
    # when the user is making a soil observation event, the variables related
    # to that are relevant while other observation variables are not.
    # This only works when the form is already open.
    relevant_variables <- reactive({
      
      if (dp()) message("Calculating relevant variables")
      
      # these are the variables among which the relevant variables are
      all_variables <- get_category_names("variable_name")
      
      # find all widgets that are currently hidden in the UI. This includes
      # regular widgets but also e.g. tables. Note that not all of these are
      # irrelevant; the regular form of a widget might be hidden because we want
      # to read its values from a table instead. This vector includes also
      # tables because that way we can check whether a table is actually hidden.
      hidden_widgets <- unlist(rlapply(activity_options, fun = function(x) {
        if (!is.null(x$condition)) {
          relevant <- evaluate_condition(x$condition, session)
          if (!is.null(relevant) && !identical(relevant, TRUE)) {
            rlapply(x, fun = function(x) x$code_name)
          }
        }
      }))
      
      # find variables which are relevant and being entered through a table
      table_variables <- NULL
      for (table_code_name in data_table_code_names) {
        if (!(table_code_name %in% hidden_widgets)) {
          table_variables <- c(table_variables, 
                               get_table_variables(table_code_name))
          # currently only one table is visible at a time
          break
        }
      }
      
      # Report relevant widgets for regular and table widgets separately.
      # We get the relevant variables by removing from all variables the widgets
      # that are hidden.
      # Also report the name of the table which is currently relevant. 
      list(
        regular = setdiff(all_variables, hidden_widgets),
        table = table_variables,
        table_code_name = setdiff(data_table_code_names, hidden_widgets)
      )
    })
    
    # When requested, list as a vector the variables among the currently 
    # relevant variables which are compulsory to be filled out.
    required_variables <- reactive({
      
      if (dp()) message("Calculating required variables")
      
      required_widgets <- NULL
      required_table_widgets <- NULL
      
      relevant <- relevant_variables()
      
      for (variable in relevant$regular) {
        if (identical(structure_lookup_list[[variable]]$required, TRUE)) {
          required_widgets <- c(required_widgets, variable)
        }
      }
      for (variable in relevant$table) {
        if (identical(structure_lookup_list[[variable]]$required, TRUE)) {
          required_table_widgets <- c(required_table_widgets, variable)
        }
      }
      
      list(
        regular = required_widgets, 
        table = required_table_widgets
      )
      
    })
  
    ################## RETURN VALUE
    
    list(
      data = form_data,
      save = reactive(input$save),
      cancel = reactive(input$cancel),
      delete = reactive(input$delete)
    )
    
  })
  
}
