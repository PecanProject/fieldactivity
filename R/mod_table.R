# Table module
# Otto Kuusela 2021
#
# Word of warning: this is (unfortunately) a fickle beast. The main problem
# underlying all difficulties related to this module is binding / unbinding the
# widgets presented in the table. Each time the table is changed (rows are added
# / removed or language changes) the previous inputs must be unbound before the
# table disappears and the new inputs appear. These new inputs must then be
# bound after they have been rendered. This sounds simple, but has caused me
# endless trouble. So tread carefully here, things break easily!

# Print messages to console
table_log <- FALSE

# javascript callback scripts must be wrapped inside a function.
# EDIT: this makes sense also, see datatables API documentation for example
js_bind_script <- "function() { Shiny.bindAll(this.api().table().node()); }"

#' Shiny module for data input in table format
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("table")), 
    br()
  )
}
    
#' table Server Functions
#'
#' @param id The id of the corresponding UI element
#' @param row_variable_value A reactive expression holding the value of the
#'   variable which determines the rows in a dynamic row group. If there are no
#'   dynamic row groups in the table, a reactive expression holding the value
#'   NULL.
#' @param language A reactive expression holding the current UI language
#' @param override_values Changing the value of this reactive expression sets
#'   the values in the table
#'
#' @import shinyvalidate
#' @noRd
mod_table_server <- function(id, row_variable_value, 
                             language, override_values) {
  
  stopifnot(is.reactive(row_variable_value))
  stopifnot(is.reactive(language))
  stopifnot(is.reactive(override_values))
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # This is used to slow things down when the value changes rapidly.
    # throttle lets the first invalidation through but holds the subsequent
    # ones for 800ms
    row_variable_value <- throttle(row_variable_value, millis = 800)

    #### Validate inputs in the table
    
    iv <- InputValidator$new()
    # start showing validation messages
    iv$enable()
    # which widgets have we already added rules for?
    rules_added <- NULL
    # Add validation rules for the specified set of widgets. widgets and
    # variables should have the same length; the latter has "pure" variable
    # names which correspond to the former row-numbered widget names.
    # This function will be called when generating the widgets.
    add_validation_rules <- function(widgets, variables) {
      lapply(1:length(widgets), FUN = function(i) {
        
        widget_name <- widgets[i]
        widget <- structure_lookup_list[[variables[i]]]
        
        if (widget_name %in% rules_added) return()
        
        # add required rule
        if (identical(widget$required, TRUE)) {
          iv$add_rule(widget_name, sv_required(message = "")) 
        }
        
        # add minimum rule
        if (!is.null(widget$min)) {
          iv$add_rule(widget_name, sv_gte(widget$min, allow_na = TRUE, 
                                       message_fmt = ""))
        }
        
        # add maximum rule
        # using [[ here because $ does partial matching and catches onto
        # maxlength
        if (!is.null(widget[["max"]])) {
          iv$add_rule(widget_name, sv_lte(widget[["max"]], allow_na = TRUE,
                                       message_fmt = ""))
        }

      })
      rules_added <- c(rules_added, widgets)
    }
    
    #### Find out useful information about the table
    
    table_structure <- structure_lookup_list[[id]]
    row_groups <- table_structure$rows
    # can be NULL, in which case all row groups are of type 'static'
    column_names <- table_structure$columns 
    static_mode <- is.null(column_names)
    
    if (!static_mode) {
      # find the row variable
      for (row_group in row_groups) {
        if (row_group$type == 'dynamic') {
          # row_variable will be available outside this loop
          row_variable <- row_group$row_variable
          # there is only one dynamic row group
          break
        }
      }
    }
    
    n_cols <- if (static_mode) {
      max(sapply(row_groups, FUN = function(x) length(x$variables)))
    } else {
      length(column_names)
    }
    
    # entered values before we e.g. add rows. This allows us to get the
    # values back when generating a new table
    old_values <- reactiveVal()
    # current values of the table
    table_values <- reactiveVal()
    
    # whether the table is currently rendered or not
    rendered <- reactiveVal(FALSE)
    
    # when the client sends a message that rendering is done, set rendered
    # to TRUE
    observeEvent(input$rendered, {
      rendered(TRUE)
      if (table_log) message(glue("input$rendered is {input$rendered}, ",
                                  "rendered set to TRUE ({id})"))
    })
    
    # when we go hidden, set rendered to FALSE and clear old values
    observeEvent(visible(), ignoreNULL = FALSE, ignoreInit = TRUE,
                 priority = 1, {
      if (!visible()) {
        if (table_log) message(glue("Rendered set to FALSE. ",
                                    "Clearing old values ({id})"))
        rendered(FALSE)
        old_values(list())
      }
    })
    
    # Determine when the table is visible
    visible <- reactive({
      if (static_mode) {
        # tables in static mode are always "visible"
        TRUE
      } else {
        length(dynamic_rows()) > 1 
      }
    })
    
    
    # this triggers the update of table_data when override_values are
    # supplied and not when they are reset to NULL. The triggering
    # behaviour is controller in the observeEvent below
    override_trigger <- reactiveVal(0)
    
    # this is a trigger for updating the table widgets when rows change
    row_trigger <- reactiveVal(0)
    
    # observeEvent ignores NULL values by default
    observeEvent(override_values(), {
      if (table_log) {
        message(glue("Triggering value pre-filling, ",
                     "values are ({id})"))
        utils::str(override_values())
      }
      # update dynamic rows (if any)
      if (!static_mode) {
        dynamic_rows(
          get_dynamic_rows_from_value(row_variable, 
                                      override_values()[[row_variable]])
        )
      }
      override_trigger(override_trigger() + 1)
    })
    
    # this allows blocking extra updates caused by the widget in the main
    # app changing its value after override_values have just been supplied.
    observeEvent(row_variable_value(), ignoreNULL = FALSE, ignoreInit = TRUE, {
      # there is no row variable in static mode
      if (static_mode) return()

      new_dynamic_rows <- get_dynamic_rows_from_value(row_variable,
                                                      row_variable_value())
      
      # as.character is needed because sometimes rows are numeric and that
      # makes comparison simpler
      if (!identical(as.character(new_dynamic_rows), 
                     as.character(dynamic_rows()))) {
        if (table_log) message(glue("Triggering the row_trigger because new ",
                              "rows are ",
                              "{paste(new_dynamic_rows, collapse = ', ')} ",
                              "and old ones are ",
                              "{paste(dynamic_rows(), collapse = ', ')} ",
                              "({id})"))
        dynamic_rows(new_dynamic_rows)
        row_trigger(row_trigger() + 1)
      } else {
        if (table_log) message(glue("Rows are identical so didn't ",
                                    "trigger an update ({id})"))
      }
    })
    
    if (!static_mode) {
      # holds the current rows in the dynamic row group as a vector
      dynamic_rows <- reactiveVal()
    }
    
    # this unbinds the table elements before they are re-rendered.
    # Setting a higher priority ensures this runs before the table render
    observe(priority = 2, {
      # when to run observer
      language() # table is re-generated when language changes
      visible() # required, because this updates before row_trigger
      row_trigger()
      override_trigger()
      # require this so that we know the table has already rendered and
      # is still visible.
      # requiring rendered adds a layer of distance from visible:
      # when visibility first goes to FALSE, rendered is still TRUE
      # here because this observer has a higher priority than the one
      # which sets rendered to FALSE.
      req(isolate(rendered()))
      session$sendCustomMessage("unbind-table", ns("table"))
      if (table_log) message(glue("Sent unbind message ({id})"))
    })
    
    # this is a flag which, when set to TRUE, blocks the calculation of 
    # sums on the total row (present on harvest_crop_table). 
    # This is used to prevent the calculation when the widgets are first created
    # and get their initial values in the table_data reactive.
    block_sum_calculation <- reactiveVal(FALSE)
    
    calculate_sum <- function(total_variable) {
      if (!static_mode) {
        row_value <- isolate(dynamic_rows())
        row_numbers <- if (is.numeric(row_value)) {
          row_value
        } else {
          1:length(row_value)
        }
      }
      
      variable_to_sum <- structure_lookup_list[[total_variable]]$sum_of
      values <- NULL
      for (row_number in row_numbers) {
        element_name <- paste(variable_to_sum, row_number, sep = "_")
        values <- c(values, isolate(input[[element_name]]))
      }
      
      value <- sum(values[!is.na(values)])
      update_ui_element(session, total_variable, value = value)
    }
    
    # calculates the widgets that should be in the table
    table_data <- reactive({
      # when the widgets should be re-calculated.
      # table_data also re-calculates when language changes if table has 
      # selectInputs, text fields with placeholders or widgets 
      # with labels (see below)
      override_trigger()
      row_trigger()
      
      if (table_log) message(glue("Table calculation begins ({id})"))
      
      override_vals <- isolate(override_values())
      do_override <- !is.null(override_vals)
      
      table_to_display <- data.frame(matrix(nrow = 0, ncol = n_cols))
      # column_names can be NULL
      names(table_to_display) <- if (is.null(column_names)) {
        rep("", n_cols)
      } else {
        column_names
      }
      
      if (do_override) {
        # if we just want to clear the table, let's do that
        if (identical(override_vals, list())) {
          if (table_log) message(glue("Clearing table ({id})"))
          override_values(NULL)
          do_override <- FALSE
          old_values(list())
        }
      }
      
      # get all the column numbers with numericInputs so we can adjust
      # the widths for these columns
      #numericInput_columns <- NULL
      current_row <- 1
      for (row_group in row_groups) {
        
        if (row_group$type == 'static') {
          
          current_col <- 1
          for (variable in row_group$variables) {
            
            element <- structure_lookup_list[[variable]]
            
            # if (element$type == "numericInput") {
            #   numericInput_columns <- 
            #     c(numericInput_columns,
            #       current_col)
            # }
            
            code_name <- variable
            
            value <- if (do_override) {
              override_vals[[variable]]
            } else {
              isolate(old_values())[[variable]]
            }
            
            if (!isTruthy(value) || value == missingval) {
              value <- ""
            }
            
            #message(glue("Value for {code_name} is {value}"))
            
            # add choices in the correct language for selectInputs
            choices <- NULL
            if (element$type == "selectInput") {
              choices <- get_selectInput_choices(element, language())
            }
            
            placeholder <- NULL
            if (!is.null(element$placeholder)) {
              placeholder <- get_disp_name(element$placeholder, 
                                           language())
            }
            
            label <- ""
            if (!identical(row_group$hide_labels, TRUE)) {
              label <- get_disp_name(element$label, language())
            }
            
            width <- if (element$type == "numericInput") {
              100
            } else {
              150
            }
            
            # as character makes the element HTML, which can then be
            # not escaped when rendering the table
            widget <- as.character(
              create_widget(element,
                            ns = ns,
                            width = width,
                            override_code_name = code_name,
                            override_label = label,
                            override_value = value,
                            override_choices = choices,
                            override_selected = value,
                            override_placeholder = placeholder
              ))
            
            add_validation_rules(code_name, variable)
            
            
            table_to_display[current_row, current_col] <- widget
            current_col <- current_col + 1
          }
          
          row_name <- ifelse(is.null(row_group$name), current_row, 
                             row_group$name)
          rownames(table_to_display)[current_row] <- row_name
          
          current_row <- current_row + 1
          
        } else if (row_group$type == 'dynamic') {
          
          # the rows in the dynamic row group
          rows <- isolate(dynamic_rows())
          
          for (row in rows) {
            # For dynamic row groups the variables on each row are 
            # determined by the columns of the table.
            # Go through each column and add widgets to the row.
            for (variable in column_names) {
              
              element <- structure_lookup_list[[variable]]
              
              # if (element$type == "numericInput") {
              #   numericInput_columns <- 
              #     c(numericInput_columns,
              #       which(column_names == variable))
              # }
              
              # the code names for these elements are
              # variablename_rownumber
              code_name <- paste(variable, current_row, sep = "_")
              
              # value to show in the widget initially
              value <- if (do_override) {
                override_vals[[variable]][current_row]
              } else {
                # fetch old value to show it after rows have changed
                old_row_number <- which(
                  isolate(old_values())[["DYNAMIC_ROWS"]] ==
                    rows[current_row])
                isolate(old_values())[[variable]][old_row_number]
              }
              
              if (!isTruthy(value) || value == missingval) {
                value <- ""
              }
              
              #message(glue("Value for {code_name} is {value}"))
              
              # add choices in the correct language for selectInputs
              choices <- NULL
              if (element$type == "selectInput") {
                choices <- get_selectInput_choices(element, 
                                                   language())
              }
              
              placeholder <- NULL
              if (!is.null(element$placeholder)) {
                placeholder <- get_disp_name(element$placeholder, 
                                             language())
              }
              
              width <- if (element$type == "numericInput") {
                100
              } else {
                150
              }
              
              # as character makes the element HTML, which can then be
              # not escaped when rendering the table
              widget <- as.character(
                create_widget(element,
                              ns = ns,
                              width = width,
                              override_code_name = code_name,
                              override_label = "",
                              override_value = value,
                              override_choices = choices,
                              override_selected = value,
                              override_placeholder = placeholder
                ))
              
              add_validation_rules(code_name, variable)
              
              table_to_display[current_row, variable] <- widget
            }
            
            rownames(table_to_display)[current_row] <- row
            current_row <- current_row + 1
          }
          
        }
        
      }
      
      # block calculation of sums once when the table becomes visible
      block_sum_calculation(TRUE)
      if (table_log) message(glue("Calculated table, has ",
                            "{nrow(table_to_display)} rows ({id})"))
      # clear override_values
      override_values(NULL)
      table_to_display
    })
    
    output$table <- DT::renderDataTable({
      req(visible())
      if (table_log) message(glue("Starting render, rendered set to FALSE ({id})"))
      
      rendered(FALSE)
      table_to_display <- table_data()
      
      if (nrow(table_to_display) == 0) {
        if (table_log) message(glue("No rows, didn't render ({id})"))
        return()
      }
      
      names(table_to_display) <- get_disp_name(names(table_to_display),
                                               language = language(),
                                               is_variable_name = TRUE)
      rownames(table_to_display) <- get_disp_name(
        rownames(table_to_display),
        language = language())
      
      # numericInput_columns <- table_data()$numericInput_columns
      # if (static_mode) {
      #   numericInput_columns <- numericInput_columns - 1
      # }
      table_to_display <- 
        DT::datatable(
          table_to_display, 
          escape = FALSE, # makes widgets actual widgets, IMPORTANT
          selection = "none",
          class = "table table-hover table-condensed",
          rownames = !static_mode, # show rownames if not static mode
          options = 
            list(dom = "t", # hide everything except table
                 # hide sorting arrows
                 ordering = FALSE,
                 # binds the inputs when drawing is done
                 drawCallback = htmlwidgets::JS(js_bind_script),
                 # calls selectize() on all selectInputs, which
                 # makes them look the way they should. Also asks
                 # the client to send the rendering done message.
                 initComplete = 
                   htmlwidgets::JS(paste0(
                     "function(settings, json) {",
                     "do_selectize('", ns("table"), "'); ",
                     "rendering_done('", ns("rendered"), "'); }"
                   )),
                 scrollX = TRUE
            ))
      # if we are in custom mode, align cells vertically so that the
      # widgets are always in line
      if (static_mode) {
        DT::formatStyle(table_to_display, 0:(n_cols-1), 
                    'vertical-align' = 'bottom')
      } else {
        table_to_display
      }
      
    }, server = FALSE)
    
    
    # return the values of the table widgets
    # has to run when visibility changes because input values are not
    # available otherwise.
    # This observer also calculates the sum on the total row in 
    # harvest_crop_table
    observe({
      
      value_list <- list()

      if (!rendered()) {
        if (table_log) message(glue("Values observe blocked, table not ",
                                    "rendered yet ({id})"))
        table_values(value_list)
        return()
      }
      
      # Add dependency on table_data() at this point. 
      # Needed, so that when new widgets are calculated, we add a
      # dependency on them
      table_data()
      
      if (table_log) message(glue("Values observe running ({id})"))
      
      # fetch values from widgets in static row groups 
      for (row_group in row_groups) {
        if (row_group$type == 'static') {
          for (variable in row_group$variables) {
            value_list[[variable]] <- input[[variable]]
          }
        }
      }
      
      if (!static_mode) {
        row_value <- dynamic_rows()
        row_numbers <- if (is.numeric(row_value)) {
          row_value
        } else {
          1:length(row_value)
        }
      }
      # does not do anything if column names are undefined
      for (variable in column_names) {
        values <- NULL
        
        for (row_number in row_numbers) {
          element_name <- paste(variable, row_number, sep = "_")
          values <- c(values, input[[element_name]])
        }
        
        # handle calculating the total values when new values are entered
        previous_vals <- isolate(table_values())[[variable]]
        total_variable <- structure_lookup_list[[variable]]$sum_to
        if (!is.null(total_variable) &&
            !identical(previous_vals, values) &&
            !isolate(block_sum_calculation())) {
          if (table_log) message(glue("Initiating sum calculation for ",
                                "{total_variable}"))
          calculate_sum(total_variable)
        }
        
        value_list[[variable]] <- values
      }
      
      block_sum_calculation(FALSE)
      table_values(value_list)
      # if there is a dynamic row group, let's add the current rows to old 
      # values so we can access them when rows change
      if (!static_mode) {
        value_list <- c(value_list, list(DYNAMIC_ROWS = isolate(dynamic_rows())))
      }
      old_values(value_list)
      
    })
    
    ################## RETURN VALUE
    
    list(
      values = table_values,
      valid = reactive(iv$is_valid())
    )
    
  })
    
}

