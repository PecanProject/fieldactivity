# Shiny module for data input in table format
# Otto Kuusela 2021

# missing value in the ICASA standard
missingval <- "-99.0"
log <- FALSE

# TODO: maybe a separate old_values() is not necessary? It works now, though,
# so changing it is not a very high priority task.

# javascript callback scripts must be wrapped inside a function.
# EDIT: this makes sense also, see datatables API documentation for example
js_bind_script <- "function() { Shiny.bindAll(this.api().table().node()); }"

# js_selectize_script <- function(ns) {
#     paste(sep = "",
#           "function() { ",
#           "Shiny.onInputChange('", ns("rendered"), "', true);",
#           "return $('#", ns("table"), "').find('select').selectize(); }")
#}
# remember to give the id through the NS function
# js_add_listener <- function(id) {
#     paste(sep = "",
#           "function add_listener() { ",
#           "alert('Adding listener!');",
#           "$('#", id, "').on( 'preDraw.dt', function() {",
#           "alert('Heard preDraw event!');",
#           "Shiny.unbindAll($('#", id, "').find('table').DataTable().table().node());",
#           "})}")
# }

# define the module's UI
tableInput <- function(id) {
    tagList(DT::dataTableOutput(NS(id, "table")), 
            br()) 
}

# row_variable_value should be the value of the variable which determines the
# rows for dynamic row groups. Override values are to be supplied in the same
# format that the table returns, i.e. a list with variable names and the values
# as vectors under those.
tableServer <- function(id, row_variable_value, language, visible, 
                        override_values) {
    
    stopifnot(is.reactive(row_variable_value))
    stopifnot(is.reactive(language))
    stopifnot(is.reactive(visible))
    stopifnot(is.reactive(override_values))
    
    moduleServer(id, function(input, output, session) {
        
        # get corresponding element info to determine which widgets to add to
        # the table
        table_structure <- structure_lookup_list[[id]]
        row_groups <- table_structure$rows
        # can be NULL, in which case all row groups are of type 'static'
        column_names <- table_structure$columns 
        static_mode <- is.null(column_names)
        
        if (!static_mode) {
            # find the row variable
            for (row_group in row_groups) {
                # there is only one dynamic row group
                if (row_group$type == 'dynamic') {
                    # row_variable will be available outside this loop
                    row_variable <- row_group$row_variable
                    break
                }
            }
        }
        
        n_cols <- if (static_mode) {
            max(sapply(row_groups, FUN = function(x) length(x$variables)))
        } else {
            length(column_names)
        }
        
        # if the column variables are not defined, we are in custom mode. This
        # happens with fertilizer_element_table
        # custom_mode <- is.null(table_structure$columns)
        # if (custom_mode) {
        #     # the number of columns is the largest of the rows' lengths
        #     n_cols <- max(sapply(row_variable, length))
        #     n_rows <- length(row_variable)
        #     variables <- unlist(row_variable)
        # } else {
        #     variables <- table_structure$columns
        #     n_cols <- length(variables)
        # }
        
        # entered values before we e.g. add rows. This allows us to get the
        # values back when generating a new table
        old_values <- reactiveVal()
        # current values of the table
        table_values <-reactiveVal()
        
        # this unbinds the table elements before they are re-rendered.
        # Setting a higher priority ensures this runs before the table render
        observe(priority = 2, {
            # when to run observer
            language()
            visible()
            row_trigger()
            override_trigger()
            # require this so that we know the table has already rendered and
            # is still visible
            # requiring rendered adds a layer of distance from visible:
            # when visibility first goes to FALSE, rendered is still TRUE
            # here because this observer has a higher priority than the one
            # which sets rendered to FALSE
            req(isolate(input$table_rows_current), isolate(rendered()))
            session$sendCustomMessage("unbind-table", NS(id, "table"))
            if (log) message(glue("Sent unbind message ({id})"))
        })
        
        # whether the table is currently rendered or not
        rendered <- reactiveVal(FALSE)
        
        # when the client sends a message that rendering is done, set rendered
        # to TRUE
        observeEvent(input$rendered, {
            rendered(TRUE)
            if (log) message(glue("input$rendered is {input$rendered}, ",
                "rendered set to TRUE ({id})"))
        })
        
        # when we go hidden, set rendered to FALSE and clear old values
        observeEvent(visible(), ignoreNULL = FALSE, ignoreInit = TRUE ,
                     priority = 1, {
            if (!visible()) {
                if (log) message(glue("Rendered set to FALSE. ",
                    "Clearing old values ({id})"))
                rendered(FALSE)
                old_values(list())
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
            if (log) {
                message(glue("Triggering value pre-filling, ",
                    "values are ({id})"))
                str(override_values())
            }
            override_trigger(override_trigger() + 1)
        })
        
        # this is a flag which, when set to TRUE, blocks the calculation of 
        # sums on the total row. This is used to prevent the calculation when
        # the widgets are first created and get their initial values in the
        # table_data reactive.
        block_sum_calculation <- reactiveVal(FALSE)
        
        calculate_sum <- function(total_variable) {
            if (!static_mode) {
                row_value <- isolate(table_values())[[row_variable]]
                row_numbers <- if (is.numeric(row_value)) {
                    1:row_value
                } else {
                    1: length(row_value)
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
        
        # this allows blocking extra updates caused by the widget in the main
        # app changing its value after override_values have just been supplied.
        observeEvent(row_variable_value(), ignoreNULL = FALSE, {
            # if (custom_mode) {
            #     if (length(row_variable_value()) == 2) {
            #         if (log) message("Triggering row trigger in custom mode")
            #         row_trigger(row_trigger() + 1)
            #     }
            #     return()
            # }
            
            if (static_mode) return()
            
            current_row_variable_value <- table_values()[[row_variable]]
            # as.character is needed because sometimes row_names() are numeric
            if (!identical(as.character(row_variable_value()), 
                           as.character(current_row_variable_value))) {
                if (log) message(glue("Triggering the row_trigger because new ",
                    "row variable value is ",
                    "{paste(row_variable_value(), collapse = ', ')} and ",
                    "old one is ",
                    "{paste(current_row_variable_value, collapse = ', ')} ",
                    "({id})"))
                row_trigger(row_trigger() + 1)
            } else {
                if (log) message(glue("Row names are identical so didn't ",
                    "trigger an update ({id})"))
            }
        })
        
        # calculates the widgets that should be in the table
        table_data <- reactive({
            # when the widgets should be re-calculated.
            # table_data also re-calculates when language changes if table has 
            # selectInputs, text fields with placeholders or widgets 
            # with labels (see below)
            override_trigger()
            row_trigger()

            if (log) message(glue("Table calculation begins ({id})"))
            
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
                    override_values(NULL)
                    return(table_to_display)
                }
                # check that the variables in override values are correct
                # if (!all(variables %in% names(override_vals))) {
                #     message(glue("The override values supplied to table {id} ",
                #                  "are missing some variables, the table ",
                #                  "will not be rendered"))
                #     override_values(NULL)
                #     return(table_to_display)
                # }
            }

            # get all the column numbers with numericInputs so we can adjust
            # the widths for these columns
            numericInput_columns <- NULL
            current_row <- 1
            for (row_group in row_groups) {
                
                if (row_group$type == 'static') {
                    
                    current_col <- 1
                    for (variable in row_group$variables) {
                        
                        element <- structure_lookup_list[[variable]]
                        
                        if (element$type == "numericInput") {
                            numericInput_columns <- 
                                c(numericInput_columns,
                                  current_col)
                        }
                        
                        code_name <- NS(id, variable)
                        
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
                        if (is.null(row_group$hide_labels)) {
                            label <- get_disp_name(element$label, language())
                        }
                        
                        # as character makes the element HTML, which can then be
                        # not escaped when rendering the table
                        widget <- as.character(
                            create_element(element,
                                           #width = width,
                                           override_code_name = code_name,
                                           override_label = label,
                                           override_value = value,
                                           override_choices = choices,
                                           override_selected = value,
                                           override_placeholder = placeholder
                            ))
                        
                        
                        table_to_display[current_row, current_col] <- widget
                        current_col <- current_col + 1
                    }
                    
                    row_name <- ifelse(is.null(row_group$name), current_row, 
                                       row_group$name)
                    rownames(table_to_display)[current_row] <- row_name
                    
                    current_row <- current_row + 1
                    
                } else if (row_group$type == 'dynamic') {
                    
                    # determine the rows in this dynamic row group
                    row_value <- if (do_override) {
                        override_vals[[row_variable]]
                    } else {
                        isolate(row_variable_value())
                    }
                    row_variable_structure <- 
                        structure_lookup_list[[row_variable]]
                    rows <- if (row_variable_structure$type == "numericInput") {
                        if (!isTruthy(row_value) || row_value == missingval) {
                            NULL
                        } else {
                            1:as.integer(row_value)
                        }
                    } else if (row_variable_structure$type == "selectInput") {
                        row_value
                    }
                    
                    for (row in rows) {
                        # For dynamic row groups the variables on each row are 
                        # determined by the columns of the table.
                        # Go through each column and add widgets to the row.
                        for (variable in column_names) {   
                            
                            element <- structure_lookup_list[[variable]]
                            
                            if (element$type == "numericInput") {
                                numericInput_columns <- 
                                    c(numericInput_columns,
                                      which(column_names == variable))
                            }
                            # width <- if (element$type == "numericInput") {
                            #     "80px"
                            # } else if (element$type == "textInput") {
                            #     "110px"
                            # } else {
                            #     "150px"
                            # }
                            
                            # the code names for these elements are
                            # variablename_rownumber
                            code_name <- paste(variable, current_row, sep = "_")
                            
                            # add observer to sum values if requested
                            # if (!is.null(element$sum_to)) {
                            #     message(glue("Adding observer for {code_name}, {element$sum_to}"))
                            #     lapply(list(list(widget = code_name, 
                            #                 total_variable = element$sum_to)), 
                            #            FUN = function(x) {
                            #         observeEvent(input[[x$widget]], su, {
                            #             message(glue("Observe for {x$widget},",
                            #                 "{x$total_variable}"))
                            #             calculate_sum(x$total_variable)
                            #     })})
                            # }
                            
                            # value to show in the widget initially
                            value <- if (do_override) {
                                override_vals[[variable]][current_row]
                            } else {
                                # fetch old value to show it after rows have changed
                                old_row_number <- which(
                                    isolate(old_values())[[row_variable]] ==
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
                            
                            # as character makes the element HTML, which can then be
                            # not escaped when rendering the table
                            widget <- as.character(
                                create_element(element,
                                               #width = width,
                                               override_code_name = 
                                                   NS(id, code_name),
                                               override_label = "",
                                               override_value = value,
                                               override_choices = choices,
                                               override_selected = value,
                                               override_placeholder = placeholder
                                ))
                            
                            
                            table_to_display[current_row, variable] <- widget
                        }
                        
                        rownames(table_to_display)[current_row] <- row
                        current_row <- current_row + 1
                    }
                    
                }
                
            }
            

            # # if there are no rows, return an empty data frame
            # if (nrow(table_to_display) == 0) {
            #     override_values(NULL)
            #     return(table_to_display)
            # }
        
            
            # add code names to the row names. These will be changed to display
            # names when rendering
            #rownames(table_to_display) <- rows
            
            # if you want to add observers to these widgets here, you need to do
            # it like this using lapply
            # lapply(1:n_rows, FUN = function(row_number) {
            #     lapply(columns, FUN = function(column_name) {
            #         code_name <- paste(column_name, row_number, sep = "_")
            #         observeEvent(input[[code_name]], {
            #             message(input[[code_name]])
            #         })
            #     })
            # })

            # block calculation of sums once when the table becomes visible
            block_sum_calculation(TRUE)
            # clear override_values
            override_values(NULL)
            if (log) message(glue("Calculated table, has ",
                "{nrow(table_to_display)} rows ({id})"))
            list(table = table_to_display, 
                 numericInput_columns = unique(numericInput_columns))
        })
        
        # this is just like the table_data reactive, but used for when the 
        # table is in custom mode
        # custom_table_data <- reactive({
        #     # when the widgets should be re-calculated.
        #     # table_data also re-calculates when language changes if table has 
        #     # selectInputs or text fields with placeholders (see below)
        #     override_trigger()
        #     row_trigger()
        #     
        #     if (log) message(glue("Table calculation begins ({id})"))
        #     
        #     override_vals <- isolate(override_values())
        #     do_override <- !is.null(override_vals)
        #     
        #     table_to_display <- 
        #         data.frame(matrix(nrow = n_rows, ncol = n_cols))
        #     names(table_to_display) <- rep("", n_cols)
        #     
        #     if (do_override) {
        #         # if we just want to clear the table, let's do that
        #         if (identical(override_vals, list())) {
        #             override_values(NULL)
        #             return(table_to_display)
        #         }
        #         # check that the variables in override values are correct
        #         if (!all(variables %in% names(override_vals))) {
        #             message(glue("The override values supplied to table {id} ",
        #                          "are missing some variables, the table ",
        #                          "will not be rendered"))
        #             override_values(NULL)
        #             return(table_to_display)
        #         }
        #     }
        #     
        #     current_row <- 1
        #     for (row in row_variable) {
        #         
        #     }
        #     
        #     # clear override_values
        #     override_values(NULL)
        #     if (log) message(glue("Calculated custom table, has ",
        #                           "{nrow(table_to_display)} rows ({id})"))
        #     table_to_display
        # })
        
        # TODO: this isn't necessary as we can simply add dependence on 
        # language to the table reactive calculation. This might be more
        # efficient though? In that case all language change related stuff
        # should probably be done this way
        #
        # narrow use case: update widget labels to match the correct
        # language. This is only needed in fertilizer_element_table, because
        # that is the only time we have labels on the widgets
        # observeEvent(language(), {
        #     req(rendered(), custom_mode)
        #     #str(reactiveValuesToList(input))
        #     if (log) message(glue("Updating widget languages ({id})"))
        #     for (variable in variables) {
        #         element <- structure_lookup_list[[variable]]
        #         update_ui_element(session, variable, label = 
        #                                get_disp_name(element$label, language()))
        #     }
        # })
        
        output$table <- DT::renderDataTable({
 
            # added language here; does it cause issues?
            # No, but why is it necessary?
            req(visible(), table_data(), language())
            table_to_display <- table_data()$table
            
            if (nrow(table_to_display) == 0) {
                if (log) message(glue("No rows, didn't render ({id})"))
                return()
            } else {
                if (log) message(glue("Rendering table ({id})"))
            }
            
            names(table_to_display) <- get_disp_name(names(table_to_display),
                                                     language = language(),
                                                     is_variable_name = TRUE)
            rownames(table_to_display) <- get_disp_name(
                rownames(table_to_display),
                language = language())
            
            numericInput_columns <- table_data()$numericInput_columns
            if (static_mode) {
                numericInput_columns <- numericInput_columns - 1
            }
            
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
                             drawCallback = JS(js_bind_script),
                             # calls selectize() on all selectInputs, which
                             # makes them look the way they should. Also tell
                             # the client to send the rendering done message.
                             initComplete = 
                             JS(paste0(
                                 "function(settings, json) {",
                                 "do_selectize('", NS(id, "table"), "'); ",
                                 "rendering_done('", NS(id, "rendered"), "'); }"
                             )),
                             scrollX = TRUE,
                             autoWidth = TRUE,
                             columnDefs = list(list(width = '35px', 
                                                    targets = 
                                                        numericInput_columns))
                        ))
            # if we are in custom mode, align cells vertically so that the
            # widgets are always in line
            if (static_mode) {
                formatStyle(table_to_display, 0:(n_cols-1), 
                            'vertical-align' = 'bottom')
            } else {
                table_to_display
            }
            
        }, server = FALSE)

        
        # return the values of the table widgets
        # has to run when visibility changes because input values are not
        # available otherwise
        observe({
            # needed, because when we are given new override values, we want
            # to know the row variable value ASAP so we are ready when the
            # values in the main app widget are changed
            override_trigger() 
            
            value_list <- list()
            did_override <- !is.null(isolate(override_values()))
            if (!static_mode) {
                value_list[[row_variable]] <- 
                    if (did_override) {
                        isolate(override_values()[[row_variable]])
                    } else {
                        isolate(row_variable_value())
                    }
            }
            
            if (!rendered() || 
                (!static_mode && length(value_list[[row_variable]]) == 0)) {
                if (log) message(glue("Values observe blocked ({id})"))
                table_values(value_list)
                return()
            }
            
            # Add dependenvy on table_data() at this point. 
            # Needed, so that when new widgets are calculated, we add a
            # dependency on them
            table_data()
            
            if (log) message(glue("Values observe running ({id})"))
            
            # fetch values from widgets in static row groups 
            for (row_group in row_groups) {
                if (row_group$type == 'static') {
                    for (variable in row_group$variables) {
                        value <- input[[variable]]
                        value_list[[variable]] <- value
                    }
                }
            }
            
            if (!static_mode) {
                row_value <- value_list[[row_variable]]
                row_numbers <- if (is.numeric(row_value)) {
                    1:row_value
                } else {
                    1: length(row_value)
                }
            }
            # does not do anything if column names are undefined
            for (variable in column_names) {
                values <- NULL
                
                for (row_number in row_numbers) {
                    element_name <- paste(variable, row_number, sep = "_")
                    values <- c(values, input[[element_name]])
                }
                
                previous_vals <- isolate(table_values())[[variable]]
                total_variable <- structure_lookup_list[[variable]]$sum_to
                if (!identical(previous_vals, values) && 
                    !is.null(total_variable) &&
                    !isolate(block_sum_calculation())) {
                    if (log) message(glue("Initiating sum calculation for ",
                        "{total_variable}"))
                    calculate_sum(total_variable)
                }
                
                value_list[[variable]] <- values
            }
            
            block_sum_calculation(FALSE)
            old_values(value_list)
            table_values(value_list)
        })

        # return table_values as reactive to the main app
        table_values
    })
}

##########################################
# example application that uses the module

tableApp <- function() {
    ui <- fluidPage(
        
        sidebarLayout(
            
            sidebarPanel(width = 5,
                         selectInput("crop", label = "Choose crops", 
                                     choices = get_category_names("CRID"), 
                                     multiple = TRUE),
                         selectInput("language", label = "Choose language", 
                                     choices = c("disp_name_eng",
                                                 "disp_name_fin")),
                         verbatimTextOutput("debug_text_output"),
                         actionButton("override_values_button", 
                                      label = "Override values")
                         
            ),
            
            mainPanel(width = 7,
                uiOutput("table_ui"),
            )
            
        )
        
    )
    server <- function(input, output, session) {
        condition <- reactiveVal(FALSE)
        observeEvent(input$crop, {
            result <- length(input$crop) > 1
            condition(result)
        })
        
        override_values <- reactiveVal(NULL)
        
        data <- tableServer("harvest_crop_table", reactive(input$crop),
                           reactive(input$language), condition, 
                           override_values = override_values)
        
        output$debug_text_output <- renderPrint({ str(data()) })
        
        output$table_ui <- renderUI({
            if (condition()) {
                if (log) message("UI rendering")
                tableInput("harvest_crop_table")
            }
        })
        
        observeEvent(input$override_values_button, {
            values <- list(harvest_crop = c("FRG", "OAT"),
                           harvest_operat_component = c("canopy", "leaf"),
                           canopy_height_harvest = c(0.4, 0.5),
                           harvest_yield_harvest_dw = c("1", "2"),
                           harv_yield_harv_f_wt = c(3,1),
                           harvest_method = c("HM001", "HM002"),
                           harvest_cut_height = c(0.3, 0.3))
            override_values(values)
            updateSelectInput(session, "crop", selected = c("FRG", "OAT"))
        })
        
        observe({
            str(data())
        })
    }

    shinyApp(ui, server)
}