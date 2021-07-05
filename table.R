# Shiny module for data input in table format
# Otto Kuusela 2021

# missing value in the ICASA standard
missingval <- "-99.0"
log <- FALSE

# TODO: move these to the javascript file
# javascript callback scripts must be wrapped inside a function for some reason
# EDIT: this makes sense also, see datatables API documentation for example
js_bind_script <- "function() { Shiny.bindAll(this.api().table().node()); }"

js_unbind_script <- paste(sep = "",
          "Shiny.addCustomMessageHandler('unbind-table', function(id) {",
          #alert($('#'+id).find('.shiny-input-container').length);
          "Shiny.unbindAll($('#'+id).find('.shiny-input-container'));
          })")

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

tableInput <- function(id) {
    
    tagList(includeScript("www/script.js"),
            tags$head(tags$script(HTML(js_unbind_script))),
            # tags$head(tags$script(HTML(paste(sep = "",
            #     "$(document).ready(function() {",
            #     "('#", NS(id, "table"), "').on('preDraw.dt', function() {",
            #     " Shiny.onInputChange('", NS(id, "rendered"), "', false); });",
            #     "});"
            # )))),
            #tags$head(tags$script(HTML(js_selectize_script(NS(id, "table"))))),
            DT::dataTableOutput(NS(id, "table")), 
            br()) 
}

# override values are to be supplied in the same format that the table returns,
# i.e. a list with variable names and the values as vectors under those.
# They also have to include a row_names component
tableServer <- function(id, row_names, language, visible, 
                        override_values = NULL) {
    
    stopifnot(is.reactive(row_names))
    stopifnot(is.reactive(language))
    stopifnot(is.reactive(visible))
    stopifnot(is.reactive(override_values))
    
    moduleServer(id, function(input, output, session) {
        
        # get corresponding element info to determine which widgets to add to
        # the table
        table_structure <- structure_lookup_list[[id]]
        variables <- table_structure$columns
        row_variable <- table_structure$rows
        n_cols <- length(variables)
        
        # this unbinds the table elements before they are re-rendered.
        # Setting a higher priority ensures this runs before the table render
        observe(priority = 1, {
            # when to run observer
            row_trigger()
            language()
            visible()
            override_trigger()
            # require this so that we know the table has already rendered
            req(isolate(input$table_rows_current), visible())
            #message("Sent unbind message")
            session$sendCustomMessage("unbind-table", NS(id, "table"))
        })
        
        # whether the table is currently rendered or not
        rendered <- reactiveVal(FALSE)
        
        # when the server sends a message that rendering is done, set rendered
        # to TRUE
        observeEvent(input$rendered, {
            rendered(TRUE)
            if (log) message(glue("input$rendered is {input$rendered}, ",
                "rendered set to TRUE ({id})"))
        })
        
        # when we go hidden, set rendered to FALSE
        observeEvent(visible(), ignoreNULL = FALSE, {
            if (!visible()) {
                if (log) message(glue("Rendered set to FALSE. ",
                    "Clearing old values ({id})"))
                rendered(FALSE)
                old_values(list())
            }
        })
        
        #n_rows <- reactive({
            
            # override_trigger()
            # row_names()
            # 
            # if (!is.null(isolate(override_values()))) {
            #     length(isolate(override_values()$row_names))
            # } else {
            #     length(row_names())
            # }
        #})
        
        # this is a trigger which triggers the update of table_data when
        # we want to. We want to trigger when override_values changes to a non-
        # NULL value, but not when we change it back to a NULL. The triggering
        # behaviour is controller in the observeEvent below
        override_trigger <- reactiveVal(0)
        
        # this is a flag which prevents updating the table once after values
        # have been prefilled. This is to prevent the update that is caused
        # by updating the widget corresponding to rows in the main app
        # block_update <- reactiveVal(FALSE)
        
        # this is a trigger for updating the table widgets when rows change
        row_trigger <- reactiveVal(0)
        
        # this ignores NULL values
        observeEvent(override_values(), {
            if (log) {
                message(glue("Triggering value pre-filling, ",
                    "values are ({id})"))
                str(override_values())
            }
            override_trigger(override_trigger() + 1)
        })
        
        # this allows blocking extra updates
        observeEvent(row_names(), ignoreNULL = FALSE, {
            #message("Row_names observer")
            current_row_names <- table_values()[[row_variable]]
            # as.character is needed because sometimes row_names() are numeric
            if (!identical(as.character(row_names()), 
                           as.character(current_row_names))) {
                if (log) message(glue("Triggering the row_trigger because ",
                    "new rows are {paste(row_names(), collapse = ', ')} and ",
                    "old ones are {paste(current_row_names, collapse = ', ')}",
                    "({id})"))
                row_trigger(row_trigger() + 1)
            } else {
                if (log) message(glue("Row names are identical so didn't ",
                    "trigger an update ({id})"))
            }
        })
        
        # clear old data when visibility changes to hidden
        # observeEvent(visible(), {
        #     if (!visible()) {
        #         vals <- list()
        #         vals[[row_variable]] <- table_values()
        #         old_values(table_values)
        #         message("Cleared old values")
        #     }
        # })
        
        table_data <- reactive({
            override_trigger()
            row_trigger()

            if (log ) message(glue("Table calculation begins ({id})"))
            
            override_vals <- isolate(override_values())
            do_override <- !is.null(override_vals)
            
            table_to_display <- data.frame(matrix(nrow = 0,
                                                  ncol = length(variables)))
            names(table_to_display) <- variables
            
            # check that the variables in override values are correct
            if (do_override) {
                if (log) message("Doing override in table calculation")
                # if we just want to clear the table, let's do that
                if (identical(override_vals, list())) {
                    override_values(NULL)
                    return(table_to_display)
                }
                if (!all(variables %in% names(override_vals))) {
                    stop(paste("The override values supplied to table", id, 
                               "are faulty!"))
                }
            }

            rows <- if (do_override) {
                
                # if overriding, determine the appropriate rows
                row_variable_structure <- structure_lookup_list[[row_variable]]
                if (row_variable_structure$type == "numericInput") {
                    
                    number_of_rows <- override_vals[[row_variable]]
                    
                    if (!isTruthy(number_of_rows) ||
                        number_of_rows == missingval) {
                        NULL
                    } else {
                        number_of_rows <- 
                            max(ceiling(as.numeric(number_of_rows)), 1)
                        1:number_of_rows
                    }
                    
                } else if (row_variable_structure$type == "selectInput") {
                    override_vals[[row_variable]]
                }
      
            } else {
                isolate(row_names())
            }
            
            if (length(rows) == 0) {
                override_values(NULL)
                return(table_to_display)
            }
            
            for (variable_name in variables) {
                element <- structure_lookup_list[[variable_name]]
                
                width <- if (element$type == "numericInput") {
                    "80px"
                } else if (element$type == "textInput") {
                    "110px"
                } else {
                    "150px"
                }
                
                for (row_number in 1:length(rows)) {
                    
                    # the code names for these elements are
                    # variablename_rownumber
                    code_name <- NS(id, 
                                    paste(variable_name, row_number, sep = "_"))
                    
                    value <- if (do_override) {
                        override_vals[[variable_name]][row_number]
                    } else {
                        old_row_number <- which(
                            isolate(old_values())[[row_variable]] ==
                                rows[row_number])
                        isolate(old_values())[[variable_name]][old_row_number]
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
                    
                    # as character makes the element HTML, which can then be
                    # not escaped when rendering the table
                    widget <- as.character(
                        create_element(element,
                                       width = width,
                                       override_code_name = code_name,
                                       override_label = "",
                                       override_value = value,
                                       override_choices = choices,
                                       override_selected = value,
                                       override_placeholder = placeholder
                                       ))
                    table_to_display[row_number,variable_name] <- widget
                }
            }
            
            # add code names to the row names. These will be changed to display
            # names when rendering
            rownames(table_to_display) <- rows
            
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

            # clear override_values
            isolate(override_values(NULL))
            if (log) message(glue("Calculated table, has ",
                "{nrow(table_to_display)} rows"))
            table_to_display
        })
        
        output$table <- DT::renderDataTable({
            
            #message("Table rendering initiated")
            
            # visible used to be isolated, does this cause binding issues?
            # edit: YES, but fixed them
            # TODO: added table_data() here, does it cause issues?
            req(visible(), table_data())
            
            if (log) message(glue("Rendering table ({id})"))
            
            table_to_display <- table_data()
            
            if (nrow(table_to_display) == 0) {
                if (log) message(glue("No rows, didn't render ({id})"))
                return()
            }
            
            names(table_to_display) <- get_disp_name(variables,
                                                     language = language(),
                                                     is_variable_name = TRUE)
            rownames(table_to_display) <- get_disp_name(
                rownames(table_to_display),
                language = language())
            
            table_to_display
            
        }, escape = FALSE, select = "none", class = "compact", server = FALSE,
        options =
            list(dom = "t",
                 # here we defined columns 0 through n_cols unorderable
                 # instead of n_cols - 1, because row names are visible
                 columnDefs = list(
                     list(orderable = FALSE, targets = 0:n_cols)),
                 # binds the inputs when drawing is done
                 drawCallback = JS(js_bind_script),
                 # calls selectize() on all selectInputs, which makes them
                 # look the way they should
                 initComplete = JS(paste0("function(settings, json) {",
                     "do_selectize('", NS(id, "table"), "'); ",
                     "rendering_done('", NS(id, "rendered"), "'); }"
                 ))
                )
        )
        
        # entered values before we e.g. add rows. This allows us to fetch
        # the back when generating a new table
        old_values <- reactiveVal()
        table_values <-reactiveVal()
        
        # return the values of the table widgets
        # has to run when visibility changes because input values are not
        # available otherwise
        observe({

            value_list <- list()
            value_list[[row_variable]] <- rownames(table_data())
            if (length(value_list[[row_variable]]) == 0 | !rendered()) {
                #old_values(value_list)
                if (log) message(glue("Values observe blocked ({id})"))
                #return(value_list)
                table_values(value_list)
                return()
            }
            
            if (log) message(glue("Values observe running ({id})"))
            
            for (variable in variables) {
                values <- NULL
                for (row_number in 1:length(value_list[[row_variable]])) {
                    element_name <- paste(variable, row_number, sep = "_")
                    values <- c(values, input[[element_name]])
                }
                value_list[[variable]] <- values
            }

            isolate(old_values(value_list))
            table_values(value_list)
        })

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