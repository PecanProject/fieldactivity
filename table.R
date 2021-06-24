# Shiny module for data input in table format
# Otto Kuusela 2021

# javascript callback scripts must be wrapped inside a function for some reason
js_bind_script <- "function() { Shiny.bindAll(this.api().table().node()); }"

js_unbind_script <- paste(sep = "",
          "Shiny.addCustomMessageHandler('unbind-table', function(id) {",
          #alert($('#'+id).find('.shiny-input-container').length);
          "Shiny.unbindAll($('#'+id).find('.shiny-input-container'));
          })")


js_selectize_script <- function(id) {
    paste(sep = "",
          "function selectize() { ",
          "return $('#", id, "').find('select').selectize(); }")
}
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
    
    tagList(tags$head(tags$script(HTML(js_unbind_script))),
            tags$head(tags$script(HTML(js_selectize_script(NS(id, "table"))))),
            DT::dataTableOutput(NS(id, "table")), 
            br()) 
}

tableServer <- function(id, row_names, language, visible) {

    stopifnot(is.reactive(row_names))
    stopifnot(is.reactive(language))
    stopifnot(is.reactive(visible))
    
    moduleServer(id, function(input, output, session) {
        
        # get corresponding element info to determine which widgets to add to
        # the table
        table_structure <- structure_lookup_list[[id]]
        variables <- table_structure$columns
        n_cols <- length(variables)
        
        # this unbinds the table elements before they are re-rendered.
        # Setting a higher priority ensures this runs before the table render
        observe(priority = 1, {
            # when to run observer
            row_names()
            language()
            visible()
            # require this so that we know the table has already rendered
            req(isolate(input$table_rows_current))
            session$sendCustomMessage("unbind-table", NS(id, "table"))
        })
        
        n_rows <- reactive(length(row_names()))
        
        table_data <- reactive({
            
            table_to_display <- data.frame(matrix(nrow = 0,
                                                  ncol = length(variables)))
            names(table_to_display) <- variables
            
            if (n_rows() == 0) {
                return(table_to_display)
            }
            
            # TODO: switch order so to make less computations
            for (row_number in 1:n_rows()) {
                for (variable_name in variables) {
                    element <- structure_lookup_list[[variable_name]]
                    
                    width <- if (element$type == "numericInput") {
                        "60px"
                    } else if (element$type == "textInput") {
                        "110px"
                    } else {
                        "100px"
                    }
                    
                    # the code names for these elements are
                    # variablename_rownumber
                    code_name <- NS(id, 
                                    paste(variable_name, row_number, sep = "_"))
                    old_row_number <- 
                        which(old_values$row_names == row_names()[row_number])
                    old_value  <- old_values[[variable_name]][old_row_number]
                    if (!isTruthy(old_value)) {
                        old_value <- ""
                    }
                    
                    # add choices in the correct language for selectInputs
                    choices <- NULL
                    if (element$type == "selectInput") {
                        choices <- get_selectInput_choices(element, language())
                    }
                    
                    widget <- as.character(
                        create_element(element,
                                       width = width,
                                       override_code_name = code_name,
                                       override_label = "",
                                       override_value = old_value,
                                       override_choices = choices,
                                       override_selected = old_value
                                       ))
                    table_to_display[row_number,variable_name] <- widget
                }
            }
            
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
            
            
            table_to_display
        })
        
        output$table <- DT::renderDataTable({
            
            req(visible())
            
            table_to_display <- table_data()
            
            if (nrow(table_to_display) == 0) {
                return()
            }
            
            names(table_to_display) <- get_disp_name(variables,
                                                     language = language(),
                                                     is_variable_name = TRUE)
            rownames(table_to_display) <- get_disp_name(isolate(row_names()),
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
                 # without the function() this doesn't work
                 initComplete = JS("function() { selectize(); }")
                )#,
        # callback = JS("Shiny.unbindAll(table.table().node());
        #               Shiny.bindAll(table.table().node());")
        )
        
        old_values <- NULL
        
        # return the values of the table widgets
        values <- reactive({
            
            value_list <- list()
            
            if (n_rows() == 0 || !visible()) {
                return(value_list)
            }
            value_list$row_names <- isolate(row_names())
            
            for (variable in variables) {
                values <- NULL
                for (row_number in 1:n_rows()) {
                    element_name <- paste(variable, row_number, sep = "_")
                    values <- c(values, input[[element_name]])
                }
                value_list[[variable]] <- values
            }

            old_values <<- value_list
            value_list
        })

    })
}

##########################################
# example application that uses the module


# row <- data.frame(eka = as.character(numericInput("moi", label = "muu", value = 3)),
#                  toka = as.character(selectInput("huhuu", label = "jaa",
#                                                  choices = c("1", "2"), multiple = FALSE)),
#                  kolkki = as.character(textInput("mahtava", label = "joo")))


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
                         verbatimTextOutput("debug_text_output")
                         
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
        
        data <- tableServer("harvest_crop_table", reactive(input$crop),
                           reactive(input$language), condition)
        
        output$debug_text_output <- renderPrint({ str(data()) })
        
        output$table_ui <- renderUI({
            if (condition()) {
                message("UI rendering")
                tableInput("harvest_crop_table")
            }
        })
        
        observe({
            str(data())
        })
    }

    shinyApp(ui, server)
}