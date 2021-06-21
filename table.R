# Shiny module for data input in table format
# Otto Kuusela 2021

tableInput <- function(id) {
    tagList(DT::dataTableOutput(NS(id, "table")), br())
}

tableServer <- function(id, row_names, language) {

    stopifnot(is.reactive(row_names))
    stopifnot(is.reactive(language))
    
    moduleServer(id, function(input, output, session) {
        
        # get corresponding element info to determine which widgets to add to
        # the table
        table_structure <- structure_lookup_list[[id]]
        columns <- table_structure$columns
        n_cols <- length(columns)
        
        output$table <- DT::renderDataTable({

            n_rows <- length(row_names())
            table_to_display <- data.frame()
            
            for (row_number in 1:n_rows) {
                for (variable_name in columns) {
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
                    code_name <- paste(variable_name, row_number, sep = "_")
                    widget <- as.character(
                        create_element(element,
                                       width = width,
                                       override_code_name = code_name,
                                       override_label = ""))
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
            
            names(table_to_display) <- get_disp_name(columns, 
                                                     language = language(),
                                                     is_variable_name = TRUE)
            rownames(table_to_display) <- get_disp_name(row_names(),
                                                        language = language())
            table_to_display
        }, escape = FALSE, select = "none",
        options = 
            list(dom = "t",
                 # here we defined columns 0 through n_cols unorderable
                 # instead of n_cols - 1, because row names are visible
                 columnDefs = list( 
                     list(orderable = FALSE, targets = 0:n_cols)), 
                 preDrawCallback = JS(
                 'function() { Shiny.unbindAll(this.api().table().node()); }'),
                 drawCallback = JS(
                 'function() { Shiny.bindAll(this.api().table().node()); }')
                 )
        )
        
        
        reactive(input)
    })
}

tableApp <- function(row_names) {
    ui <- fluidPage(
        tableInput("table")
    )
    server <- function(input, output, session) {
        tableServer("table", row_names)
    }
    
    shinyApp(ui, server)
}
        
        
