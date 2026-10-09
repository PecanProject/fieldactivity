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

# TODO: Move these labels to display_names.csv (or the schema's x-ui) once the
# CSV gains a Swedish column. Until then, Swedish is hardcoded here.
schema_table_add_row_label <- function(iso) {
  if (identical(iso, "fi")) {
    "Lis\u00e4\u00e4 rivi"
  } else if (identical(iso, "sv")) {
    "L\u00e4gg till rad"
  } else {
    "Add row"
  }
}

schema_table_remove_row_label <- function(iso) {
  if (identical(iso, "fi")) {
    "Poista rivi"
  } else if (identical(iso, "sv")) {
    "Ta bort rad"
  } else {
    "Remove row"
  }
}

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
    
#' Build a single table cell widget as an HTML string
#' @noRd
build_cell_widget <- function(code_name, col_desc, ns, iso, value) {
  if (!isTruthy(value)) value <- ""
  width <- if (col_desc$type == "numericInput") 100 else NULL
  as.character(render_property_widget(code_name, col_desc, ns, iso,
                                      label = "", value = value,
                                      width = width))
}

#' Build a remove-row button as an HTML string
#' @noRd
build_remove_row_button <- function(ns, iso, row_idx, can_remove) {
  as.character(
    tags$button(
      type = "button",
      class = "btn btn-default btn-sm schema-array-table__remove-row",
      title = schema_table_remove_row_label(iso),
      `aria-label` = schema_table_remove_row_label(iso),
      onclick = sprintf(
        "Shiny.setInputValue('%s', %d, {priority: 'event'})",
        ns("remove_row_index"),
        row_idx
      ),
      disabled = if (!can_remove) "disabled" else NULL,
      icon("trash")
    )
  )
}

#' Table server module
#'
#' @param id Module ID (must match the table_id used in render_array_table)
#' @param desc The property descriptor for the array
#' @param language Reactive language value
#' @param override_values ReactiveVal for setting table values, as a list of
#'   rows (named lists keyed by column name)
#'
#' @return A list with values() and valid() reactives. values() holds the
#'   table contents as a list of rows (named lists keyed by column name)
#' @import shinyvalidate
#' @noRd
mod_table_server <- function(id, desc, language, override_values) {
  
  stopifnot(is.reactive(language))
  stopifnot(is.reactive(override_values))
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    columns <- desc$array_columns
    if (is.null(columns)) return(list(values = reactiveVal(list()),
                                       valid = reactive(TRUE)))

    column_names <- names(columns)
    action_column_name <- "..remove_row.."
    
    iv <- InputValidator$new()
    iv$enable()
    rules_added <- NULL
        
    add_validation_rule <- function(widget_name, col_desc, row_number) {
      force(row_number)
      if (widget_name %in% rules_added) return()
      rules_added <<- c(rules_added, widget_name)
        
      child_iv <- field_validator(col_desc, widget_name)
      if (is.null(child_iv)) return()
        
      # Only validate when this row is displayed. Widgets are numbered by
      # display position, so compare with the number of rows rather than
      # with the (stable) row ids
      child_iv$condition(reactive(row_number <= length(dynamic_rows())))
      iv$add_validator(child_iv)
        }
        
    n_cols <- length(column_names)
    # the latest entered rows and their dynamic row ids, used to keep the
    # entered values when the table is re-rendered
    old_values <- reactiveVal()
    table_values <- reactiveVal()
    rendered <- reactiveVal(FALSE)
    dynamic_rows <- reactiveVal()
        
    observeEvent(input$rendered, { rendered(TRUE) })

    visible <- reactive(length(dynamic_rows()) > 0)
    
    observeEvent(visible(), ignoreNULL = FALSE, ignoreInit = TRUE,
                 priority = 1, {
      if (!visible()) {
        rendered(FALSE)
        old_values(list())
      }
    })
    
    override_trigger <- reactiveVal(0)
    row_trigger <- reactiveVal(0)
    
    observeEvent(override_values(), {
      values <- override_values()
      if (is.null(values)) return()

      dynamic_rows(if (length(values) > 0) seq_along(values) else 1L)
      override_trigger(override_trigger() + 1)
    })
    
    # Initialize with one row when first accessed (no override data)
    observe({
      if (is.null(dynamic_rows())) {
        dynamic_rows(1L)
      }
    }, priority = -1)

    # Add row handler
    observeEvent(input$add_row, {
      current <- dynamic_rows()
      if (length(current) == 0) {
        dynamic_rows(1L)
      } else {
        dynamic_rows(c(current, max(current) + 1L))
      }
        row_trigger(row_trigger() + 1)
    })
    
    # Remove a specific row while keeping stable row ids.
    observeEvent(input$remove_row_index, {
      current <- dynamic_rows()
      row_id <- input$remove_row_index
      if (length(current) <= 1 || !(row_id %in% current)) return()
      dynamic_rows(current[current != row_id])
      row_trigger(row_trigger() + 1)
    })
    
    # Update button labels on language change
    observeEvent(language(), {
      iso <- lang_to_iso(language())
      updateActionButton(session, "add_row",
                         label = schema_table_add_row_label(iso))
    })

    # Unbind before re-render
    observe(priority = 2, {
      language()
      visible()
      row_trigger()
      override_trigger()
      req(isolate(rendered()))
      session$sendCustomMessage("unbind-table", ns("table"))
    })
    
    table_data <- reactive({
      override_trigger()
      row_trigger()
      
      iso <- lang_to_iso(language())
      override_vals <- isolate(override_values())
      do_override <- !is.null(override_vals)
      
      table_to_display <- data.frame(
        matrix("", nrow = 0, ncol = n_cols + 1L),
        stringsAsFactors = FALSE
      )
      names(table_to_display) <- c(column_names, action_column_name)
      
      if (do_override && identical(override_vals, list())) {
          override_values(NULL)
          do_override <- FALSE
          old_values(list())
        }

      rows <- isolate(dynamic_rows())
      can_remove_rows <- length(rows) > 1L

      current_row <- 1
      for (row_idx in rows) {
        for (variable in column_names) {
          col_desc <- columns[[variable]]

          value <- if (do_override) {
            override_vals[[row_idx]][[variable]]
          } else {
            old <- isolate(old_values())
            old_row_number <- match(row_idx, old$row_ids)
            if (!is.na(old_row_number)) old$rows[[old_row_number]][[variable]]
      }
      
          code_name <- paste(variable, current_row, sep = "_")
          add_validation_rule(code_name, col_desc, current_row)
          table_to_display[current_row, variable] <-
            build_cell_widget(code_name, col_desc, ns, iso, value)
            }
            
        table_to_display[current_row, action_column_name] <-
          build_remove_row_button(ns, iso, row_idx, can_remove_rows)
            
        rownames(table_to_display)[current_row] <- as.character(row_idx)
            current_row <- current_row + 1
          }
          
      override_values(NULL)
      table_to_display
    })
    
    output$table <- DT::renderDataTable({
      req(visible())
      rendered(FALSE)
      table_to_display <- table_data()
      
      if (nrow(table_to_display) == 0) return()
      
      iso <- lang_to_iso(language())
      # full titles, as they include the unit
      col_labels <- vapply(column_names, function(cn) {
        schema_get_title(columns[[cn]]$titles, iso, cn)
      }, character(1))
      names(table_to_display) <- c(col_labels, "")
      
      table_to_display <- 
        DT::datatable(
          table_to_display, 
          escape = FALSE,
          selection = "none",
          class = "table table-hover",
          rownames = FALSE,
          options = 
            list(dom = "t",
                 ordering = FALSE,
                 autoWidth = FALSE,
                 drawCallback = htmlwidgets::JS(js_bind_script),
                 initComplete = 
                   htmlwidgets::JS(paste0(
                     "function(settings, json) {",
                     "do_selectize('", ns("table"), "'); ",
                     "rendering_done('", ns("rendered"), "'); }"
                   )),
                 columnDefs = list(
                   list(
                     orderable = FALSE,
                     targets = ncol(table_to_display) - 1L,
                     className = "schema-array-table__actions-cell",
                     width = "1%"
                   )
                 )
            ))
        table_to_display
    }, server = FALSE)
    
    observe({
      if (!rendered()) {
        table_values(list())
        return()
      }
      
      table_data()
      
      rows <- dynamic_rows()
      if (length(rows) == 0) {
        table_values(list())
        return()
      }
      
      # widgets are numbered by display position, not by dynamic row id
      row_values <- lapply(seq_along(rows), function(row_number) {
        row <- lapply(column_names, function(variable) {
          input[[paste(variable, row_number, sep = "_")]]
        })
        names(row) <- column_names
        row
    })
    
      table_values(row_values)
      old_values(list(rows = row_values, row_ids = rows))
    })
    
    list(
      values = table_values,
      valid = reactive(iv$is_valid())
    )
    
  })
    
}

