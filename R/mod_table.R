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

# -- Helpers for mod_table_server_schema ------------------------------------

#' Build a single table cell widget as an HTML string
#' @noRd
build_schema_cell_widget <- function(variable, col_desc, ns, iso,
                                      current_row, value) {
  code_name <- paste(variable, current_row, sep = "_")

  if (!isTruthy(value) || identical(value, missingval)) value <- ""

  choices <- NULL
  if (identical(col_desc$type, "selectInput")) {
    choices <- schema_get_choices(col_desc$choices, iso)
  }

  placeholder <- NULL
  if (!is.null(col_desc$placeholders)) {
    placeholder <- schema_get_title(col_desc$placeholders, iso, "")
  }

  width <- if (col_desc$type == "numericInput") 100 else NULL

  widget_html <- as.character(
    render_property_widget(variable, col_desc, ns, iso,
                            override_code_name = code_name,
                            override_label = "",
                            override_value = value,
                            override_choices = choices,
                            override_selected = value,
                            override_placeholder = placeholder,
                            width = width))

  list(html = widget_html, code_name = code_name)
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

#' Schema-driven table server module
#'
#' @param id Module ID (must match the table_id used in render_array_table)
#' @param array_prop_name The property name of the array in the schema
#' @param desc The property descriptor for the array
#' @param schema The loaded schema
#' @param language Reactive language value
#' @param override_values ReactiveVal for setting table values
#' @param parent_input The parent module's input
#' @param parent_iv The parent module's InputValidator
#' @param parent_ns The parent module's namespace function
#'
#' @return A list with values() and valid() reactives
#' @import shinyvalidate
#' @noRd
mod_table_server_schema <- function(id, array_prop_name, desc, schema,
                                     language, override_values,
                                     parent_input, parent_iv, parent_ns) {

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

    add_schema_validation_rules <- function(widgets, variables) {
      lapply(seq_along(widgets), FUN = function(i) {
        widget_name <- widgets[i]
        col_desc <- columns[[variables[i]]]

        if (widget_name %in% rules_added) return()

        # Extract the row number from the widget name (e.g. "crop_name_2" -> 2)
        row_num <- as.integer(sub(paste0("^", variables[i], "_"), "", widget_name))

        child_iv <- InputValidator$new()

        if (isTRUE(col_desc$required)) {
          child_iv$add_rule(widget_name, sv_required(message = "Required"))
        }
        if (!is.null(col_desc$minimum)) {
          child_iv$add_rule(widget_name, sv_gte(col_desc$minimum, allow_na = TRUE,
                                           message_fmt = "Must be >= {rhs}"))
        }
        if (!is.null(col_desc$maximum)) {
          child_iv$add_rule(widget_name, sv_lte(col_desc$maximum, allow_na = TRUE,
                                           message_fmt = "Must be <= {rhs}"))
        }
        if (isTRUE(col_desc$is_integer)) {
          child_iv$add_rule(widget_name, function(value) {
            if (is.null(value) || is.na(value)) return(NULL)
            if (value != floor(value)) return("Must be a whole number")
            NULL
          })
        }

        # Only validate when this row still exists
        local({
          local_row <- row_num
          child_iv$condition(reactive({
            local_row %in% dynamic_rows()
          }))
        })
        iv$add_validator(child_iv)
      })
      rules_added <<- c(rules_added, widgets)
    }

    n_cols <- length(column_names)
    old_values <- reactiveVal()
    table_values <- reactiveVal()
    rendered <- reactiveVal(FALSE)
    dynamic_rows <- reactiveVal()

    observeEvent(input$rendered, { rendered(TRUE) })

    visible <- reactive({
      rows <- dynamic_rows()
      !is.null(rows) && length(rows) > 0
    })

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

      # Determine rows from the data
      first_col <- column_names[1]
      col_data <- values[[first_col]]
      if (!is.null(col_data) && length(col_data) > 0) {
        dynamic_rows(as.integer(seq_along(col_data)))
      } else {
        dynamic_rows(1L)
      }
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
      if (is.null(current) || length(current) == 0) {
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
      if (is.null(current) || length(current) <= 1) return()
      if (is.null(row_id) || !(row_id %in% current)) return()
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

    # Sum calculation for schema tables is handled by the parent form module's
    # auto-sum observer, not inside the table module itself.

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
      if (is.null(rows) || length(rows) == 0) rows <- integer(0)
      can_remove_rows <- length(rows) > 1L

      current_row <- 1
      for (row_idx in rows) {
        for (variable in column_names) {
          col_desc <- columns[[variable]]

          value <- if (do_override) {
            override_vals[[variable]][row_idx]
          } else {
            old_row_number <- which(
              isolate(old_values())[["DYNAMIC_ROWS"]] == rows[current_row])
            isolate(old_values())[[variable]][old_row_number]
          }

          cell <- build_schema_cell_widget(variable, col_desc, ns, iso,
                                            current_row, value)
          add_schema_validation_rules(cell$code_name, variable)
          table_to_display[current_row, variable] <- cell$html
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
      # Use unitless titles or regular titles for column headers
      col_labels <- vapply(column_names, function(cn) {
        col_desc <- columns[[cn]]
        if (!is.null(col_desc$unitless_titles)) {
          schema_get_title(col_desc$unitless_titles, iso, cn)
        } else {
          schema_get_title(col_desc$titles, iso, cn)
        }
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
      value_list <- list()

      if (!rendered()) {
        table_values(value_list)
        return()
      }

      table_data()

      rows <- dynamic_rows()
      if (is.null(rows) || length(rows) == 0) {
        table_values(value_list)
        return()
      }

      row_numbers <- seq_along(rows)

      for (variable in column_names) {
        values <- NULL
        for (row_number in row_numbers) {
          element_name <- paste(variable, row_number, sep = "_")
          values <- c(values, input[[element_name]])
        }
        value_list[[variable]] <- values
      }

      table_values(value_list)

      value_list <- c(value_list, list(DYNAMIC_ROWS = isolate(dynamic_rows())))
      old_values(value_list)
    })

    list(
      values = table_values,
      valid = reactive(iv$is_valid())
    )
  })
}
