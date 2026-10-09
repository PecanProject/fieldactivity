# Schema-driven UI renderer
# Generates Shiny widgets from the parsed management-event schema

#' Create a label with a required asterisk indicator
#' @param label The label text
#' @param required Whether the field is required
#' @return The label, optionally with a red asterisk appended
make_required_label <- function(label, required) {
  if (!isTRUE(required) || is.null(label) || identical(label, "")) return(label)
  tagList(label, tags$span(" *", class = "required-asterisk"))
}

#' Convert a condition string from schema x-ui format to JavaScript for conditionalPanel
#' Replaces input.FIELD with input['ns-FIELD'] for namespaced Shiny modules
#' @param condition The condition string (e.g. "input.organic_material == 'RE003'")
#' @param ns Namespace function
#' @return A JavaScript condition string with namespaced input references
convert_condition_to_js <- function(condition, ns) {
  gsub("input\\.(\\w+)", paste0("input['", ns("\\1"), "']"), condition,
       perl = TRUE)
}

#' Build the validator for a schema-driven widget
#'
#' Rules come from the property descriptor: required, minimum, maximum and
#' whole numbers. An empty optional widget skips the other rules.
#' @param desc Property descriptor
#' @param id Input ID of the widget
#' @return An InputValidator, or NULL if the property has no rules
#' @import shinyvalidate
field_validator <- function(desc, id = desc$id) {
  rules <- list()
  if (!is.null(desc$minimum)) {
    rules <- c(rules, sv_gte(desc$minimum, message_fmt = "Must be >= {rhs}"))
  }
  if (!is.null(desc$maximum)) {
    rules <- c(rules, sv_lte(desc$maximum, message_fmt = "Must be <= {rhs}"))
  }
  if (isTRUE(desc$is_integer)) {
    rules <- c(rules, function(value) {
      if (value != floor(value)) "Must be a whole number"
    })
  }
  if (!isTRUE(desc$required) && length(rules) == 0) return(NULL)
  
  iv <- InputValidator$new()
  if (isTRUE(desc$required)) {
    iv$add_rule(id, sv_required(message = "Required"))
  } else {
    iv$add_rule(id, sv_optional())
  }
  for (rule in rules) iv$add_rule(id, rule)
  iv
}

#' Render the full schema-driven form
#' @param schema The loaded schema (from load_schema)
#' @param ns Shiny namespace function
#' @param language Language code or column name
#' @return A tagList of Shiny UI elements
render_schema_form <- function(schema, ns, language) {
  iso <- lang_to_iso(language)
  er <- schema$event_registry
  pr <- schema$property_registry
  
  # Build per-event-type conditional panels
  event_panels <- lapply(names(er), function(event_const) {
    event_entry <- er[[event_const]]
    panel_content <- render_event_panel(event_entry, pr, ns, iso, event_const)
    
    condition <- paste0("input['", ns("mgmt_operations_event"), 
                        "'] == '", event_const, "'")
    conditionalPanel(condition = condition, panel_content)
  })
  
  tagList(event_panels)
}

#' Render the panel for a single event type
#' @param event_entry An event registry entry
#' @param pr Property registry
#' @param ns Namespace function
#' @param iso ISO language code
#' @param event_const The event type const value
#' @return A tagList
render_event_panel <- function(event_entry, pr, ns, iso, event_const) {
  widgets <- lapply(event_entry$property_names, function(pn) {
    desc <- lookup_property(pr, pn, event_const)
    if (desc$is_discriminator && event_entry$has_subtypes) {
      render_subtype_section(desc, event_entry, pr, ns, iso, event_const)
    } else {
      render_field(desc, ns, iso)
    }
  })
  tagList(widgets)
}

#' Render the widget or table of a field, shown only when its x-ui condition
#' (if any) is met
#' @param desc Property descriptor
#' @param ns Namespace function
#' @param iso ISO language code
#' @return A Shiny tag
render_field <- function(desc, ns, iso) {
  w <- if (desc$type == "dataTable") {
    render_array_table(desc, ns, iso)
  } else if (desc$type == "fileInput") {
    mod_fileInput_ui(ns(desc$id), desc, iso)
  } else {
    render_property_widget(desc$id, desc, ns, iso)
  }
  if (is.null(desc$condition)) return(w)
  conditionalPanel(condition = convert_condition_to_js(desc$condition, ns), w)
}

#' Render a subtype discriminator and its conditional panels
render_subtype_section <- function(desc, event_entry, pr, ns, iso, 
                                    event_const) {
  # Render the discriminator selectInput with subtype choices
  subtype_choices <- build_subtype_choices(event_entry, iso)
  discriminator_widget <- render_property_widget(desc$id, desc, ns, iso,
                                                  choices = subtype_choices)
  
  # Build subtype conditional panels
  subtype_panels <- lapply(names(event_entry$subtypes), function(sub_const) {
    sub_widgets <- lapply(event_entry$subtypes[[sub_const]]$property_names,
                          function(spn) {
      render_field(lookup_property(pr, spn, event_const, sub_const), ns, iso)
    })
    condition <- paste0("input['", ns(desc$id), "'] == '", sub_const, "'")
    conditionalPanel(condition = condition, tagList(sub_widgets))
  })
  
  tagList(discriminator_widget, subtype_panels)
}

#' Render a single Shiny input widget from a property descriptor
#' @param id Input ID of the widget (desc$id for form fields)
#' @param desc Property descriptor from the registry
#' @param ns Namespace function
#' @param iso ISO language code
#' @param label Optional label, the property title by default
#' @param value Optional initial value (the selected value of a selectInput)
#' @param choices Optional choices, the property's choices by default
#' @param width Optional width
#' @return A Shiny widget tag
render_property_widget <- function(id, desc, ns, iso, label = NULL,
                                   value = NULL, choices = NULL,
                                   width = NULL) {
  input_id <- ns(id)
  label <- label %||% make_required_label(
    schema_get_title(desc$titles, iso, desc$name), desc$required)
  placeholder <- if (!is.null(desc$placeholders)) {
    schema_get_title(desc$placeholders, iso, "")
  }

  if (desc$type == "selectInput") {
    choices <- choices %||% schema_get_choices(desc$choices, iso) %||%
      stats::setNames("", "")
    selectInput(input_id, label = label, choices = choices, selected = value,
                width = width)
  } else if (desc$type == "numericInput") {
    numericInput(input_id, label = label,
                 value = if (is.numeric(value)) value else NA,
                 min = desc$minimum %||% NA, max = desc$maximum %||% NA,
                 step = if (isTRUE(desc$is_integer)) 1 else "any",
                 width = width)
  } else if (desc$type == "dateInput") {
    date_val <- if (isTruthy(value)) {
      tryCatch(as.Date(value), error = function(e) Sys.Date())
    } else {
      Sys.Date()
    }
    dateInput(input_id, label = label, value = date_val, format = "dd/mm/yyyy",
              max = Sys.Date(), weekstart = 1, width = width)
  } else if (desc$type == "textAreaInput") {
    textAreaInput(input_id, label = label, value = value %||% "",
                  resize = "vertical", placeholder = placeholder,
                  width = width)
  } else {
    textInput(input_id, label = label, value = value %||% "",
              placeholder = placeholder, width = width)
  }
}

#' Render a schema array property as a table module placeholder
#' @param desc The property descriptor of the array (e.g. planting_list)
#' @param ns Namespace function
#' @param iso ISO language code
#' @return A tagList with the table module UI
render_array_table <- function(desc, ns, iso) {
  table_id <- schema_table_id(desc)
  table_ns <- NS(ns(table_id))
  div(
    class = "schema-array-table",
    mod_table_ui(ns(table_id)),
    div(
      class = "schema-array-table__footer",
      actionButton(table_ns("add_row"),
                   label = schema_table_add_row_label(iso),
                   icon = icon("plus"),
                   class = "btn-sm btn-default")
    )
  )
}

#' Module ID of the table for an array property
#' @param desc The property descriptor of the array
#' @return The table module ID
schema_table_id <- function(desc) {
  paste0(desc$id, "_table")
}

#' Update a single schema widget's label and choices
#' @param session Shiny session
#' @param desc Property descriptor
#' @param iso ISO language code
#' @param input The input of the session, to keep the current selection
#' @param choices Optional choices (e.g. for a subtype discriminator)
update_schema_widget <- function(session, desc, iso, input, choices = NULL) {
  id <- desc$id
  label <- make_required_label(
    schema_get_title(desc$titles, iso, desc$name),
    desc$required
  )
  placeholder <- if (!is.null(desc$placeholders)) {
    schema_get_title(desc$placeholders, iso, "")
  }
  
  if (desc$type == "selectInput") {
    choices <- choices %||% schema_get_choices(desc$choices, iso)
    updateSelectInput(session, id, label = label, choices = choices,
                      selected = input[[id]])
  } else if (desc$type == "numericInput") {
    updateNumericInput(session, id, label = label)
  } else if (desc$type == "dateInput") {
    updateDateInput(session, id, label = label)
  } else if (desc$type == "textAreaInput") {
    updateTextAreaInput(session, id, label = label, placeholder = placeholder)
  } else if (desc$type == "textInput") {
    updateTextInput(session, id, label = label, placeholder = placeholder)
  }
}

#' Build event type selectInput choices from the schema
#' @param schema Loaded schema
#' @param iso ISO language code
#' @return Named character vector for selectInput
build_event_type_choices <- function(schema, iso) {
  values <- names(schema$event_type_choices)
  labels <- vapply(schema$event_type_choices, function(titles) {
    schema_get_title(titles, iso, "")
  }, character(1))
  stats::setNames(c("", values), c("", labels))
}

#' Build subtype selectInput choices
#' @param event_entry An event registry entry
#' @param iso ISO language code
#' @return Named character vector for selectInput
build_subtype_choices <- function(event_entry, iso) {
  if (!event_entry$has_subtypes) return(NULL)
  subtypes <- event_entry$subtypes
  values <- vapply(subtypes, function(s) s$const, character(1))
  labels <- vapply(subtypes, function(s) {
    schema_get_title(s$titles, iso, s$const)
  }, character(1))
  stats::setNames(c("", values), c("", labels))
}

#' Update a schema-driven widget value (for populating forms)
#' @param session Shiny session
#' @param desc Property descriptor
#' @param value The value to set. NULL clears the widget
update_schema_value <- function(session, desc, value) {
  id <- desc$id
  if (is.atomic(value) && all(is_missing_value(value))) value <- NULL
  
  wtype <- desc$type
  
  if (wtype == "selectInput") {
    updateSelectInput(session, id, selected = value %||% "")
  } else if (wtype == "numericInput") {
    updateNumericInput(session, id,
                       value = if (is.null(value)) NA else as.numeric(value))
  } else if (wtype == "dateInput") {
    date_val <- tryCatch(as.Date(value, format = date_format_json),
                         warning = function(cnd) NULL,
                         error = function(cnd) NULL)
    if (length(date_val) != 1 || is.na(date_val)) {
      # updateDateInput ignores NULL, a null value clears the input
      session$sendInputMessage(id, list(value = NA))
    } else {
      updateDateInput(session, id, value = date_val)
    }
  } else if (wtype == "textAreaInput") {
    updateTextAreaInput(session, id, value = value %||% "")
  } else if (wtype == "textInput") {
    updateTextInput(session, id, value = value %||% "")
  }
}
