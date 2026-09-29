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
  widgets <- list()
  
  # Render event-level properties (excluding discriminator and const-only)
  for (pn in event_entry$property_names) {
    desc <- lookup_property(pr, pn, event_const)
    if (is.null(desc)) next
    if (desc$type == "const") next
    
    if (desc$type == "dataTable") {
      w <- render_array_table(desc, ns, iso)
    } else if (desc$is_discriminator && event_entry$has_subtypes) {
      w <- render_subtype_section(desc, event_entry, pr, ns, iso,
                                   event_const)
    } else {
      w <- render_property_widget(desc$id, desc, ns, iso)
    }
    # Wrap in conditionalPanel if x-ui condition is defined
    if (!is.null(desc$condition)) {
      w <- conditionalPanel(
        condition = convert_condition_to_js(desc$condition, ns), w)
    }
    widgets[[length(widgets) + 1]] <- w
  }

  tagList(widgets)
}

#' Render a subtype discriminator and its conditional panels
render_subtype_section <- function(desc, event_entry, pr, ns, iso, 
                                    event_const) {
  # Render the discriminator selectInput with subtype choices
  subtype_choices <- build_subtype_choices(event_entry, iso)
  discriminator_widget <- render_property_widget(desc$id, desc, ns, iso,
                                                  override_choices = subtype_choices)
  
  # Build subtype conditional panels
  subtype_panels <- lapply(names(event_entry$subtypes), function(sub_const) {
    sub <- event_entry$subtypes[[sub_const]]
    
    sub_widgets <- list()
    for (spn in sub$property_names) {
      sdesc <- lookup_property(pr, spn, event_const, sub_const)
      if (is.null(sdesc)) next
      if (sdesc$type == "const") next
      
      if (sdesc$type == "dataTable") {
        sw <- render_array_table(sdesc, ns, iso)
      } else {
        sw <- render_property_widget(sdesc$id, sdesc, ns, iso)
      }
      if (!is.null(sdesc$condition)) {
        sw <- conditionalPanel(
          condition = convert_condition_to_js(sdesc$condition, ns), sw)
      }
      sub_widgets[[length(sub_widgets) + 1]] <- sw
    }
    
    condition <- paste0("input['", ns(desc$id), "'] == '", sub_const, "'")
    conditionalPanel(condition = condition, tagList(sub_widgets))
  })
  
  tagList(discriminator_widget, subtype_panels)
}

#' Render a single Shiny input widget from a property descriptor
#' @param prop_name Input ID of the widget (desc$id for form fields)
#' @param desc Property descriptor from the registry
#' @param ns Namespace function
#' @param iso ISO language code
#' @param override_code_name Optional code name override (for table cells)
#' @param override_label Optional label override
#' @param override_value Optional value override
#' @param override_choices Optional choices override
#' @param override_selected Optional selected value override
#' @param override_placeholder Optional placeholder override
#' @param width Optional width
#' @return A Shiny widget tag
render_property_widget <- function(prop_name, desc, ns, iso,
                                    override_code_name = NULL,
                                    override_label = NULL,
                                    override_value = NULL,
                                    override_choices = NULL,
                                    override_selected = NULL,
                                    override_placeholder = NULL,
                                    width = NULL) {
  input_id <- ns(if (!is.null(override_code_name)) {
    override_code_name
  } else {
    prop_name
  })
  
  label <- if (!is.null(override_label)) {
    override_label
  } else {
    make_required_label(
      schema_get_title(desc$titles, iso, desc$name),
      desc$required
    )
  }

  value <- if (!is.null(override_value)) override_value else ""
  
  placeholder <- if (!is.null(override_placeholder)) {
    override_placeholder
  } else if (!is.null(desc$placeholders)) {
    schema_get_title(desc$placeholders, iso, "")
  } else {
    NULL
  }
  
  wtype <- desc$type
  extra_args <- list()
  if (!is.null(width)) extra_args$width <- width
  
  if (wtype == "selectInput") {
    choices <- if (!is.null(override_choices)) {
      override_choices
    } else {
      schema_get_choices(desc$choices, iso)
    }
    if (is.null(choices)) choices <- stats::setNames("", "")
    selected <- override_selected
    
    do.call(selectInput, c(list(
      inputId = input_id,
      label = label,
      choices = choices,
      selected = selected
    ), extra_args))
    
  } else if (wtype == "numericInput") {
    num_val <- if (is.numeric(override_value)) override_value else NA
    num_min <- if (!is.null(desc$minimum)) desc$minimum else NA
    num_max <- if (!is.null(desc$maximum)) desc$maximum else NA
    
    do.call(numericInput, c(list(
      inputId = input_id,
      label = label,
      value = num_val,
      min = num_min,
      max = num_max,
      step = if (isTRUE(desc$is_integer)) 1 else "any"
    ), extra_args))
    
  } else if (wtype == "dateInput") {
    date_val <- if (!is.null(override_value) && nchar(override_value) > 0) {
      tryCatch(as.Date(override_value), error = function(e) Sys.Date())
    } else {
      Sys.Date()
    }
    
    do.call(dateInput, c(list(
      inputId = input_id,
      label = label,
      value = date_val,
      format = "dd/mm/yyyy",
      max = Sys.Date(),
      weekstart = 1
    ), extra_args))
    
  } else if (wtype == "textAreaInput") {
    do.call(textAreaInput, c(list(
      inputId = input_id,
      label = label,
      value = value,
      resize = "vertical",
      placeholder = placeholder
    ), extra_args))
    
  } else if (wtype == "textInput") {
    do.call(textInput, c(list(
      inputId = input_id,
      label = label,
      value = value,
      placeholder = placeholder
    ), extra_args))
    
  } else {
    # Fallback
    textInput(inputId = input_id, label = label, value = value)
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
#' @param override_choices Optional choices (e.g. for a subtype discriminator)
update_schema_widget <- function(session, desc, iso, input,
                                 override_choices = NULL) {
  id <- desc$id
  label <- make_required_label(
    schema_get_title(desc$titles, iso, desc$name),
    desc$required
  )
  placeholder <- if (!is.null(desc$placeholders)) {
    schema_get_title(desc$placeholders, iso, "")
  }
  
  if (desc$type == "selectInput") {
    choices <- override_choices %||% schema_get_choices(desc$choices, iso)
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
  result <- c("", values)
  names(result) <- c("", labels)
  result
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
  result <- c("", values)
  names(result) <- c("", labels)
  result
}

#' Update a schema-driven widget value (for populating forms)
#' @param session Shiny session
#' @param desc Property descriptor
#' @param value The value to set. NULL clears the widget
update_schema_value <- function(session, desc, value) {
  if (is.null(desc)) return()
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
