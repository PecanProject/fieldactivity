#' Create UI elements based on parsed schema
#'
#' This function generates the main UI structure for the fertilizer application form
#' based on the parsed JSON schema.
#'
#' @param parsed_schema A list containing the parsed schema structure
#' @param ns A namespace function for Shiny module compatibility
#' @param language The current language code (e.g., "en" for English)
#'
#' @return A tagList containing all UI elements for the fertilizer application form
#'
#' @details
#' The function iterates through each event in the parsed schema and creates UI elements
#' for both common properties and oneOf sections. It uses helper functions
#' create_properties_ui() and create_oneof_ui() to generate these elements.
#'
#' @examples
#' parsed_schema <- parse_json_schema(schema)
create_ui <- function(parsed_schema, ns, language = "en") {
  ui_elements <- lapply(parsed_schema, function(event) {
    event_oneof <- create_oneof_ui(event$oneOf, ns, language)
    event_properties <- create_properties_ui(
      event$properties, ns, language,
      event_oneof$id, event_oneof$options
    )
    tagList(event_properties, event_oneof$elements)
  })

  return(tagList(ui_elements))
}


#' Create UI elements for a set of properties
#'
#' This function generates Shiny UI elements based on the parsed properties
#' from the JSON schema. It handles various property types and creates
#' appropriate input widgets, including the special case for 'oneOf' properties.
#'
#' @param properties A list of parsed properties (output from parse_property)
#' @param ns A namespace function for Shiny module compatibility
#' @param language The current language code (e.g., "en" for English)
#'
#' @return A list of Shiny UI elements corresponding to the input properties
#'
#' @details
#' The function creates UI elements for each property, handling different types:
#' - For simple types (string, number, boolean), it calls create_widget()
#' - For 'oneOf' properties, it creates a select input for choosing the option,
#'   and nested property inputs for each option
#'
#' For 'oneOf' properties, the function:
#' 1. Creates a select input for choosing between options
#' 2. Generates UI elements for each option's properties
#' 3. Implements JavaScript to show/hide property inputs based on selection
#'
#' @examples
#' properties <- list(
#'   name = list(type = "string", title = list(en = "Name")),
#'   age = list(type = "number", title = list(en = "Age"))
#' )
create_properties_ui <- function(properties, ns, language = "en", oneof_id = NULL, oneof_options = NULL) {
  lapply(names(properties), function(prop_name) {
    prop <- properties[[prop_name]]
    if (!is.null(prop$oneOf)) {
      # print(paste0("building ui for ", prop_name))
      # Handle oneOf properties
      oneof_id <- ns(paste0(prop_name, "_oneof"))

      # Create options with an empty default option
      oneof_options <-
        setNames(
          sapply(prop$oneOf, function(option) option$value),
          sapply(prop$oneOf, function(option) {
            if (!is.null(option$title) && !is.null(option$title[[language]])) {
              option$title[[language]]
            } else {
              option$value
            }
          })
        )


      oneof_select <- selectInput(oneof_id,
        label = h4(prop$title[[language]]),
        choices = oneof_options,
        selected = ""
      )

      # Create a uiOutput for nested properties
      nested_properties_id <- ns(paste0(prop_name, "_nested"))
      nested_properties <- uiOutput(nested_properties_id)

      div(
        oneof_select,
        nested_properties
      )
    } else if (prop$type == "array" && !is.null(prop$items) && prop$items$type == "object") {
      # Handle array of objects
      array_title <- h4(prop$title[[language]])
      array_items <- create_properties_ui(prop$items$properties, ns, language)
      div(
        array_title,
        div(class = "array-items", array_items)
      )
    } else if (prop$type == "object" && !is.null(prop$properties)) {
      # Handle nested objects
      object_title <- h4(prop$title[[language]])
      object_properties <- create_properties_ui(prop$properties, ns, language)
      div(
        object_title,
        div(class = "object-properties", object_properties)
      )
    } else {
      # Create individual widget for other property types
      div(
        create_widget(prop, ns, language, oneof_id, oneof_options)
      )
    }
  })
}


#' Create UI elements for oneOf sections
#'
#' This function generates UI elements for the oneOf sections in the schema,
#' allowing users to select between different options.
#'
#' @param oneof A list containing the oneOf options from the parsed schema
#' @param ns A namespace function for Shiny module compatibility
#' @param language The current language code (e.g., "en" for English)
#'
#' @return A tagList containing UI elements for the oneOf section
#'
#' @details
#' The function creates a select input for choosing between oneOf options and
#' generates conditional panels for each option's properties. It uses
#' create_properties_ui() to generate UI elements for each option's properties.
#'
#' @examples
#' oneof <- parsed_schema$fertilizer$oneOf
#' oneof_ui <- create_oneof_ui(oneof, ns, "en")
create_oneof_ui <- function(oneof, ns, language = "en", parent = "oneof_select") {
  if (length(oneof) == 0) {
    return(NULL)
  }


  oneof_id <- ns(paste0(parent, "_oneof"))
  oneof_options <- c(" " = " ", sapply(seq_along(oneof), function(i) {
    option <- oneof[[i]]
    if (!is.null(option$title) && !is.null(option$title[[language]])) {
      option$title[[language]]
    } else {
      paste("Option", i)
    }
  }))

  oneof_select <- selectInput(oneof_id, label = "Select an option", choices = oneof_options)

  oneof_properties_ui <- lapply(seq_along(oneof), function(i) {
    option <- oneof[[i]]

    nested_oneof_ui <- NULL
    if (!is.null(option$oneOf)) {
      nested_oneof_ui <- create_oneof_ui(option$oneOf, ns, language, paste0(parent, "_", i))
    }
    option_properties <- create_properties_ui(option$properties, ns, language, nested_oneof_ui$id, nested_oneof_ui$options)

    conditionalPanel(
      condition = sprintf("input['%s'] === '%s'", oneof_id, option$title[[language]]),
      tagList(option_properties, nested_oneof_ui$elements)
    )
  })

  return_list <- list(elements = tagList(
    # oneof_select,
    oneof_properties_ui
  ), id = oneof_id, options = oneof_options)
}


#' Create individual widget for a property
#'
#' This function generates an appropriate Shiny input widget based on the
#' property type and attributes.
#'
#' @param element A parsed property structure
#' @param ns A namespace function for Shiny module compatibility
#' @param language The current language code (e.g., "en" for English)
#'
#' @return A tagList containing the input widget and validation element
#'
#' @details
#' The function creates different types of input widgets based on the property type:
#' - select: Creates a selectInput with choices from oneOf or choices attribute
#' - number: Creates a numericInput with min and max constraints
#' - string: Creates a textInput or textAreaInput based on UI specifications
#' It also adds a validation element for each input.
#'
#' @examples
#' element <- list(type = "number", title = list(en = "Age"), minimum = 0, maximum = 120)
#' widget <- create_widget(element, ns, "en")
create_widget <- function(element, ns = NS(NULL), language = "en", oneof_id = NULL, oneof_options = NULL) {
  if (is.null(element$type)) {
    return(NULL)
  }

  element_label <- element$title[[language]]
  element_code_name <- ns(make.names(element_label))

  input_element <- switch(element$type,
    "select" = {
      choices <- if (!is.null(element$oneOf)) {
        print(element$oneOf)
        setNames(
          sapply(element$oneOf, function(choice) choice$value),
          sapply(element$oneOf, function(choice) choice$title[[language]])
        )
      } else if (!is.null(element$choices)) {
        setNames(
          sapply(element$choices, function(choice) choice$value),
          sapply(element$choices, function(choice) choice$title[[language]])
        )
      } else {
        NULL
      }
      selectInput(
        inputId = element_code_name, label = element_label,
        choices = choices,
        selected = NULL
      )
    },
    "number" = {
      numericInput(
        inputId = element_code_name,
        label = element_label,
        value = NULL,
        min = if (!is.null(element$minimum)) element$minimum else NA,
        max = if (!is.null(element$maximum)) element$maximum else NA
      )
    },
    "string" = {
      if (!is.null(element$ui) && !is.null(element$ui$`form-type`)) {
        if (element$ui$`form-type` == "textAreaInput") {
          textAreaInput(
            inputId = element_code_name,
            label = element_label,
            value = "",
            resize = "vertical",
            placeholder = if (!is.null(element$ui$`form-placeholder`)) element$ui$`form-placeholder` else ""
          )
        } else {
          textInput(
            inputId = element_code_name,
            label = element_label,
            value = "",
            placeholder = if (!is.null(element$ui$`form-placeholder`)) element$ui$`form-placeholder` else ""
          )
        }
      } else {
        textInput(
          inputId = element_code_name,
          label = element_label,
          value = "",
          placeholder = if (!is.null(element$ui$`form-placeholder`)) element$ui$`form-placeholder` else ""
        )
      }
    },
    "date" = {
      dateInput(
        inputId = element_code_name,
        label = element_label,
        value = NULL
      )
    },
    NULL # Default case for unknown types
  )

  if (!is.null(element$ui) && !is.null(element$ui$oneOf) && !is.null(oneof_id)) {
    input_element <- selectInput(oneof_id, label = element_label, choices = oneof_options)
  }

  validation_id <- paste0(element_code_name, "_validation")

  tagList(
    div(
      class = "form-group",
      input_element,
      tags$div(id = validation_id, class = "invalid-feedback")
    )
  )
}



#' Get choices for select inputs
#'
#' This function extracts and formats choices for select input widgets.
#'
#' @param element The parsed property structure
#' @param language The current language code (e.g., "en" for English)
#'
#' @return A named vector of choices for select inputs
#'
#' @details
#' The function handles two cases for select input choices:
#' 1. Choices defined directly in the element
#' 2. Choices referenced from another part of the schema (not implemented)
#'
#' @examples
#' element <- list(type = "select", choices = list(
#'   list(value = "a", title = list(en = "Option A")),
#'   list(value = "b", title = list(en = "Option B"))
#' ))
#' choices <- get_select_choices(element, "en")
get_select_choices <- function(element, language = "en") {
  if (element$type == "select") {
    if (!is.null(element$choices)) {
      # If choices are defined in the element, use them
      choices <- sapply(element$choices, function(choice) choice$value)
      names(choices) <- sapply(element$choices, function(choice) choice$title[[language]])
      return(choices)
    } else if (!is.null(element$ref)) {
      # If choices are referenced, implement logic to fetch them
      # For now, return NULL
      return(NULL)
    }
  }
  return(NULL)
}

#' Update UI element
#'
#' This function updates the value of a UI element based on its type.
#'
#' @param session The current Shiny session
#' @param element The element to update
#' @param value The new value
#' @param language The current language code (e.g., "en" for English)
#'
#' @return NULL (updates are performed via side effects)
#'
#' @details
#' The function updates different types of input widgets:
#' - select: Updates choices and selected value
#' - number: Updates numeric value
#' - string: Updates text value (for both textInput and textAreaInput)
#'
#' @examples
#' # Inside a Shiny server function:
#' update_ui_element(session, element, "new value", "en")
update_ui_element <- function(session, element, value, language = "en") {
  if (is.null(element) || is.null(element$type)) {
    return(NULL)
  }

  element_code_name <- NS("dynamic")(names(element)[1])

  # Update the UI element based on its type
  if (element$type == "select") {
    updateSelectInput(session, element_code_name, choices = get_select_choices(element, language), selected = value)
  } else if (element$type == "number") {
    updateNumericInput(session, element_code_name, value = value)
  } else if (element$type == "string" && !is.null(element$ui) && element$ui$`form-type` == "textAreaInput") {
    updateTextAreaInput(session, element_code_name, value = value)
  } else if (element$type == "string") {
    updateTextInput(session, element_code_name, value = value)
  }
}
