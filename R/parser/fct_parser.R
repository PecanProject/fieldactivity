#' Parse JSON Schema
#'
#' This function takes a complete JSON schema and parses it into a structured format
#' that can be used for UI generation and data validation.
#'
#' @param schema A list representing the complete JSON schema
#'
#' @return A list containing parsed schema information for each event type
#'
#' @details
#' The function iterates through each event in the schema's 'oneOf' array and
#' parses it using the `parse_event` function. The resulting structure is organized
#' by event type.
#'
#' @examples
#' schema <- jsonlite::fromJSON("schema.json", simplifyVector = FALSE)
#' parsed_schema <- parse_json_schema(schema)
parse_json_schema <- function(schema) {
  # Iterate through each event in the schema's oneOf array
  # for (event in schema) {
  #   event_type <- event$properties$mgmt_operations_event$const
  #   parsed_schema[[event_type]] <- parse_event(event, schema)
  # }

  event <- parse_event(schema, schema)

  return(event)
}


#' Parse Individual Event
#'
#' This function parses a single event from the JSON schema, including its properties
#' and any 'oneOf' sections.
#'
#' @param event A list representing an event in the schema
#' @param schema The complete JSON schema (used for reference if needed)
#'
#' @return A list containing parsed event information, including:
#'   - title: A list of titles in different languages
#'   - properties: A list of parsed properties for the event
#'   - oneOf: A list of parsed options for 'oneOf' sections (if present)
#'
#' @details
#' The function processes each property of the event and any 'oneOf' sections,
#' creating a structured representation of the event.
#'
#' @examples
#' event <- schema$oneOf[[1]]
#' parsed_event <- parse_event(event, schema)
parse_event <- function(event, schema) {
  parsed_event <- list(
    title = get_multilingual_field(event, "title"),
    properties = list(),
    oneOf = list()
  )

  # Parse each property of the event
  for (prop_name in names(event$properties)) {
    prop <- event$properties[[prop_name]]
    parsed_event$properties[[prop_name]] <- parse_property(prop, schema)
  }

  # Parse oneOf field if present
  if (!is.null(event$oneOf)) {
    parsed_event$oneOf <- lapply(event$oneOf, function(option) {
      list(
        title = get_multilingual_field(option, "title"),
        properties = lapply(option$properties, function(p) parse_property(p, schema)),
        oneOf = lapply(option$oneOf, function(p) {
          parse_event(p, schema)
        })
      )
    })
  }

  return(parsed_event)
}


#' Parse a single property from the JSON schema
#'
#' This function takes a property object from the JSON schema and processes it
#' to create a structured representation that can be used to generate UI elements.
#' It handles various property types, including the 'oneOf' case for multiple options.
#'
#' @param prop A list representing a single property from the JSON schema
#' @param schema The complete JSON schema (used for reference if needed)
#'
#' @return A list containing the parsed property information, including:
#'   - type: The data type of the property
#'   - title: A list of titles in different languages
#'   - description: A list of descriptions in different languages (if available)
#'   - enum: A list of possible values for enum types
#'   - minimum: The minimum value for numeric types (if applicable)
#'   - maximum: The maximum value for numeric types (if applicable)
#'   - oneOf: A list of parsed options for 'oneOf' properties
#'   - Other fields specific to the property type
#'
#' @details
#' The function handles various property types, including strings, numbers,
#' booleans, and the special 'oneOf' case. For 'oneOf' properties, it recursively
#' parses each option and its properties.
#'
#' @examples
#' prop <- list(
#'   type = "string",
#'   title = "Example Property",
#'   enum = c("Option 1", "Option 2")
#' )
parse_property <- function(prop, schema) {
  parsed_prop <- list(
    title = get_multilingual_field(prop, "title"),
    type = prop$type
  )

  if (!is.null(prop$format)) {
    if (prop$format == "date") {
      parsed_prop$type <- "date"
    } else if (prop$format == "url") {
      parsed_prop <- list()
      return(parsed_prop)
    }
  }

  if (!is.null(prop$allOf)) {
    # Handle allOf properties (typically used for references)
    parsed_prop$type <- "select"
    ref <- prop$allOf[[2]]$`$ref`
    if (!is.null(ref)) {
      ref_path <- strsplit(sub("^#/", "", ref), "/")[[1]]
      ref_def <- Reduce(`[[`, ref_path, schema)
      if (!is.null(ref_def$oneOf)) {
        parsed_prop$choices <- lapply(ref_def$oneOf, function(choice) {
          list(title = get_multilingual_field(choice, "title"), value = choice$const)
        })
      }
    }
  }

  if (!is.null(prop$items)) {
    # Handle array items
    parsed_prop$items <- parse_property(prop$items, schema)
  }

  if (!is.null(prop$properties)) {
    # Handle nested object properties
    parsed_prop$properties <- lapply(prop$properties, function(p) parse_property(p, schema))
  }

  if (!is.null(prop$`x-ui`)) {
    # Handle UI-specific properties
    parsed_prop$ui <- prop$`x-ui`
  }

  if (!is.null(prop$minimum)) parsed_prop$minimum <- prop$minimum
  if (!is.null(prop$maximum)) parsed_prop$maximum <- prop$maximum

  if (!is.null(prop$oneOf)) {
    parsed_prop$oneOf <- lapply(prop$oneOf, function(option) {
      parsed_option <- list(
        title = if (!is.null(option$title)) {
          get_multilingual_field(option, "title")
        } else {
          list(en = option$const, fi = option$const, sv = option$const)
        },
        value = option$const
      )

      # print(paste0("parsing oneOf for ", parsed_prop$title$en))

      # Recursively parse nested properties
      if (!is.null(option$properties)) {
        parsed_option$properties <- lapply(option$properties, function(p) parse_property(p, schema))
      }

      return(parsed_option)
    })
    parsed_prop$type <- "select"
  }


  return(parsed_prop)
}



#' Get Multilingual Field
#'
#' This function extracts multilingual values for a given field from an object.
#'
#' @param obj A list containing multilingual fields
#' @param field The base name of the field
#'
#' @return A list of multilingual values with keys 'en', 'fi', and 'sv'
#'
#' @details
#' The function looks for the base field name and its language-specific variants
#' (e.g., "field_fi" and "field_sv") in the provided object.
#'
#' @examples
#' obj <- list(
#'   title = "Example",
#'   title_fi = "Esimerkki",
#'   title_sv = "Exempel"
#' )
get_multilingual_field <- function(obj, field) {
  # Extract and return multilingual values for the given field
  list(
    en = obj[[field]],
    fi = obj[[paste0(field, "_fi")]],
    sv = obj[[paste0(field, "_sv")]]
  )
}
