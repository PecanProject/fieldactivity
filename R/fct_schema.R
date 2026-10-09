# Schema interpreter for management-event JSON schema
# Parses the bundled schema and builds registries for UI generation

# Separator used in property_registry keys to namespace properties by
# event type and subtype (e.g. "crop_name___planting" or
# "soil_depth___observation___observation_type_soil").
# Property names and event/subtype const values must NOT contain this string.
REGISTRY_KEY_SEP <- "___"

schema_file_path <- function() {
  system.file("extdata", "management-event.schema.json", 
              package = "fieldactivity")
}

#' Load and parse the management-event schema
#'
#' Every property is registered once per event type (and subtype) it appears
#' in. Its registry key is also its unique input ID in the form, so a property
#' shared by several event types (e.g. mgmt_event_long_notes) gets a separate
#' widget per event type.
#' @return A list with components: raw (the full parsed schema), event_registry,
#'   property_registry, common_properties, event_type_choices
load_schema <- function() {
  raw <- jsonlite::fromJSON(schema_file_path(), simplifyVector = FALSE)
  
  defs <- raw[["$defs"]]
  common_props <- raw$properties
  one_of <- raw$oneOf
  top_required <- raw$required %||% character(0)
  
  event_registry <- list()
  property_registry <- list()
  
  # Register common properties (shared across all events)
  for (prop_name in names(common_props)) {
    if (prop_name == "$schema") next
    prop <- common_props[[prop_name]]
    prop_resolved <- resolve_property(prop, defs)
    desc <- build_property_descriptor(prop_name, prop_resolved, 
                                       required = prop_name %in% top_required,
                                       event_type = "__common__")
    property_registry[[prop_name]] <- desc
  }
  
  common_prop_names <- setdiff(names(common_props), "$schema")
  
  # Build event type choices for the discriminator selectInput
  event_type_choices <- list()
  
  for (event_def in one_of) {
    event_props <- event_def$properties
    
    # The mgmt_operations_event const identifies the event type
    event_const <- event_props$mgmt_operations_event[["const"]]
    if (is.null(event_const)) next
    
    event_titles <- extract_titles(event_def)
    event_required <- event_def$required %||% character(0)
    
    event_type_choices[[event_const]] <- event_titles
    
    # Detect nested oneOf (subtypes, e.g. fertilizer, observation)
    nested_oneof <- event_def$oneOf
    has_subtypes <- !is.null(nested_oneof) && length(nested_oneof) > 0
    
    # Find the subtype discriminator if present
    subtype_discriminator <- NULL
    subtype_registry <- list()
    
    if (has_subtypes) {
      # Find which property is the discriminator
      for (pn in names(event_props)) {
        if (pn == "mgmt_operations_event") next
        p <- event_props[[pn]]
        if (isTRUE(p[["x-ui"]]$discriminator)) {
          subtype_discriminator <- pn
          break
        }
      }
      
      for (subtype_def in nested_oneof) {
        sub_props <- subtype_def$properties
        # Find the const value for the discriminator
        sub_const <- NULL
        if (!is.null(subtype_discriminator) && 
            !is.null(sub_props[[subtype_discriminator]])) {
          sub_const <- sub_props[[subtype_discriminator]][["const"]]
        }
        if (is.null(sub_const)) next
        
        sub_titles <- extract_titles(subtype_def)
        sub_required <- subtype_def$required %||% character(0)
        
        # Register subtype-specific properties
        sub_prop_names <- character(0)
        for (spn in names(sub_props)) {
          if (spn == subtype_discriminator) next
          if (spn == "mgmt_operations_event") next
          sp <- sub_props[[spn]]
          if (!is.null(sp[["const"]]) && is.null(sp$type)) next
          sp_resolved <- resolve_property(sp, defs)
          is_req <- spn %in% sub_required || spn %in% event_required
          desc <- build_property_descriptor(spn, sp_resolved, 
                                             required = is_req,
                                             event_type = event_const)
          desc$subtype <- sub_const
          property_registry[[paste0(spn, REGISTRY_KEY_SEP, event_const, REGISTRY_KEY_SEP, sub_const)]] <- desc
          sub_prop_names <- c(sub_prop_names, spn)
        }
        
        subtype_registry[[sub_const]] <- list(
          const = sub_const,
          titles = sub_titles,
          property_names = sub_prop_names,
          required = sub_required
        )
      }
    }
    
    # Register event-level properties (excluding mgmt_operations_event const 
    # and properties that only belong to subtypes). An event can redefine a
    # common property (e.g. grazing's date is its start date); the common
    # widget is then relabelled instead of adding a second widget.
    event_prop_names <- character(0)
    common_overrides <- list()
    for (pn in names(event_props)) {
      if (pn == "mgmt_operations_event") next
      p <- event_props[[pn]]
      
      # Skip if this property has a const (it's the discriminator value itself)
      if (!is.null(p[["const"]]) && is.null(p$type)) next
      
      p_resolved <- resolve_property(p, defs)
      is_req <- pn %in% event_required
      desc <- build_property_descriptor(pn, p_resolved, 
                                         required = is_req,
                                         event_type = event_const)
      if (pn %in% common_prop_names) {
        common_overrides[[pn]] <- desc
        next
      }
      # Use a unique key to avoid collision between events sharing property names
      reg_key <- paste0(pn, REGISTRY_KEY_SEP, event_const)
      property_registry[[reg_key]] <- desc
      event_prop_names <- c(event_prop_names, pn)
    }
    
    event_registry[[event_const]] <- list(
      const = event_const,
      titles = event_titles,
      property_names = event_prop_names,
      required = event_required,
      has_subtypes = has_subtypes,
      subtype_discriminator = subtype_discriminator,
      subtype_discriminator_id = if (!is.null(subtype_discriminator)) {
        paste0(subtype_discriminator, REGISTRY_KEY_SEP, event_const)
      },
      subtypes = subtype_registry,
      common_overrides = common_overrides
    )
  }
  
  # The registry key is the property's input ID. x-ui conditions refer to
  # other properties by name, so point them at the IDs in the same event type.
  for (key in names(property_registry)) {
    desc <- property_registry[[key]]
    desc$id <- key
    if (!is.null(desc$xui$condition)) {
      desc$condition <- condition_to_field_ids(
        desc$xui$condition, property_registry, desc$event_type, desc$subtype)
    }
    property_registry[[key]] <- desc
  }

  list(
    raw = raw,
    event_registry = event_registry,
    property_registry = property_registry,
    common_properties = common_prop_names,
    event_type_choices = event_type_choices
  )
}

#' Point the input references of an x-ui condition at field IDs
#' @param condition A condition such as "input.chemical_type == 'x'"
#' @param registry The property registry
#' @param event_type Event type of the property the condition belongs to
#' @param subtype Subtype of that property (or NULL)
#' @return The condition with each input.name replaced by input.id
condition_to_field_ids <- function(condition, registry, event_type, subtype) {
  names_used <- unique(regmatches(
    condition, gregexpr("(?<=input\\.)\\w+", condition, perl = TRUE))[[1]])
  for (name in names_used) {
    key <- lookup_property_key(registry, name, event_type, subtype)
    if (is.null(key)) next
    condition <- gsub(paste0("input\\.", name, "\\b"), paste0("input.", key),
                      condition, perl = TRUE)
  }
  condition
}

#' Resolve $ref and allOf in a property definition
#' @param prop The property definition (list)
#' @param defs The $defs section from the schema
#' @return The resolved property with refs inlined
resolve_property <- function(prop, defs) {
  if (is.null(prop)) return(prop)
  
  # Handle allOf: merge titles from first element with content from resolved ref
  if (!is.null(prop$allOf)) {
    merged <- list()
    for (part in prop$allOf) {
      resolved <- resolve_ref(part, defs)
      merged[names(resolved)] <- resolved
    }
    # Carry over any top-level keys not in allOf
    for (k in names(prop)) {
      if (k != "allOf" && is.null(merged[[k]])) {
        merged[[k]] <- prop[[k]]
      }
    }
    return(merged)
  }
  
  # Handle direct $ref
  prop <- resolve_ref(prop, defs)
  
  # Recursively resolve items for arrays
  if (!is.null(prop$items) && is.list(prop$items)) {
    prop$items <- resolve_property(prop$items, defs)
    if (!is.null(prop$items$properties)) {
      for (ipn in names(prop$items$properties)) {
        prop$items$properties[[ipn]] <- resolve_property(
          prop$items$properties[[ipn]], defs)
      }
    }
  }
  
  prop
}

#' Resolve a $ref pointer within the schema $defs
#' @param obj A list that may contain a $ref key
#' @param defs The $defs section from the schema
#' @return The resolved object
resolve_ref <- function(obj, defs) {
  ref <- obj[["$ref"]]
  if (is.null(ref)) return(obj)
  
  # refs are like "#/$defs/crop_ident_ICASA"
  parts <- strsplit(sub("^#/", "", ref), "/")[[1]]
  target <- defs
  for (p in parts[-1]) {
    target <- target[[p]]
  }
  target %||% obj
}

# Titles are only used as UI labels, so they are shown in sentence case here
# rather than changing the (all lowercase) titles in the schema itself
extract_titles <- function(node) {
  list(
    en = to_sentence_case(node$title %||% ""),
    fi = to_sentence_case(node$title_fi %||% ""),
    sv = to_sentence_case(node$title_sv %||% "")
  )
}

#' Capitalise the first letter of a label
#'
#' Labels whose first word already contains capitals (e.g. "pH after the
#' application") are left as they are.
#' @param x A character string
#' @return x with its first letter in upper case
to_sentence_case <- function(x) {
  first_word <- sub(" .*", "", x)
  if (!identical(first_word, tolower(first_word))) return(x)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

#' Determine the Shiny widget type from a schema property
#' @param prop A resolved property descriptor
#' @return A string: "selectInput", "numericInput", "dateInput", etc.
determine_widget_type <- function(prop) {
  xui <- prop[["x-ui"]]
  
  if (!is.null(xui[["form-type"]])) {
    return(xui[["form-type"]])
  }
  
  # const-only properties should not be rendered
  if (!is.null(prop[["const"]]) && is.null(prop$type)) {
    return("const")
  }
  
  prop_type <- prop$type
  
  if (identical(prop_type, "array") &&
      !is.null(prop$items) && identical(prop$items$type, "object")) {
    return("dataTable")
  }
  
  if (identical(prop_type, "string")) {
    if (length(prop$oneOf) > 0) return("selectInput")
    if (isTRUE(xui$discriminator)) return("selectInput")
    if (identical(prop$format, "date")) return("dateInput")
    return("textInput")
  }

  if (identical(prop_type, "number") || identical(prop_type, "integer")) {
    return("numericInput")
  }
  
  "textInput"
}

#' Build a property descriptor for the property registry
#' @param name Property name (used as input ID)
#' @param prop Resolved property definition
#' @param required Whether this property is required
#' @param event_type The event type this property belongs to
#' @return A named list describing the property
build_property_descriptor <- function(name, prop, required, event_type) {
  xui <- prop[["x-ui"]]
  widget_type <- determine_widget_type(prop)
  
  choices <- NULL
  if (widget_type == "selectInput" && !is.null(prop$oneOf)) {
    choices <- extract_oneof_choices(prop$oneOf)
  }
  
  array_columns <- NULL
  array_items_required <- NULL
  if (widget_type == "dataTable" && !is.null(prop$items$properties)) {
    array_columns <- list()
    array_items_required <- prop$items$required %||% character(0)
    for (col_name in names(prop$items$properties)) {
      col_prop <- prop$items$properties[[col_name]]
      array_columns[[col_name]] <- build_property_descriptor(
        col_name, col_prop, 
        required = col_name %in% array_items_required,
        event_type = event_type)
    }
  }
  
  titles <- extract_titles(prop)
  
  placeholders <- NULL
  if (!is.null(xui)) {
    ph_en <- xui[["form-placeholder"]] %||% xui[["placeholder"]]
    ph_fi <- xui[["form-placeholder_fi"]] %||% xui[["placeholder_fi"]]
    ph_sv <- xui[["form-placeholder_sv"]] %||% xui[["placeholder_sv"]]
    if (!is.null(ph_en) || !is.null(ph_fi)) {
      placeholders <- list(en = ph_en %||% "", fi = ph_fi %||% "", 
                           sv = ph_sv %||% "")
    }
  }
  
  # total_of info for auto-sum fields
  total_of <- NULL
  if (!is.null(xui$total_of_list)) {
    total_of <- list(
      list_name = xui$total_of_list,
      property_name = xui$total_of_property
    )
  }
  
  list(
    name = name,
    type = widget_type,
    titles = titles,
    choices = choices,
    required = required,
    minimum = prop$minimum,
    maximum = prop$maximum,
    min_items = prop$minItems,
    placeholders = placeholders,
    total_of = total_of,
    event_type = event_type,
    is_integer = identical(prop$type, "integer"),
    is_discriminator = isTRUE(xui$discriminator),
    array_columns = array_columns,
    array_items_required = array_items_required,
    xui = xui
  )
}

#' Extract selectInput choices from a oneOf array
#' @param one_of_array A list of oneOf entries each with const and title
#' @return A list of choice descriptors with const, titles
extract_oneof_choices <- function(one_of_array) {
  choices <- list()
  for (entry in one_of_array) {
    const_val <- entry[["const"]]
    if (is.null(const_val)) next
    choices[[length(choices) + 1]] <- list(
      value = const_val,
      titles = extract_titles(entry)
    )
  }
  choices
}

#' Get title in the requested language with fallback
#' @param titles A list with keys en, fi, sv
#' @param language Language code: "en", "fi", or "sv"
#' @param fallback A fallback string if all titles are empty
#' @return The title string
schema_get_title <- function(titles, language = "en", fallback = "") {
  if (is.null(titles)) return(fallback)
  result <- titles[[language]]
  if (!is.null(result) && nchar(result) > 0) return(result)
  # fallback to English
  result <- titles[["en"]]
  if (!is.null(result) && nchar(result) > 0) return(result)
  fallback
}

#' Convert language column name to ISO code
#' @param language Either an ISO code or a display_names.csv column name
#' @return ISO language code
lang_to_iso <- function(language) {
  mapping <- c(
    "disp_name_eng" = "en",
    "disp_name_fin" = "fi",
    "disp_name_swe" = "sv",
    "en" = "en",
    "fi" = "fi",
    "sv" = "sv"
  )
  result <- mapping[language]
  if (is.na(result)) "en" else unname(result)
}

#' Build a named choice vector for selectInput from schema choices
#' @param choices A list of choice descriptors from extract_oneof_choices
#' @param language Language code or column name
#' @return A named character vector suitable for selectInput choices
schema_get_choices <- function(choices, language) {
  if (is.null(choices) || length(choices) == 0) return(NULL)
  iso <- lang_to_iso(language)
  values <- vapply(choices, function(ch) ch$value, character(1))
  labels <- vapply(choices, function(ch) {
    schema_get_title(ch$titles, iso, ch$value)
  }, character(1))
  stats::setNames(c("", values), c("", labels))
}

#' Look up a property descriptor from the registry
#' @param registry The property registry
#' @param prop_name Property name
#' @param event_type Event type const (or NULL for common)
#' @param subtype Subtype const (or NULL)
#' @return The property descriptor, or NULL
lookup_property <- function(registry, prop_name, event_type = NULL,
                            subtype = NULL) {
  key <- lookup_property_key(registry, prop_name, event_type, subtype)
  if (is.null(key)) NULL else registry[[key]]
}

#' Find the registry key (input ID) of a property
#' @inheritParams lookup_property
#' @return The most specific matching key, or NULL
lookup_property_key <- function(registry, prop_name, event_type = NULL,
                                subtype = NULL) {
  candidates <- c(
    if (!is.null(event_type) && !is.null(subtype)) {
      paste0(prop_name, REGISTRY_KEY_SEP, event_type, REGISTRY_KEY_SEP, subtype)
    },
    if (!is.null(event_type)) paste0(prop_name, REGISTRY_KEY_SEP, event_type),
    prop_name
  )
  for (key in candidates) {
    if (!is.null(registry[[key]])) return(key)
  }
  NULL
}

#' Find a property by name in any event type
#'
#' For display purposes where the event type is not known (e.g. event list
#' column headers). Returns the first matching descriptor.
#' @param schema The loaded schema (from load_schema)
#' @param prop_name Property name
#' @return The property descriptor, or NULL
find_property_by_name <- function(schema, prop_name) {
  Find(function(desc) identical(desc$name, prop_name),
       schema$property_registry)
}

#' Get properties relevant to the currently selected event/subtype
#' @param schema The loaded schema (from load_schema)
#' @param event_type The selected event type const
#' @param subtype The selected subtype const (or NULL)
#' @return A list with: common, event_props, subtype_props, all
get_relevant_properties <- function(schema, event_type, subtype = NULL) {
  common <- schema$common_properties
  
  event_entry <- schema$event_registry[[event_type]]
  event_props <- character(0)
  subtype_props <- character(0)
  
  if (!is.null(event_entry)) {
    event_props <- event_entry$property_names
    
    if (event_entry$has_subtypes && !is.null(subtype) &&
        !is.null(event_entry$subtypes[[subtype]])) {
      subtype_props <- event_entry$subtypes[[subtype]]$property_names
    }
  }
  
  list(
    common = common,
    event_props = event_props,
    subtype_props = subtype_props,
    all = unique(c(common, event_props, subtype_props))
  )
}

#' Get the fields shown for the selected event type and subtype
#'
#' Common properties are not included, they have their own widgets.
#' @param schema The loaded schema (from load_schema)
#' @param event_type The selected event type const
#' @param subtype The selected subtype const (or NULL)
#' @return A list of property descriptors, event-level fields first
get_relevant_fields <- function(schema, event_type, subtype = NULL) {
  relevant <- get_relevant_properties(schema, event_type, subtype)
  fields <- lapply(c(relevant$event_props, relevant$subtype_props),
                   function(pn) {
                     lookup_property(schema$property_registry, pn, event_type,
                                     subtype)
                   })
  Filter(Negate(is.null), fields)
}
