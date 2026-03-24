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
#' @return A list with components: raw (the full parsed schema), event_registry,
#'   property_registry, common_properties, event_type_choices
load_schema <- function() {
  raw <- jsonlite::fromJSON(schema_file_path(), simplifyVector = FALSE)
  
  defs <- raw[["$defs"]]
  common_props <- raw$properties
  one_of <- raw$oneOf
  top_required <- if (!is.null(raw$required)) raw$required else character(0)
  
  event_registry <- list()
  property_registry <- list()
  
  # Register common properties (shared across all events)
  for (prop_name in names(common_props)) {
    if (prop_name == "$schema") next
    prop <- common_props[[prop_name]]
    prop_resolved <- resolve_property(prop, defs)
    desc <- build_property_descriptor(prop_name, prop_resolved, 
                                       required = prop_name %in% top_required,
                                       event_type = "__common__",
                                       is_array_item = FALSE)
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
    event_required <- if (!is.null(event_def$required)) {
      event_def$required
    } else {
      character(0)
    }
    
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
        xui <- p[["x-ui"]]
        if (!is.null(xui) && identical(xui$discriminator, TRUE)) {
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
        sub_required <- if (!is.null(subtype_def$required)) {
          subtype_def$required
        } else {
          character(0)
        }
        
        # Register subtype-specific properties
        sub_prop_names <- character(0)
        for (spn in names(sub_props)) {
          if (spn == subtype_discriminator) next
          if (spn == "mgmt_operations_event") next
          sp <- sub_props[[spn]]
          sp_resolved <- resolve_property(sp, defs)
          is_req <- spn %in% sub_required || spn %in% event_required
          desc <- build_property_descriptor(spn, sp_resolved, 
                                             required = is_req,
                                             event_type = event_const,
                                             is_array_item = FALSE)
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
    # and properties that only belong to subtypes)
    event_prop_names <- character(0)
    for (pn in names(event_props)) {
      if (pn == "mgmt_operations_event") next
      p <- event_props[[pn]]
      
      # Skip if this property has a const (it's the discriminator value itself)
      if (!is.null(p[["const"]]) && is.null(p$type)) next
      
      p_resolved <- resolve_property(p, defs)
      is_req <- pn %in% event_required
      desc <- build_property_descriptor(pn, p_resolved, 
                                         required = is_req,
                                         event_type = event_const,
                                         is_array_item = FALSE)
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
      subtypes = subtype_registry
    )
  }
  
  # Build reverse-lookup index: bare prop_name -> first matching registry key.
  # Avoids linear scan in find_any_property_desc().
  property_reverse_index <- list()
  for (key in names(property_registry)) {
    bare_name <- sub(paste0(REGISTRY_KEY_SEP, ".*"), "", key)
    if (is.null(property_reverse_index[[bare_name]])) {
      property_reverse_index[[bare_name]] <- key
    }
  }

  list(
    raw = raw,
    event_registry = event_registry,
    property_registry = property_registry,
    property_reverse_index = property_reverse_index,
    common_properties = common_prop_names,
    event_type_choices = event_type_choices
  )
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
      merged <- merge_lists(merged, resolved)
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
  if (is.null(target)) return(obj)
  target
}

merge_lists <- function(a, b) {
  for (k in names(b)) {
    a[[k]] <- b[[k]]
  }
  a
}

extract_titles <- function(node) {
  list(
    en = node$title %||% "",
    fi = node$title_fi %||% "",
    sv = node$title_sv %||% ""
  )
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
    if (!is.null(prop$oneOf) && length(prop$oneOf) > 0) return("selectInput")
    if (!is.null(xui) && identical(xui$discriminator, TRUE)) return("selectInput")
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
#' @param is_array_item Whether this property is inside an array items
#' @return A named list describing the property
build_property_descriptor <- function(name, prop, required, event_type, 
                                       is_array_item) {
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
    array_items_required <- if (!is.null(prop$items$required)) {
      prop$items$required
    } else {
      character(0)
    }
    for (col_name in names(prop$items$properties)) {
      col_prop <- prop$items$properties[[col_name]]
      array_columns[[col_name]] <- build_property_descriptor(
        col_name, col_prop, 
        required = col_name %in% array_items_required,
        event_type = event_type,
        is_array_item = TRUE)
    }
  }
  
  titles <- extract_titles(prop)
  
  # Unitless titles for table column headers
  unitless_titles <- NULL
  if (!is.null(xui)) {
    ut_en <- xui$unitless_title %||% xui$unitless_title2
    ut_fi <- xui$unitless_title_fi %||% xui$unitless_title2_fi
    ut_sv <- xui$unitless_title_sv %||% xui$unitless_title2_sv
    if (!is.null(ut_en) || !is.null(ut_fi) || !is.null(ut_sv)) {
      unitless_titles <- list(
        en = ut_en %||% "",
        fi = ut_fi %||% "",
        sv = ut_sv %||% ""
      )
    }
  }
  
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
    unitless_titles = unitless_titles,
    choices = choices,
    required = required,
    minimum = prop$minimum,
    maximum = prop$maximum,
    min_items = prop$minItems,
    placeholders = placeholders,
    total_of = total_of,
    event_type = event_type,
    is_array_item = is_array_item,
    is_integer = identical(prop$type, "integer"),
    is_discriminator = !is.null(xui) && identical(xui$discriminator, TRUE),
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
  lang_key <- language
  result <- titles[[lang_key]]
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
  if (is.na(result) || is.null(result)) "en" else unname(result)
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
  result <- c("", values)
  names(result) <- c("", labels)
  result
}

#' Look up a property descriptor from the registry
#' @param registry The property registry
#' @param prop_name Property name
#' @param event_type Event type const (or NULL for common)
#' @param subtype Subtype const (or NULL)
#' @return The property descriptor, or NULL
lookup_property <- function(registry, prop_name, event_type = NULL,
                            subtype = NULL) {
  # Try subtype-specific key first
  if (!is.null(subtype) && !is.null(event_type)) {
    key <- paste0(prop_name, REGISTRY_KEY_SEP, event_type, REGISTRY_KEY_SEP, subtype)
    if (!is.null(registry[[key]])) return(registry[[key]])
  }
  # Try event-specific key
  if (!is.null(event_type)) {
    key <- paste0(prop_name, REGISTRY_KEY_SEP, event_type)
    if (!is.null(registry[[key]])) return(registry[[key]])
  }
  # Try common property
  if (!is.null(registry[[prop_name]])) return(registry[[prop_name]])
  NULL
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
