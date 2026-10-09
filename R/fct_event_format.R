# Canonical event format
#
# In memory and on disk, an event has the shape defined by
# management-event.schema.json: common and event-level properties at the top
# level, per-item properties (e.g. one entry per harvested crop) nested in the
# event's array properties (harvest_list, planting_list, ...), and empty values
# omitted.
#
# Files written before the schema was adopted use the legacy (ICASA-style)
# format. normalize_legacy_event() upgrades those events when they are read, so
# the rest of the app only sees the canonical format, and saving rewrites the
# whole events.json in it. Legacy differences handled:
# - the organic_material event type (now fertilizer with the organic subtype)
# - event notes stored under per-event-type names (see legacy_name_map)
# - per-item values stored flat on the event, either as scalars (one item) or
#   as parallel vectors (one element per item)
# - empty values stored as the ICASA missing value "-99.0"
# - the grazing period stored as a start and end date pair (grazing_period)
#   instead of date (the start date) and end_date
# Keys the schema does not know about are kept as they are.

# Legacy property name -> schema property name
legacy_name_map <- c(
  "mgmt_event_notes" = "mgmt_event_short_notes",
  "bed_prep_notes" = "mgmt_event_long_notes",
  "chemical_applic_notes" = "mgmt_event_long_notes",
  "fertilizer_notes" = "mgmt_event_long_notes",
  "grazing_notes" = "mgmt_event_long_notes",
  "harvest_comments" = "mgmt_event_long_notes",
  "irrigation_notes" = "mgmt_event_long_notes",
  "mowing_notes" = "mgmt_event_long_notes",
  "mulch_placement_notes" = "mgmt_event_long_notes",
  "mulch_removal_notes" = "mgmt_event_long_notes",
  "observation_notes" = "mgmt_event_long_notes",
  "other_notes" = "mgmt_event_long_notes",
  "planting_notes" = "mgmt_event_long_notes",
  "tillage_treatment_notes" = "mgmt_event_long_notes",
  "weeding_notes" = "mgmt_event_long_notes"
)

#' Check which elements of an atomic vector are empty
#'
#' NA, blank strings and the ICASA missing value (stored as "-99.0" or -99)
#' count as empty.
#' @param x An atomic vector
#' @return A logical vector of the same length
is_missing_value <- function(x) {
  is.na(x) | trimws(as.character(x)) %in% c("", missingval, "-99")
}

#' Recursively remove empty values from an event
#'
#' Atomic values that are entirely empty, rows (list items) that become empty
#' and lists that become empty are removed.
#' @param x An event, a list of rows or a single value
#' @return x without empty values, or NULL if nothing is left
drop_empty_values <- function(x) {
  if (is.list(x)) {
    x <- lapply(x, drop_empty_values)
    keep <- !vapply(x, is.null, logical(1))
    return(if (any(keep)) x[keep] else NULL)
  }
  if (length(x) == 0 || all(is_missing_value(x))) return(NULL)
  x
}

#' Move flat per-item values into the event's array properties
#'
#' Legacy events store item columns (e.g. harvest_crop, harvest_method) on the
#' event itself: a scalar for a single item or parallel vectors for several.
#' This zips them into the rows of the array property (e.g. harvest_list).
#' Which columns belong to which array is read from the schema.
#' @param event An event list
#' @param schema The loaded schema (from load_schema)
#' @return The event with item columns nested under array properties
lift_array_fields <- function(event, schema) {
  event_type <- event$mgmt_operations_event
  if (is.null(event_type)) return(event)
  entry <- schema$event_registry[[event_type]]
  if (is.null(entry)) return(event)

  subtype <- if (isTRUE(entry$has_subtypes)) {
    event[[entry$subtype_discriminator]]
  }
  event_level <- get_relevant_properties(schema, event_type, subtype)$all

  for (desc in get_relevant_fields(schema, event_type, subtype)) {
    if (desc$type != "dataTable" || !is.null(event[[desc$name]])) next

    # an item column that is also an event-level property stays where it is
    columns <- setdiff(intersect(names(desc$array_columns), names(event)),
                       event_level)
    if (length(columns) == 0) next

    n_items <- max(lengths(event[columns]))
    event[[desc$name]] <- lapply(seq_len(n_items), function(i) {
      lapply(event[columns], function(values) {
        if (i <= length(values)) values[[i]]
      })
    })
    event[columns] <- NULL
  }
  event
}

#' Split a legacy grazing period into start and end dates
#'
#' The start date becomes the event date if the event has none. If the event
#' date differs from the start date, grazing_period is kept so that the start
#' date is not lost.
#' @param event An event list
#' @return The event with end_date (and date) set from grazing_period
split_grazing_period <- function(event) {
  period <- unlist(event$grazing_period)
  if (length(period) != 2 || !is.null(event$end_date)) return(event)
  event$end_date <- period[[2]]
  event$date <- event$date %||% period[[1]]
  if (identical(event$date, period[[1]])) event$grazing_period <- NULL
  event
}

#' Upgrade an event read from events.json to the canonical format
#'
#' Events already in the canonical format pass through unchanged apart from
#' dropping empty values. See the top of this file for what is upgraded.
#' @param event An event list
#' @param schema The loaded schema (from load_schema)
#' @return The event in the canonical format
normalize_legacy_event <- function(event, schema = mgmt_schema) {
  if (identical(event$mgmt_operations_event, "organic_material")) {
    event$mgmt_operations_event <- "fertilizer"
  }

  event <- drop_empty_values(event)

  for (old_name in names(legacy_name_map)) {
    new_name <- legacy_name_map[[old_name]]
    # if both are set, keep the legacy key too rather than lose its value
    if (!is.null(event[[old_name]]) && is.null(event[[new_name]])) {
      event[[new_name]] <- event[[old_name]]
      event[[old_name]] <- NULL
    }
  }

  if (identical(event$mgmt_operations_event, "grazing")) {
    event <- split_grazing_period(event)
  }

  drop_empty_values(lift_array_fields(event, schema))
}

#' Flatten events into a table with one row per event
#'
#' Array properties are spread into one column per item property, holding the
#' items' values in order separated by "; " (an empty value keeps its place).
#' Vectors are joined the same way. The $schema key is left out.
#' @param events A list of events in the canonical format
#' @return A data frame of character columns
events_to_table <- function(events) {
  rows <- lapply(events, function(event) {
    event[["$schema"]] <- NULL
    row <- list()
    for (name in names(event)) {
      value <- event[[name]]
      if (!is.list(value)) {
        row[[name]] <- paste(value, collapse = "; ")
        next
      }
      columns <- unique(unlist(lapply(value, names)))
      for (column in columns) {
        cells <- vapply(value, function(item) {
          paste(item[[column]] %||% "", collapse = " ")
        }, character(1))
        # an item property with the same name as an event property gets the
        # array's name as a prefix
        col_name <- if (column %in% names(event)) {
          paste(name, column, sep = ".")
        } else {
          column
        }
        row[[col_name]] <- paste(cells, collapse = "; ")
      }
    }
    row
  })

  col_names <- unique(unlist(lapply(rows, names)))
  table <- lapply(col_names, function(col_name) {
    vapply(rows, function(row) row[[col_name]] %||% "", character(1))
  })
  names(table) <- col_names
  as.data.frame(table, stringsAsFactors = FALSE, check.names = FALSE)
}
