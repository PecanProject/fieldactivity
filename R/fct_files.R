# Functions for creating and reading the json data files containing the events
# and for managing event-related (image) files.
# Otto Kuusela 2021

# path to json file folder
json_file_base_folder <- function() golem::get_golem_options("json_file_path")

# LIFECYCLE: This URL is embedded in every persisted event JSON file as "$schema".
# When bumping the schema version or moving the schema repository, update this
# value AND consider backward-compatibility for files already written with the
# old URL.  Coordinate changes with write_json_file() below and any external
# consumers that validate events against this schema.
schema_url <- "https://raw.githubusercontent.com/hamk-uas/fieldobservatory-data-schemas/main/management-event.schema.json"

# Legacy property name mapping for backward-compatible reading
legacy_name_map <- c(
  "mgmt_event_notes" = "mgmt_event_short_notes",
  "planting_notes" = "mgmt_event_long_notes",
  "harvest_comments" = "mgmt_event_long_notes",
  "fertilizer_comments" = "mgmt_event_long_notes",
  "tillage_notes" = "mgmt_event_long_notes",
  "chemical_notes" = "mgmt_event_long_notes"
)

#' Create a folder for a site-block combination
#'
#' Given a site and a block on that site, create a folder under
#' json_file_base_folder where the events.json file and related image files will
#' be stored. If the base folder doesn't exist, the function will throw an
#' error.
#'
#' @param site The site to create the folder for
#' @param block The block to create the folder for
#' @param base_folder Included for testing reasons, the default value should
#'   otherwise be used
#'
#' @return TRUE if the directory was created successfully or already exists,
#'   FALSE otherwise.
create_file_folder <- function(site, block, 
                               base_folder = json_file_base_folder()) {
  if (!dir.exists(base_folder)) {
    stop(glue("Could not find folder {json_file_base_folder}"))
  }
  
  folder_path <- file.path(base_folder, site, block)
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  } else { TRUE }
}

#' Write a given event list to a json file
#'
#' The function will overwrite the current events.json file and replace it with
#' one generated from the supplied list of events
#'
#' @param site The site of the events
#' @param block The block of the events
#' @param event_list The list of events to write to the events.json file
#' @param base_folder Included for testing reasons, the default value should
#'   otherwise be used
write_json_file <- function(site, block, event_list, rotation_list, 
                            base_folder = json_file_base_folder()) {
  
  create_file_folder(site, block)
  
  file_path <- file.path(base_folder, site, block, "events.json")
  
  if (length(event_list) > 0) {
    for (i in 1:length(event_list)) {
      event_list[[i]]$block <- NULL
      
      # Add $schema field
      event_list[[i]][["$schema"]] <- schema_url
      
      ##### EXCEPTIONS
      event <- event_list[[i]]
      
      # if the event type is fertilizer application and the fertilizer
      # type is organic, change mgmt_operations_event to organic_material
      # to conform to the ICASA standard
      if (identical(event$mgmt_operations_event, "fertilizer") &&
          identical(event$fertilizer_type, "fertilizer_type_organic")) {
        event_list[[i]]$mgmt_operations_event <- "organic_material"    
      }
      
      #####
    }
  }
  
  if (length(rotation_list) > 0) {
    for (j in 1:length(rotation_list)) {
      rotation_list[[j]]$block <- NULL
    }
  }
  
  experiment <- list()
  experiment$management <- list()
  experiment$management$rotation <- rotation_list
  experiment$management$events <- event_list

  jsonlite::write_json(experiment, path = file_path, pretty = TRUE, 
                       null = "list", auto_unbox = TRUE)
}

#' Read the events from the events.json file
#' 
#' Reads the events from the events.json file specific to this site and block
#' combination and returns as a list of events. Applies backward-compatible
#' normalization for legacy events.
#' 
#' @param site The site to read from
#' @param block The block to read from
#' @param base_folder Included for testing reasons, the default value should
#'   otherwise be used
#'   
#' @return A list with $events and $rotation components.
read_json_file <- function(site, block, 
                           base_folder = json_file_base_folder()) {
  
  file_path <- file.path(base_folder, site, block, "events.json")
  
  if (!file.exists(file_path)) {
    return(list())
  }
  
  management <- NULL
  
  events <- jsonlite::fromJSON(file_path, 
                               simplifyDataFrame = FALSE)$management$events
  
  rotation <- jsonlite::fromJSON(file_path, 
                                  simplifyDataFrame = FALSE)$management$rotation
  
  if (length(events) == 0) {
    return(list())
  }
  
  for (i in 1:length(events)) {
    events[[i]]$block <- block
    
    ##### EXCEPTIONS
    
    # if mgmt_operations_event is organic_material, change it to fertilizer
    if (identical(events[[i]]$mgmt_operations_event, "organic_material")) {
      events[[i]]$mgmt_operations_event <- "fertilizer"
    }
    
    # Normalize legacy property names
    events[[i]] <- normalize_legacy_event(events[[i]])
    
    #####
  }
  
  if (length(rotation) != 0){
    for (j in 1:length(rotation)) {
      rotation[[j]]$block <- block
    }
  }
  
  management$events <- events
  management$rotation <- rotation
  
  return(management)
}

#' Normalize a legacy event to use schema property names
#' @param event An event list
#' @return The event with legacy names mapped to schema names
normalize_legacy_event <- function(event) {
  for (old_name in names(legacy_name_map)) {
    new_name <- legacy_name_map[[old_name]]
    if (!is.null(event[[old_name]]) && is.null(event[[new_name]])) {
      event[[new_name]] <- event[[old_name]]
      event[[old_name]] <- NULL
    }
  }
  event
}

#' Copy a file related to an event and name it appropriately
#'
#' When a file (image) is uploaded through a fileInput widget, it is saved to a
#' temporary folder. This function copies that file to an appropriate directory
#' and name. The file does not have to be originally in a temporary folder —
#' any file path is valid. This allows the function to also be used when cloning
#' an event and its associated images need to be duplicated.
#'
#' @details The new file name has the format
#'   `yyyy-mm-dd_site_block_variable_name_#.ext` where `#` is an incrementing
#'   number (0, 1, 2, ...) to ensure uniqueness within the target folder.
#'
#' @param orig_filepath The path of the file to copy
#' @param variable_name Which variable is this file for? E.g. canopeo_image
#' @param site The site where the event took place
#' @param block The block where the event took place
#' @param date The day of the event as a character string, format yyyy-mm-dd
#' @param filepath_is_relative If TRUE, json_file_base_folder will be added
#' @param delete_original Should the original file be deleted after copying?
#' @param base_folder Included for testing reasons
#' 
#' @return A path to the new location of the file relative to events.json.
#' 
#' @importFrom glue glue
copy_file <- function(orig_filepath, variable_name, site, block, date,
                      filepath_is_relative = FALSE, delete_original = FALSE,
                      base_folder = json_file_base_folder()) {
  create_file_folder(site, block)
  
  if (filepath_is_relative) {
    orig_filepath <- file.path(base_folder, orig_filepath)
  }
  
  if (!file.exists(orig_filepath)) {
    stop(glue("The file {orig_filepath} to copy does not exist"))
  }
  
  file_extension <- tolower(tools::file_ext(orig_filepath))
  allowed_extensions <- c("jpg", "jpeg", "tif", "tiff", "png")
  if (!(file_extension %in% allowed_extensions)) {
    stop("This file extension is not supported")
  }
  
  file_base <- paste(date, site, block, variable_name, sep = "_")
  filepath <- file.path(base_folder, site, block, variable_name)
  if (!dir.exists(filepath)) {
    dir.create(filepath)
  }
  
  number <- 0
  while (TRUE) {
    file_name <- paste(file_base, number, sep = "_")
    file_name <- paste(file_name, file_extension, sep = ".")
    if (!file.exists(file.path(filepath, file_name))) {
      break
    }
    number <- number + 1
    # don't loop forever
    if (number >= 1000) {
      stop("Could not find a unique name for the file")
    }
  }
  
  success <- tryCatch(expr = file.copy(from = orig_filepath, 
                                       to = file.path(filepath, file_name),
                                       copy.date = TRUE, 
                                       overwrite = FALSE),
                      warning = function(cnd) {message(cnd); FALSE},
                      error = function(cnd) {message(cnd); FALSE})

  if (success & delete_original) {
    deleted_original <- tryCatch(expr = file.remove(orig_filepath),
                                 warning = function(cnd) {message(cnd)},
                                 error = function(cnd) {message(cnd)})
  }
  
  if (success) {
    message(glue("Copied file to {file.path(filepath, file_name)}"))
    return(file.path(variable_name, file_name))
  } else {
    stop("Error in moving file")
  }
  
}

#' Delete a file
#'
#' @param filepath The path to the file which should be deleted.
#' @param filepath_relative Set to TRUE and supply site and block if filepath is
#'   relative to the events.json file.
#' @param site The site where the event took place
#' @param block The block where the event took place
#' @param base_folder Included for testing reasons
#' 
#' @importFrom glue glue
delete_file <- function(filepath, site = NULL, block = NULL, 
                        filepath_relative = FALSE, 
                        base_folder = json_file_base_folder()) {
  if (filepath_relative) {
    filepath <- file.path(base_folder, site, block, filepath)
  }
  
  if (file.exists(filepath)) {
    file.remove(filepath)
    message(glue("Deleted file {filepath}"))
  } else {
    stop(glue("Could not delete file {filepath} because it was not found"))
  }
}
