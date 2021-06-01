# Functions for creating and reading the json data files containing the events
# Otto Kuusela 2021

# missing value in the ICASA standard
missingval <- "-99.0"
# relative path to json file folder
json_file_folder <- "data/management_events"

date_format <- "%d/%m/%Y"

create_json_file <- function(file_path) {
  
  # if the events directory (stored in json_file_folder) doesn't exist,
  # create it
  if (!file.exists(json_file_folder)) {
    dir.create(json_file_folder, recursive = TRUE)
  }
  
  # create structure with no events
  experiment <- list()
  experiment$management <- list()
  experiment$management$events <- list()
  
  # create file
  jsonlite::write_json(experiment, path = file_path, pretty = TRUE, 
                       null = 'null', auto_unbox = TRUE)
  
}

append_to_json_file <- function(site_name, block, date, activity, notes) {
  
  # corresponding file name: "sitename_block_events.json"
  file_name <- paste(site_name, block, "events.json", sep = "_")
  file_path <- file.path(json_file_folder, file_name)
  
  # if file doesn't exist, create it
  if (!file.exists(file_path)) {
    create_json_file(file_path)
  }
  
  experiment <- jsonlite::fromJSON(file_path)
  
  # replaces the notes with missingval if the value is not supplied,
  # otherwise stays the same
  notes <- replace_missing_value(notes)
  
  # create a new “row” to be added to experiment$management$events
  new_event <- data.frame(mgmt_operations_event = activity,
                          mgmt_event_date = date,
                          mgmt_event_notes = notes)
  
  # add new event
  experiment$management$events <- rbind(experiment$management$events, new_event)
  
  # save changes
  jsonlite::write_json(experiment, path = file_path, pretty = TRUE, 
                       null = 'null', auto_unbox = TRUE)
}

# write a given data frame to a json file, overwriting everything in it
# this is used when editing previously entered entries
write_json_file <- function(site_name, block, new_data_frame) {
  # corresponding file name: "sitename_block_events.json"
  file_name <- paste(site_name, block, "events.json", sep = "_")
  file_path <- file.path(json_file_folder, file_name)
  
  # if file doesn't exist, create it
  if (!file.exists(file_path)) {
    # this could happen when an entry's block is edited to be one that 
    # does not yet have entries
    create_json_file(file_path)
  }
  
  # create appropriate structure
  experiment <- list()
  experiment$management <- list()
  experiment$management$events <- new_data_frame

  # if the notes are not supplied, replace with missing value
  experiment$management$events$mgmt_event_notes <- 
    sapply(experiment$management$events$mgmt_event_notes, 
           FUN = replace_missing_value)
  
  # create file
  jsonlite::write_json(experiment, path = file_path, pretty = TRUE, 
                       null = 'null', auto_unbox = TRUE)
}

# retrieve the events of a specific site and block and return as a data frame
# if language is set to NULL, code_names will be displayed and column for
# ordering by date omitted
retrieve_json_info <- function(site_name, block, language) {
  
  # corresponding file name: "sitename_block_events.json"
  file_name <- paste(site_name, block, "events.json", sep = "_")
  file_path <- file.path(json_file_folder, file_name)
  
  # if file doesn't exist or given names are empty, can't read it
  if (!file.exists(file_path) | site_name == "" | block == "") {
    return(data.frame())
  }
  
  events <- jsonlite::fromJSON(file_path)$management$events
  
  # if there are no rows, return an empty data frame
  if (is.null(nrow(events))) {
    return(data.frame())
  }
  
  # if requested, add display names and ordering by date column
  if (!is.null(language)) {
    
    events <- replace_with_display_names(events, language)
    
    # add a new column for ordering by date (this will be hidden in the table)
    events$date_ordering <- as.Date(events$mgmt_event_date, format = date_format)
    
    # swap code names for display names in activity types
    # the get_disp_name function is defined in display_name_helpers.R
    #events$mgmt_operations_event <- 
    #  sapply(events$mgmt_operations_event, 
    #         FUN = get_disp_name, 
    #         language = language)
  
  }
  
  # replace missingvals with ""
  events$mgmt_event_notes <- 
    sapply(events$mgmt_event_notes, function(x) ifelse(x == missingval, "", x))
  
  return(events)
  
}

# replace code names with display names in an event data frame
# this only applies to values coming from selectInputs (for now)
replace_with_display_names <- function(events_with_code_names, language) {
  
  events_with_display_names <- events_with_code_names
  
  for (variable_name in names(events_with_code_names)) {
    
    # determine if variable_name corresponds to a selectInput element
    element <- rlapply(
      structure,
      code_name_checker,
      code_name = variable_name)
    
    if (!element$type == "selectInput") next
    
    events_with_display_names[[variable_name]] <- 
      sapply(events_with_code_names[[variable_name]], 
             FUN = get_disp_name, 
             language = language)
    
  }
  
  return(events_with_display_names)
}

# if no value is supplied, replace with missing value
# trimws removes whitespace around the string
replace_missing_value <- function(value) {
  if (trimws(value) == "") {
    value <- missingval
  }
  return(value)
}