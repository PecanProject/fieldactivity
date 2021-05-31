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
  
  # if the notes are not supplied, replace with missing value
  # trimws removes whitespace around the string
  if (trimws(notes) == "") {
    notes <- missingval
  }
  
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

# retrieve the events of a specific site and block and return as a data frame
# if language is set to NULL, code_names will be displayed
retrieve_json_info <- function(site_name, block, language) {
  
  # corresponding file name: "sitename_block_events.json"
  file_name <- paste(site_name, block, "events.json", sep = "_")
  file_path <- file.path(json_file_folder, file_name)
  
  # if file doesn't exist or given names are empty, can't read it
  if (!file.exists(file_path) | site_name == "" | block == "") {
    return(data.frame())
  }
  
  events <- jsonlite::fromJSON(file_path)$management$events
  
  # add a new column for ordering by date (this will be hidden in the table)
  events$date_ordering <- as.Date(events$mgmt_event_date, format = date_format)
  
  # swap code names for display names in activity types
  # the get_disp_name function is defined in display_name_helpers.R
  if (!is.null(language)) {
    events$mgmt_operations_event <- 
      sapply(events$mgmt_operations_event, 
             FUN = get_disp_name, 
             language = language)
  }
  
  # replace missingvals with ""
  events$mgmt_event_notes <- 
    sapply(events$mgmt_event_notes, function(x) ifelse(x == missingval, "", x))
  
  return(events)
  
}