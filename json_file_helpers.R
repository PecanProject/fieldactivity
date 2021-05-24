# Helper functions for the management data input app
# Otto Kuusela 2021

# TODO: a lot of error handling

# missing value in the ICASA standard
missingval <- "-99.0"
# relative path to json file folder
json_file_folder <- "data/management_events/"

create_json_file <- function(file_path) {
  
  # create structure with no events
  experiment <- list()
  experiment$management <- list()
  experiment$management$events <- list()
  
  # create file
  jsonlite::write_json(experiment, path = file_path, pretty = TRUE, 
                       null = 'null', auto_unbox = TRUE)
  
}
  
append_to_json_file <- function(site_name, block, date, activity, notes) {
  
  # corresponding file name
  file_name <- paste(site_name, block, "events.json", sep = "_")
  file_path <- paste0(json_file_folder, file_name)
  
  
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

retrieve_json_info <- function(site_name, block) {
  # corresponding file name
  file_name <- paste(site_name, block, "events.json", sep = "_")
  file_path <- paste0(json_file_folder, file_name)
  
  # if file doesn't exist or given names are empty, can't read it
  if (!file.exists(file_path) | site_name == "" | block == "") {
    return(data.frame())
  }
  
  events <- jsonlite::fromJSON(file_path)$management$events
  
  return(events)
}