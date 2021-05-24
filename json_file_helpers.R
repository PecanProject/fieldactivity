# Helper functions for the management data input app
# Otto Kuusela 2021

# missing value in the ICASA standard
missingval <- "-99.0"
# relative path to json file folder
json_file_folder <- "data/management_events"


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
  
  # corresponding file name: "sitename_blocknumber_events.json"
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

retrieve_json_info <- function(site_name, block) {
  
  # corresponding file name: "sitename_blocknumber_events.json"
  file_name <- paste(site_name, block, "events.json", sep = "_")
  file_path <- file.path(json_file_folder, file_name)
  
  # if file doesn't exist or given names are empty, can't read it
  if (!file.exists(file_path) | site_name == "" | block == "") {
    return(data.frame())
  }
  
  events <- jsonlite::fromJSON(file_path)$management$events
  
  # add a 4th column for ordering by date (this will be hidden in the table)
  events$date_ordering <- as.Date(events$mgmt_event_date, format = "%d/%m/%Y")
  
  # swap code names for display names
  # the get_disp_name function is defined in display_name_helpers.R
  events$mgmt_operations_event <- 
    sapply(events$mgmt_operations_event, FUN = get_disp_name)
  
  # replace missingvals with ""
  events$mgmt_event_notes <- 
    sapply(events$mgmt_event_notes, function(x) ifelse(x == missingval, "", x))
  
  # make column names pretty
  # the last "date_ordering" is for the hidden column intended for ordering
  # the table chronologically
  colnames(events) <- c(get_category_display_names("table_col_name"),
                        "date_ordering")
  
  return(events)
  
}