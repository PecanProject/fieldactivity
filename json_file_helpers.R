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

# TODO: isn't used anymore
# append_to_json_file <- function(site_name, block, date, activity, notes) {
#   
#   # corresponding file name: "sitename_block_events.json"
#   file_name <- paste(site_name, block, "events.json", sep = "_")
#   file_path <- file.path(json_file_folder, file_name)
#   
#   # if file doesn't exist, create it
#   if (!file.exists(file_path)) {
#     create_json_file(file_path)
#   }
#   
#   experiment <- jsonlite::fromJSON(file_path)
#   
#   # replaces the notes with missingval if the value is not supplied,
#   # otherwise stays the same
#   notes <- replace_missing_value(notes)
#   
#   # create a new “row” to be added to experiment$management$events
#   new_event <- data.frame(mgmt_operations_event = activity,
#                           mgmt_event_date = date,
#                           mgmt_event_notes = notes)
#   
#   # add new event
#   experiment$management$events <- rbind(experiment$management$events, new_event)
#   
#   # save changes
#   jsonlite::write_json(experiment, path = file_path, pretty = TRUE, 
#                        null = 'null', auto_unbox = TRUE)
# }

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
  #experiment$management$events$mgmt_event_notes <- 
  #  sapply(experiment$management$events$mgmt_event_notes, 
  #         FUN = replace_missing_value)
  
  # create file
  jsonlite::write_json(experiment, path = file_path, pretty = TRUE, 
                       #null = 'null',
                       auto_unbox = TRUE)
}

generate_empty_data_frame <- function() {
    variable_names <- get_category_names("variable_name", NULL)
    new_table <- data.frame()
    
    for (variable_name in variable_names) {
        
        # check if column should be list, character or numeric
        # currently list only if multiple is defined on the element in
        # sidebar_ui_structure.json
        #element <- rlapply(
        #    structure,
        #    code_name_checker,
        #    code_name = variable_name)
        element <- structure_lookup_list[[variable_name]]
        
        if (element$type == "numericInput") {
            new_table[[variable_name]] <- numeric()
        } else if (!is.null(element$multiple)) {
            new_table[[variable_name]] <- list()
        } else {
            new_table[[variable_name]] <- character()
        }
    }
    
    return(new_table)
}

# retrieve the events of a specific site and block and return as a data frame
# this retrieves the events in the same "format" as they will be saved back
# later, i.e. with code names, "-99.0" for missing values etc. 
retrieve_json_info <- function(site_name, block) {
  
  # corresponding file name: "sitename_block_events.json"
  file_name <- paste(site_name, block, "events.json", sep = "_")
  file_path <- file.path(json_file_folder, file_name)
  
  # if file doesn't exist or given names are empty, can't read it
  if (!file.exists(file_path) | site_name == "" | block == "") {
    return(generate_empty_data_frame())
  }
  
  events <- jsonlite::fromJSON(file_path)$management$events
  
  # if there are no rows, return an empty data frame
  if (is.null(nrow(events))) {
    return(generate_empty_data_frame())
  }
  
  # sometimes the types of the columns in the events data frame is incorrect,
  # e.g. harvest_crop should be a list() type whereas if only single crops are
  # reported in the json file, it will have said column as a character type. So
  # let's convert them
  for (variable_name in get_category_names("variable_name")) {
      
      element <- structure_lookup_list[[variable_name]]
      
      # if the data frame doesn't contain this variable already, add it
      if (is.null(events[[variable_name]])) {
          events[[variable_name]] <- character(nrow(events))
      }
      
      if (element$type == "numericInput" 
          && !is.numeric(events[[variable_name]])) {
          events[[variable_name]] <- as.numeric(events[[variable_name]])
      } else if (!is.null(element$multiple) 
                 && !(class(events[[variable_name]]) == "list")) {
          events[[variable_name]] <- as.list(events[[variable_name]])
      }
  }
  
  return(events)
}

# replace code names with display names in an event data frame
# this only applies to values coming from selectInputs and textAreaInpts
# (for now)
# TODO: move to display_name_helpers.R
replace_with_display_names <-
    function(events_with_code_names, language) {
        events_with_display_names <- events_with_code_names
        
        for (variable_name in names(events_with_code_names)) {
            # determine if variable_name corresponds to a selectInput element
            #element <- rlapply(structure,
            #                   code_name_checker,
            #                   code_name = variable_name)
            element <- structure_lookup_list[[variable_name]]
            
            if (is.null(element$type)) {
                stop(paste("Could not find element of name",variable_name,
                           "in sidebar_ui_structure.json file. Check it!"))
            }
            
            if (element$type == "selectInput") {
                events_with_display_names[[variable_name]] <-
                    sapply(events_with_code_names[[variable_name]],
                           FUN = function(x) {paste(get_disp_name(
                               x, language = language), 
                               collapse = ", ")}
                    )
            } else if (element$type == "textAreaInput") {
                events_with_display_names[[variable_name]] <-
                    sapply(events_with_code_names[[variable_name]],
                           FUN = function(x) {
                               ifelse(x==missingval,"",x)})
            }
         
        }
        
        return(events_with_display_names)
    }

# if no value is supplied, replace with missing value
# trimws removes whitespace around the string
# TODO: is this used anywhere?
replace_missing_value <- function(value) {
  if (trimws(value) == "") {
    value <- missingval
  }
  return(value)
}