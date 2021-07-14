# Functions for creating and reading the json data files containing the events
# Otto Kuusela 2021

# missing value in the ICASA standard
missingval <- "-99.0"
# relative path to json file folder
json_file_base_folder <- "data/management_events"
#json_file_base_folder <- "/data/fo-event-files"

# create_json_file <- function(file_path) {
#     
#     # if the events directory (stored in json_file_base_folder) doesn't exist,
#     # stop
#     if (!file.exists(json_file_base_folder)) {
#         stop(glue("Could not find folder {json_file_base_folder}"))
#         #dir.create(json_file_base_folder, recursive = TRUE)
#     }
#     
#     if (!file.exists(file_path))
#     
#     # create structure with no events
#     experiment <- list()
#     experiment$management <- list()
#     experiment$management$events <- list()
#     
#     # create file
#     jsonlite::write_json(experiment, path = file_path, pretty = TRUE, 
#                          null = 'list', auto_unbox = TRUE)
#     
# }

create_file_folder <- function(site, block) {
    # if the events directory (stored in json_file_base_folder) doesn't exist,
    # stop
    if (!dir.exists(json_file_base_folder)) {
        stop(glue("Could not find folder {json_file_base_folder}"))
    }
    
    folder_path <- file.path(json_file_base_folder, site, block)
    if (!dir.exists(folder_path)) {
        dir.create(folder_path, recursive = TRUE)
    }
}

# write a given event list to a json file, overwriting everything in it
write_json_file <- function(site, block, new_list) {
    
    # this ensures that the folder to store this file exists
    create_file_folder(site, block)
    
    file_path <- file.path(json_file_base_folder, site, block, "events.json")

    # create appropriate structure
    experiment <- list()
    experiment$management <- list()
    experiment$management$events <- new_list
    
    # erase block information in each event if there are any events in the list
    if (length(experiment$management$events) > 0) {
        for (i in 1:length(experiment$management$events)) {
            experiment$management$events[[i]]$block <- NULL
        }
    }
    
    # create file
    jsonlite::write_json(experiment, path = file_path, pretty = TRUE, 
                         null = "list", auto_unbox = TRUE)
}

# retrieve the events of a specific site and block and return as a NESTED LIST.
# this retrieves the events in the same "format" as they will be saved back
# later, i.e. with code names, "-99.0" for missing values etc.
# the only difference is the block value, which will be removed when saving
# back to a json file.
retrieve_json_info <- function(site, block) {
    file_path <- file.path(json_file_base_folder, site, block, "events.json")
    
    # if file doesn't exist or given names are empty, can't read it
    if (!file.exists(file_path) | site == "" | block == "") {
        return(list())
    }
    
    events <- jsonlite::fromJSON(file_path, 
                                 simplifyDataFrame = FALSE)$management$events
    
    # if there are no events, return an empty list
    if (length(events) == 0) {
        return(list())
    }
    
    # add block information to each event
    for (i in 1:length(events)) {
        events[[i]]$block <- block
    }
    
    return(events)
}

# date needs to be in yyyy-mm-dd format!
# TODO: what if there are multiple imagges uploaded on the same day?
move_uploaded_file <- function(tmp_filepath, variable_name, site, block, date) {
    # ensures the folder for this site-block combo is there
    create_file_folder(site, block)
    
    # base of the new file
    file_base <- paste(date, site, block, variable_name, sep = "_")
    file_base <- paste(file_base, tools::file_ext(tmp_filepath), sep = ".")
    
    tmp_file_base <- basename(tmp_filepath)
    
    relative_path <- file.path(site, block, variable_name)
    filepath <- file.path(json_file_base_folder, relative_path)
    if (!dir.exists(filepath)) {
        dir.create(filepath)
    }
    
    file.copy(from = tmp_filepath, to = filepath, copy.date = TRUE, 
              overwrite = TRUE)
    file.rename(file.path(filepath, tmp_file_base), 
                file.path(filepath, file_base))
    
    #message(glue("new filepath is {file.path(variable_name, file_base)}"))
    
    return(file.path(variable_name, file_base))
}