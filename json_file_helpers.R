# Functions for creating and reading the json data files containing the events
# Otto Kuusela 2021

# missing value in the ICASA standard
missingval <- "-99.0"
# relative path to json file folder
#json_file_folder <- "data/management_events"
json_file_folder <- "/data/fo-event-files"

create_json_file <- function(file_path) {
    
    # if the events directory (stored in json_file_folder) doesn't exist,
    # create it
    if (!file.exists(json_file_folder)) {
        stop(glue("Could not find folder {json_file_folder}"))
        #dir.create(json_file_folder, recursive = TRUE)
    }
    
    # create structure with no events
    experiment <- list()
    experiment$management <- list()
    experiment$management$events <- list()
    
    # create file
    jsonlite::write_json(experiment, path = file_path, pretty = TRUE, 
                         null = 'null', auto_unbox = TRUE)
    
}

# write a given data frame to a json file, overwriting everything in it
# this is used when editing previously entered entries
write_json_file <- function(site_name, block, new_list) {
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
    experiment$management$events <- new_list
    
    # erase block information in each event if there are any events in the list
    if (length(experiment$management$events) > 0) {
        for (i in 1:length(experiment$management$events)) {
            experiment$management$events[[i]]$block <- NULL
        }
    }
    
    # create file
    jsonlite::write_json(experiment, path = file_path, pretty = TRUE, 
                         null = "list",
                         auto_unbox = TRUE)
}

# retrieve the events of a specific site and block and return as a NESTED LIST.
# this retrieves the events in the same "format" as they will be saved back
# later, i.e. with code names, "-99.0" for missing values etc.
# the only difference is the block column, which will be removed when saving
# back to a json file.
retrieve_json_info <- function(site_name, block) {
    
    # corresponding file name: "sitename_block_events.json"
    file_name <- paste(site_name, block, "events.json", sep = "_")
    file_path <- file.path(json_file_folder, file_name)
    
    # if file doesn't exist or given names are empty, can't read it
    if (!file.exists(file_path) | site_name == "" | block == "") {
        return(list())
    }
    
    events <- jsonlite::fromJSON(file_path, 
                                 simplifyDataFrame = FALSE)$management$events
    
    # if there are no rows, return an empty data frame
    if (length(events) == 0) {
        return(list())
    }
    
    # add block information to each event
    for (i in 1:length(events)) {
        events[[i]]$block <- block
    }
    
    return(events)
}