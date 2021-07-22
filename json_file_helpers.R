# Functions for creating and reading the json data files containing the events
# Otto Kuusela 2021

# missing value in the ICASA standard
#missingval <- "-99.0"
# relative path to json file folder
json_file_base_folder <- if (dev_mode) {
    "data/management_events"
} else {
    "/data/fo-event-files"
}
#json_file_base_folder <- "data/management_events"
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
write_json_file <- function(site, block, event_list) {
    
    # this ensures that the folder to store this file exists
    create_file_folder(site, block)
    
    file_path <- file.path(json_file_base_folder, site, block, "events.json")

    
    # if there are events in the list, do the following:
    # - erase block information in each event
    # - apply exceptions
    if (length(event_list) > 0) {
        for (i in 1:length(event_list)) {
            event_list[[i]]$block <- NULL
            
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
    
    # create appropriate structure
    experiment <- list()
    experiment$management <- list()
    experiment$management$events <- event_list
    
    # create file
    jsonlite::write_json(experiment, path = file_path, pretty = TRUE, 
                         null = "list", auto_unbox = TRUE)
}

# retrieve the events of a specific site and block and return as a NESTED LIST.
# this retrieves the events in the same "format" as they will be saved back
# later, i.e. with code names, "-99.0" for missing values etc.
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
    
    # add block information and apply exceptions to each event
    for (i in 1:length(events)) {
        events[[i]]$block <- block
        
        ##### EXCEPTIONS
        
        # if mgmt_operations_event is organic_material, change it to fertilizer
        if (identical(events[[i]]$mgmt_operations_event, "organic_material")) {
            events[[i]]$mgmt_operations_event <- "fertilizer"
        }
        
        #####
    }
    
    return(events)
}

# when a file is uploaded through a fileInput widget, it is saved to a temporary
# folder. This function moves that file to an appropriate directory and also
# renames it. The name will be of the format 
# yyyy-mm-dd_site_block_variable_name_# where # is a number (0, 1, 2, ...) to
# ensure that files have unique names. The date is the date of the event, and
# it needs to be in yyyy-mm-dd format!
# if filepath_is_relative is set to TRUE, the path in tmp_filepath should
# be preceded by json_file-base_folder
move_uploaded_file <- function(tmp_filepath, variable_name, site, block, date,
                               filepath_is_relative = FALSE) {
    # ensures the folder for this site-block combo is there
    create_file_folder(site, block)
    
    # modify tmp_filepath if it is relative to events.json
    if (filepath_is_relative) {
        tmp_filepath <- file.path(json_file_base_folder, tmp_filepath)
    }
    
    # check that the temporary file actually exists
    if (!file.exists(tmp_filepath)) {
        stop(glue("The file {tmp_filepath} to move does not exist"))
    }
    
    file_extension <- tools::file_ext(tmp_filepath)
    allowed_extensions <- c("jpg", "jpeg", "tif", "tiff", "png")
    # if the image format is not supported, stop
    if (!(file_extension %in% allowed_extensions)) {
        stop("This file extension is not supported")
    }
    
    # base of the new file name
    file_base <- paste(date, site, block, variable_name, sep = "_")
    
    tmp_file_name <- basename(tmp_filepath)
    
    # path to the final file folder
    filepath <- file.path(json_file_base_folder, site, block, variable_name)
    if (!dir.exists(filepath)) {
        dir.create(filepath)
    }
    
    # determine the number to add to the end of the file name to keep file names
    # in the folder unique
    number <- 0
    while (TRUE) {
        file_name <- paste(file_base, number, sep = "_")
        file_name <- paste(file_name, file_extension, sep = ".")
        if (!file.exists(file.path(filepath, file_name))) {
            # we found a unique name. It will be available in file_name after
            # the loop
            break
        }
        number <- number + 1
        
        # don't loop forever
        if (number >= 1000) {
            stop("Could not find a unique name for the file")
        }
    }
    
    # if the filepath is relative, it means the file indicated by tmp_filepath
    # is already under the json_file_base_folder, and therefore we do not need
    # to copy the file there. However, if the file is in e.g. /tmp/..., we
    # do need to copy first as directly renaming causes an error
    if (filepath_is_relative) {
        success <- tryCatch(expr = file.rename(
                                        from = tmp_filepath,
                                        to = file.path(filepath, file_name)),
                            warning = function(cnd) {message(cnd); FALSE},
                            error = function(cnd) {message(cnd); FALSE})
    } else {
        success <- tryCatch(expr = file.copy(from = tmp_filepath, 
                                             to = filepath, copy.date = TRUE, 
                                             overwrite = TRUE),
                            warning = function(cnd) {message(cnd); FALSE},
                            error = function(cnd) {message(cnd); FALSE})
        
        if (success) {
            success <- tryCatch(expr = file.rename(
                                    from = file.path(filepath, tmp_file_name), 
                                    to = file.path(filepath, file_name)),
                                warning = function(cnd) {message(cnd); FALSE},
                                error = function(cnd) {message(cnd); FALSE})
        }
    }
    
    if (success) {
        message(glue("Moved file to {file.path(filepath, file_name)}"))
        return(file.path(variable_name, file_name))
    } else {
        stop("Error in moving file")
    }
    
}

# delete the file with the path filepath. If the path is relative to the 
# events.json file, this should be indicated with filepath_relative so we can 
# figure out the correct path
delete_file <- function(filepath, 
                        site = NULL, block = NULL, filepath_relative = TRUE) {
    if (filepath_relative) {
        filepath <- file.path(json_file_base_folder, site, block, filepath)
    }
    
    if (file.exists(filepath)) {
        file.remove(filepath)
        message(glue("Deleted file {filepath}"))
    } else {
        stop(glue("Could not delete file {filepath} because it was not found"))
    }
}