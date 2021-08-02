# Functions for creating and reading the json data files containing the events
# Otto Kuusela 2021

# relative path to json file folder
json_file_base_folder <- function() golem::get_golem_options("json_file_path")

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
  if (!dir.exists(json_file_base_folder())) {
    stop(glue("Could not find folder {json_file_base_folder}"))
  }
  
  folder_path <- file.path(json_file_base_folder(), site, block)
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
}

# write a given event list to a json file, overwriting everything in it
write_json_file <- function(site, block, event_list) {
  
  # this ensures that the folder to store this file exists
  create_file_folder(site, block)
  
  file_path <- file.path(json_file_base_folder(), site, block, "events.json")
  
  
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
  
  file_path <- file.path(json_file_base_folder(), site, block, "events.json")
  
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

#' Copy a file related to an event and name it appropriately
#' 
#' When a file (image) is uploaded through a fileInput widget, it is saved to a
#' temporary folder. This function copies that file to an appropriate directory
#' and name. The file does not have to be originally in a temporary
#' folder, any file path is ok. This means this function can also be used e.g.
#' when cloning and event and the images associated with it need to be
#' duplicated.
#' @param orig_filepath The path of the file to copy
#' @param variable_name Which variable is this file for? E.g. canopeo_image
#' @param site The site where the event took place
#' @param block The block where the event took place
#' @param date The day of the event, the format must be yyyy-mm-dd
#' @param filepath_is_relative If TRUE, json_file_base_folder will be added to
#'   the beginning of filepath
#' @param delete_original Should the original file be deleted after copying?
#' @details The name will be of the format 
#' yyyy-mm-dd_site_block_variable_name_# where # is a number (0, 1, 2, ...) to
#' ensure that files have unique names. 
#' @importFrom glue glue
copy_file <- function(orig_filepath, variable_name, site, block, date,
                      filepath_is_relative = FALSE, delete_original = FALSE) {
  # ensures the folder for this site-block combo is there
  create_file_folder(site, block)
  
  # add json_file_base_folder to filepath if requested
  if (filepath_is_relative) {
    orig_filepath <- file.path(json_file_base_folder(), orig_filepath)
  }
  
  # check that the temporary file actually exists
  if (!file.exists(orig_filepath)) {
    stop(glue("The file {orig_filepath} to copy does not exist"))
  }
  
  file_extension <- tools::file_ext(orig_filepath)
  allowed_extensions <- c("jpg", "jpeg", "tif", "tiff", "png")
  # if the image format is not supported, stop
  if (!(file_extension %in% allowed_extensions)) {
    stop("This file extension is not supported")
  }
  
  # base of the new file name
  file_base <- paste(date, site, block, variable_name, sep = "_")

  # path to the final file folder
  filepath <- file.path(json_file_base_folder(), site, block, variable_name)
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
  
  success <- tryCatch(expr = file.copy(from = orig_filepath, 
                                       to = file.path(filepath, file_name),
                                       copy.date = TRUE, 
                                       overwrite = FALSE),
                      warning = function(cnd) {message(cnd); FALSE},
                      error = function(cnd) {message(cnd); FALSE})

  # if we succeeded in renaming, delete the original file if requested 
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
#' Delete the file with the path filepath. Used to delete files (images)
#' associated with events, e.g. canopeo_image
#' @param filepath The path to the file which should be deleted.
#' @param filepath_relative Set to TRUE and supply site and block if filepath is
#'   relative to the events.json file. This allows the function to figure out
#'   the correct path to the file.
#' @param site The site where the event took place
#' @param block The block where the event took place
#' @importFrom glue glue
delete_file <- function(filepath, 
                        site = NULL, block = NULL, filepath_relative = FALSE) {
  if (filepath_relative) {
    filepath <- file.path(json_file_base_folder(), site, block, filepath)
  }
  
  if (file.exists(filepath)) {
    file.remove(filepath)
    message(glue("Deleted file {filepath}"))
  } else {
    stop(glue("Could not delete file {filepath} because it was not found"))
  }
}