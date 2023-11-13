# Functions for creating and reading the json data files containing the events
# and for managing event-related (image) files.
# Otto Kuusela 2021

# path to json file folder
json_file_base_folder <- function() golem::get_golem_options("json_file_path")

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
  # if the events directory (stored in json_file_base_folder) doesn't exist,
  # stop
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
  
  # this ensures that the folder to store this file exists
  create_file_folder(site, block)
  
  file_path <- file.path(base_folder, site, block, "events.json")
  
  # if there are events in the list, do the following:
  # - erase block information in each event
  # - apply other exceptions
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
  
  # If rotations on the list --> erase the block information like with events
  if (length(rotation_list) > 0) {
    for (j in 1:length(rotation_list)) {
      rotation_list[[j]]$block <- NULL
      
      rotation <- rotation_list[[j]]
    }
  }
  
  # create appropriate structure
  experiment <- list()
  experiment$management <- list()
  
  # rotation will be part of the management
  experiment$management$rotation <- rotation_list
  
  experiment$management$events <- event_list
  

  # create file
  jsonlite::write_json(experiment, path = file_path, pretty = TRUE, 
                       null = "list", auto_unbox = TRUE)
}

#' Read the events from the events.json file
#' 
#' Reads the events from the events.json file specific to this site and block
#' combination and returns as a list of events.
#' 
#' @param site The site to read from
#' @param block The block to read from
#' @param base_folder Included for testing reasons, the default value should
#'   otherwise be used
#'   
#' @return A list of events, which are themselves lists. If the corresponding
#'   file does not exist or there are no events, returns an empty list.
read_json_file <- function(site, block, 
                           base_folder = json_file_base_folder()) {
  
  file_path <- file.path(base_folder, site, block, "events.json")
  
  # if file doesn't exist or given names are empty, can't read it
  if (!file.exists(file_path)) {
    return(list())
  }
  
  management <- NULL
  
  events <- jsonlite::fromJSON(file_path, 
                               simplifyDataFrame = FALSE)$management$events
  
  rotation <- jsonlite::fromJSON(file_path, 
                                  simplifyDataFrame = FALSE)$management$rotation
  
  # if there are no events, return an empty list
  if (length(events) == 0) {
    return(list())
  }
  
  # # if there are no rotation, return an empty list
  # if (length(rotation) == 0) {
  #   return(list())
  # }
  
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
  
  # add block info for rotations
  if (length(rotation) != 0){
    for (j in 1:length(rotation)) {
      rotation[[j]]$block <- block
    }
  }
  
  # Add events and rotation as a list objects which both will be returned
  # when function is called
  management$events <- events
  management$rotation <- rotation
  
  return(management)
}

#' Copy a file related to an event and name it appropriately
#' 
#' When a file (image) is uploaded through a fileInput widget, it is saved to a
#' temporary folder. This function copies that file to an appropriate directory
#' and name. The file does not have to be originally in a temporary
#' folder, any file path is ok. Therefore this function can also be used e.g.
#' when cloning and event and the images associated with it need to be
#' duplicated.
#' @param orig_filepath The path of the file to copy
#' @param variable_name Which variable is this file for? E.g. canopeo_image
#' @param site The site where the event took place
#' @param block The block where the event took place
#' @param date The day of the event as a character string, the format must be
#'   yyyy-mm-dd
#' @param filepath_is_relative If TRUE, json_file_base_folder will be added to
#'   the beginning of filepath
#' @param delete_original Should the original file be deleted after copying?
#' @param base_folder Included for testing reasons, the default value should
#'   otherwise be used
#' 
#' @details The name will be of the format 
#' yyyy-mm-dd_site_block_variable_name_# where # is a number (0, 1, 2, ...) to
#' ensure that files have unique names. 
#' 
#' @return A path to the new location of the file relative to the events.json
#'   file.
#' 
#' @importFrom glue glue
copy_file <- function(orig_filepath, variable_name, site, block, date,
                      filepath_is_relative = FALSE, delete_original = FALSE,
                      base_folder = json_file_base_folder()) {
  # ensures the folder for this site-block combo is there
  create_file_folder(site, block)
  
  # add json_file_base_folder to filepath if requested
  if (filepath_is_relative) {
    orig_filepath <- file.path(base_folder, orig_filepath)
  }
  
  # check that the temporary file actually exists
  if (!file.exists(orig_filepath)) {
    stop(glue("The file {orig_filepath} to copy does not exist"))
  }
  
  file_extension <- tolower(tools::file_ext(orig_filepath))
  allowed_extensions <- c("jpg", "jpeg", "tif", "tiff", "png")
  # if the image format is not supported, stop
  if (!(file_extension %in% allowed_extensions)) {
    stop("This file extension is not supported")
  }
  
  # base of the new file name
  file_base <- paste(date, site, block, variable_name, sep = "_")

  # path to the final file folder
  filepath <- file.path(base_folder, site, block, variable_name)
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
#' 
#' @param filepath The path to the file which should be deleted.
#' @param filepath_relative Set to TRUE and supply site and block if filepath is
#'   relative to the events.json file. This allows the function to figure out
#'   the correct path to the file.
#' @param site The site where the event took place
#' @param block The block where the event took place
#' @param base_folder Included for testing reasons, the default value should
#'   otherwise be used
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