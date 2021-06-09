# Functions for creating and reading the json data files containing the events
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

# generate_empty_data_frame <- function() {
#     variable_names <- get_category_names("variable_name", NULL)
#     new_table <- data.frame()
#     
#     for (variable_name in variable_names) {
#         
#         # check if column should be list, character or numeric
#         # currently list only if multiple is defined on the element in
#         # sidebar_ui_structure.json
#         #element <- rlapply(
#         #    structure,
#         #    code_name_checker,
#         #    code_name = variable_name)
#         element <- structure_lookup_list[[variable_name]]
#         
#         if (element$type == "numericInput") {
#             new_table[[variable_name]] <- numeric()
#         } else if (!is.null(element$multiple)) {
#             new_table[[variable_name]] <- list()
#         } else {
#             new_table[[variable_name]] <- character()
#         }
#     }
#     
#     return(new_table)
# }

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
    
    # sometimes the types of the columns in the events data frame is incorrect,
    # e.g. harvest_crop should be a list() type whereas if only single crops are
    # reported in the json file, it will have said column as a character type.
    # So let's convert them
    # for (variable_name in get_category_names("variable_name")) {
    #     
    #     # we'll add block later
    #     if (variable_name == "block") next
    #     
    #     element <- structure_lookup_list[[variable_name]]
    #     
    #     # if the data frame doesn't contain this variable already, add it
    #     if (is.null(events[[variable_name]])) {
    #         events[[variable_name]] <- NA #character(nrow(events))
    #     }
    #     
    #     if (element$type == "numericInput" && 
    #         !is.numeric(events[[variable_name]])) {
    #         events[[variable_name]] <- as.numeric(events[[variable_name]])
    #     } else if (!is.null(element$multiple) && 
    #                !(class(events[[variable_name]]) == "list")) {
    #         events[[variable_name]] <- as.list(events[[variable_name]])
    #     } else if (class(events[[variable_name]]) == "logical") {
    #         events[[variable_name]] <- as.character(events[[variable_name]])
    #     }
    # }
    
    # add block information to each event
    for (i in 1:length(events)) {
        events[[i]]$block <- block
    }
    
    return(events)
}