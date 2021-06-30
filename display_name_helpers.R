# Helper functions to go from code names to pretty display names in the
# correct language
# Otto Kuusela 2021

# read the csv file containing the display names
display_name_dict_path <- "data/display_names.csv"
display_names_dict <- read.csv(display_name_dict_path, comment.char = "#")

date_format_json <- "%Y-%m-%d"
date_format_display <- "%d/%m/%Y"

# find all code names and display names belonging to a given category
# display names are set as the names, code names are the values
# if language is NULL, only code names are returned
get_category_names <- function(category1, language = NULL) {
    
  category_names <- subset(display_names_dict, category == category1)
  code_names <- category_names$code_name
  
  if (!is.null(language)) {
    disp_names <- category_names[[language]]
    names(code_names) <- disp_names
  }
  
  return(code_names)
}

# get the display name(s) corresponding to a code name (/names)
# returns the display names(s) as a value
# if display name is not found, the code name is returned
# if is_variable_name is set to true, then only variable names will be looked
# at, otherwise only non-variable names will be looked at. This is because
# there might be clashes between the variable and non-variable code names
# e.g. organic_material is both an option in mgmt_operations_event and a 
# variable
get_disp_name <- function(code_name1, language, is_variable_name = FALSE) {
  
  if (is_variable_name) {
      rows_to_check <- subset(display_names_dict, category == "variable_name")
  } else {
      rows_to_check <- subset(display_names_dict, 
                                !(category == "variable_name"))
  }
  
  row_indexes <- match(code_name1, rows_to_check$code_name)
  display_name <- rows_to_check[row_indexes, language]
  
  # replace missing display names with the corresponding code names
  display_name[is.na(display_name)] <- code_name1[is.na(display_name)]
  display_name[display_name == missingval] <- ""
  
  return(display_name)
}

# replace code names with display names in an event data frame
# also replaces missingvals
replace_with_display_names <- function(events_with_code_names, language) {
    events_with_display_names <- events_with_code_names
    
    for (variable_name in names(events_with_code_names)) {
        # determine the type of element the variable corresponds to
        element <- structure_lookup_list[[variable_name]]
        
        if (is.null(element$type)) {
            next
            #stop(paste("Could not find element of name",variable_name,
            #           "in sidebar_ui_structure.json file. Check it!"))
        }
        
        if (element$type == "selectInput") {
            # the pasting is done to ensure we get a nicely formatted name
            # when x is a character vector
            events_with_display_names[[variable_name]] <-
                sapply(events_with_code_names[[variable_name]],
                       FUN = function(x) {
                           name <- get_disp_name(
                               x, language = language)
                           paste(ifelse(name=="", "-", name), 
                                 collapse = ", ")})
        } else if (element$type == "textAreaInput" | 
                   element$type == "textInput" | 
                   element$type == "numericInput") {
            events_with_display_names[[variable_name]] <-
                sapply(events_with_code_names[[variable_name]],
                       FUN = function(x) {
                           if (length(x) > 1) {
                               paste(ifelse(x==missingval,"-",x), 
                                     collapse = ", ")
                           } else {
                               ifelse(x==missingval,"",x)
                           }
                       })
        } else if (element$type == "dateInput") {
            events_with_display_names[[variable_name]] <-
                sapply(events_with_code_names[[variable_name]], 
                       FUN = function(x) { 
                           format(
                               as.Date(x, format = date_format_json), 
                               date_format_display) 
                           })
        }
        
    }
    
    return(events_with_display_names)
}