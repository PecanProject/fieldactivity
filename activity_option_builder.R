# Builds the option ui for different activity types
# e.g. builds additional options for the planting activity type
# Otto Kuusela 2021

structure_file_path <- "data/activity_option_structure.json"
structure <- jsonlite::fromJSON(structure_file_path)

# currently implemented for planting, in the future for all activity types
planting_options <- structure$activities$planting

# creates the ui for a list of elements in the structure file
create_ui <- function(input_list) {
  complete_panel <- wellPanel(lapply(input_list, create_element))
  
  if (!is.null(input_list$condition)) {
    complete_panel <- conditionalPanel(
      condition = input_list$condition, complete_panel)
  }

  return(complete_panel)
}

# creates the individual elements
create_element <- function(element) {
  
    # element is a string, i.e. a visibility condition for a element set
    # it has already been handled in create_ui
    if (!is.list(element)) {
      return()
    }
        
    new_element <- NULL
        
    if (element$type == "checkboxInput") {
      new_element <- checkboxInput(element$code_name, element$label)
    } else if (element$type == "selectInput") {
      new_element <- selectInput(element$code_name, element$label,
                                 element$choices)
    } else if (element$type == "helpText") {
      new_element <- helpText(element$text)
    }
    
    new_panel <- conditionalPanel(condition = element$condition, new_element)
    
    # if there are sub-elements to create, do that
    if (!is.null(element$sub_elements)) {
      return(list(new_panel, create_ui(element$sub_elements)))
    }
    
    return(new_panel)
}
