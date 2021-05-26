# Builds the option ui for different activity types
# e.g. builds additional options for the planting activity type
# Otto Kuusela 2021

structure_file_path <- "data/activity_option_structure.json"
structure <- jsonlite::fromJSON(structure_file_path)
activity_options <- structure$activities

# creates the ui for a list of elements in the structure file.
# create_border specifies whether a border should be drawn around the 
# elements in the input_list. It is typically set to false when calling
# create_ui for the entire structure$activities, and true otherwise
create_ui <- function(input_list, create_border) {
  new_elements <- lapply(input_list, create_element)
  
  if (create_border) {
    new_elements <- wellPanel(new_elements)
  }
  
  # if there is a visibility condition, apply it
  if (!is.null(input_list$condition)) {
    new_elements <- conditionalPanel(
      condition = input_list$condition, new_elements)
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
  
    # element is a list of elements, because it is doesn't have the type
    # attribute. In that case we want to create all of the elements
    if (is.null(element$type)) {
      return(create_ui(element, create_border = TRUE))
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
      return(list(new_panel, create_ui(element$sub_elements, create_border = TRUE)))
    }
    
    return(new_panel)
}
