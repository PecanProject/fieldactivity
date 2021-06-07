# Builds the ui based on a json file
# e.g. builds additional options for the different activity types
# Otto Kuusela 2021

structure_file_path <- "data/ui_structure.json"
structure <- jsonlite::fromJSON(structure_file_path)
activity_options <- structure$sidebar$mgmt_operations_event$sub_elements

# function which recursively applies a function to the elements of a list that
# are themselves lists.
# returns the values returned by fun as a list
rlapply <- function(x, fun, name_fun = NULL, ...) {
    
    results <- list()
    
    for (element in x) {
        if (!is.list(element)) {
            next
        }
        
        # x is a list, so let's test it
        result <- fun(element, ...)
        
        if (!is.null(result)) {
            
            # if we have a naming function defined, use that
            # index is either an actual index or name of the element
            if (is.null(name_fun)) {
                index <- length(results) + 1
            } else {
                index <- name_fun(element)
            }
            
            results[[index]] <- result
        }
        
        # more results might lurk on lower levels of the list. 
        # So let's investigate those
        more_results <- rlapply(element, fun, name_fun, ...)
        
        if (length(more_results) > 0) {
            results <- append(results, more_results)
        }
    }
    
    if (length(results) > 1) {
        return(results)
    } else if (length(results) == 1) {
        return(results[[1]])
    } else {
        return(NULL)
    }
    
}

build_structure_lookup_list <- function() {
    element_fetcher <- function(x) {
        if (!is.null(x$code_name)) {
            # we don't need the sub_elements listed, those will come separately
            x$sub_elements <- NULL
            return(x)
        } else {
            return(NULL)
        }
    }
    
    element_name_fetcher <- function(x) { return(x$code_name) }
    
    lookup_list <- rlapply(structure, fun = element_fetcher,
                           name_fun = element_name_fetcher)
    return(lookup_list)
}

structure_lookup_list <- build_structure_lookup_list()

# help texts (technically textOutputs) have a different method of updating
# when the language is changed because they are outputs rather than inputs,
# and for that we need a list of the code names of these objects. 
text_output_code_names <- c("window_title", "edit_mode_title", "frontpage_text")

# creates the ui for a list of elements in the structure file.
# create_border specifies whether a border should be drawn around the 
# elements in the input_list. It is typically set to false when calling
# create_ui for the entire activity_options list, and true otherwise
create_ui <- function(input_list, language, create_border) {
  new_elements <- lapply(input_list, create_element, language = language)
  
  if (create_border) {
    new_elements <- wellPanel(new_elements)
  }
  
  # if there is a visibility condition, apply it
  if (!is.null(input_list$condition)) {
    new_elements <- conditionalPanel(
      condition = input_list$condition, new_elements)
  }

  return(new_elements)
}

# creates the individual elements
create_element <- function(element, language) {
  
    # element is a string, i.e. a visibility condition for a element set
    # it has already been handled in create_ui
    if (!is.list(element)) {
      return()
    }
  
    # element is a list of elements, because it doesn't have the type
    # attribute. In that case we want to create all of the elements in that list
    if (is.null(element$type)) {
      return(create_ui(element, language = language, create_border = TRUE))
    }
        
    new_element <- NULL
        
    # the labels will be set to element$label which is a code_name, not a 
    # display_name, but this is okay as the server will update this as the
    # language changes (which also happens when the program starts)
    
    if (element$type == "checkboxInput") {
      new_element <- checkboxInput(element$code_name, element$label)
    } else if (element$type == "selectInput") {
      # if multiple is defined (=TRUE) then pass that to selectInput
      multiple <- ifelse(is.null(element$multiple), FALSE, TRUE)
      # we don't enter choices yet, that will be handled by the server
      new_element <- selectInput(element$code_name, element$label, 
                                 choices = c(""), multiple = multiple)
    } else if (element$type == "textOutput") {
      # these are inteded to look like helpTexts so make text gray
      new_element <- span(textOutput(element$code_name), style = "color:gray")
      # add to the list to draw the text in the correct language
      # the <<- operator assigns to the global environment
      # maybe not the best programming technique, but it works
      text_output_code_names <<- c(text_output_code_names, element$code_name)
    } else if (element$type == "textInput") {
      new_element <- textInput(element$code_name, element$label)
    } else if (element$type == "numericInput") {
      new_element <- numericInput(element$code_name, 
                                  element$label, 
                                  min = element$min,
                                  value = 0)
    } else if (element$type == "textAreaInput") {
      new_element <- textAreaInput(element$code_name, 
                                   element$label,
                                   resize = "vertical")
    }
    
    # put the new element in a conditionalPanel. If no condition is specified,
    # the element will be visible by default
    new_panel <- conditionalPanel(condition = element$condition, new_element)
    
    # if there are sub-elements to create, do that
    if (!is.null(element$sub_elements)) {
      return(list(new_panel, 
                  create_ui(element$sub_elements, create_border = TRUE)))
    }
    
    return(new_panel)
}


# checks whether the list x (corresponding to a UI element) has a specified
# code name, and if yes, return it
# this function is used in app.R to find the element corresponding to a
# given code name when updating UI language
code_name_checker <- function(x, code_name) {
  if (is.null(x$code_name)) {
    return(NULL)
  }
  
  if (x$code_name == code_name) {
    return(x)
  } else {
    return(NULL)
  }
}


