# Builds the ui based on a json file
# e.g. builds additional options for the different activity types
# Otto Kuusela 2021

#structure_file_path <- "data/ui_structure.json"
structure_file_path <- function() system.file("extdata", "ui_structure.json", 
                                   package = "fieldactivity")
structure <- jsonlite::fromJSON(structure_file_path(), simplifyMatrix = FALSE)
activity_options <- structure$sidebar$mgmt_operations_event$sub_elements


#' Recursively apply function to lists in a list
#' @description Recursively apply a function to the elements of a list that
#' are themselves lists.
#' @param x The list of lists to apply the function to
#' @param fun The function to apply to lists
#' @param name_fun Function used to name the elements of the returned list.
#' Should take a list as argument and return the name
#' @param ... arguments to pass to fun
#' @return A one-level list where each element is the value fun returns for a
#' given list in x
#' @noRd
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
    return(results)
    #return(results[[1]])
  } else {
    return(NULL)
  }
  
}

#' Build lookup list for UI elements
#' @description Build a list where the names are the code names of UI elements
#' and the values are the corresponding element structures (lists) found in 
#' ui_structure.json
#' @return The lookup list.
#' @noRd
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
  
  element_name_fetcher <- function(x) x$code_name
  
  lookup_list <- rlapply(structure, fun = element_fetcher,
                         name_fun = element_name_fetcher)
  return(lookup_list)
}

structure_lookup_list <- build_structure_lookup_list()

# help texts (technically textOutputs) have a different method of updating
# when the language is changed because they are outputs rather than inputs,
# and for that we need a list of the code names of these objects.
# The same goes for data tables (excluding event table).
# We also need the code names of fileInput delete buttons to set up observers
# for them
text_output_code_names <- NULL
data_table_code_names <- NULL
fileInput_delete_code_names <- NULL
fileInput_code_names <- NULL
for (element in structure_lookup_list) {
  if (element$type == "textOutput") {
    text_output_code_names <- c(text_output_code_names, element$code_name)
  } else if (element$type == "dataTable") {
    data_table_code_names <- c(data_table_code_names, element$code_name)
  } else if (element$type == "actionButton" && !is.null(element$fileInput)) {
    fileInput_delete_code_names <- c(fileInput_delete_code_names, 
                                     element$code_name)
  } else if (element$type == "fileInput") {
    fileInput_code_names <- c(fileInput_code_names, element$code_name)
  }
}

# creates the ui for a list of elements in the structure file.
# create_border specifies whether a border should be drawn around the 
# elements in the input_list. It is typically set to false when calling
# create_ui for the entire activity_options list, and true otherwise
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
  
  return(new_elements)
}

# creates the individual elements
# the override_label and ... functionalities are used for creating elements
# in dynamic (e.g. multi-crop) data tables. Do NOT supply the label argument in
# the unnamed arguments (...)!
create_element <- function(element, override_label = NULL, 
                           override_code_name = NULL, 
                           override_value = NULL,
                           override_choices = NULL,
                           override_selected = NULL,
                           override_placeholder = NULL, ...) {
  
  # element is a string, i.e. a visibility condition for a element set
  # it has already been handled in create_ui
  if (!is.list(element)) {
    return()
  }
  
  # element is a list of elements, because it doesn't have the type
  # attribute. In that case we want to create all of the elements in that list
  if (is.null(element$type)) {
    return(create_ui(element, create_border = FALSE))
  }
  
  # the labels will be set to element$label which is a code_name, not a 
  # display_name, but this is okay as the server will update this as the
  # language changes (which also happens when the program starts)
  # the following allows overwriting the label through ...
  element_label <- element$label
  if (!is.null(override_label)) {
    element_label <- override_label
  }
  
  element_code_name <- element$code_name
  if (!is.null(override_code_name)) {
    element_code_name <- override_code_name
  }
  
  element_value <- ""
  if (!is.null(override_value)) {
    element_value <- override_value
  }
  
  element_choices <- ""
  if (!is.null(override_choices)) {
    element_choices <- override_choices
  }
  
  element_placeholder <- element$placeholder
  if (!is.null(override_placeholder)) {
    element_placeholder <- override_placeholder
  }
  
  new_element <- if (element$type == "checkboxInput") {
    checkboxInput(element_code_name, label = element_label, ...)
  } else if (element$type == "selectInput") {
    # if multiple is defined (=TRUE) then pass that to selectInput
    multiple <- ifelse(is.null(element$multiple), FALSE, TRUE)
    # we don't enter choices yet, that will be handled by the server
    selectInput(element_code_name, label = element_label, 
                choices = element_choices, multiple = multiple,
                selected = override_selected, ...)
  } else if (element$type == "textOutput") {
    if (!is.null(element$style) && element$style == "label") {
      textOutput(element_code_name, ...)
    } else {
      # these are inteded to look like helpTexts so make text gray
      tagList(
        span(textOutput(element_code_name, ...), style = "color:gray"),
        br())
    }
  } else if (element$type == "textInput") {
    textInput(inputId = element_code_name, label = element_label, 
              value = element_value, placeholder = element_placeholder, ...)
  } else if (element$type == "numericInput") {
    numericInput(inputId = element_code_name, 
                 label = element_label, 
                 min = element$min,
                 max = ifelse(is.null(element$max),NA,element$max),
                 value = element_value,
                 step = ifelse(is.null(element$step),"any",element$step),
                 ...)
  } else if (element$type == "textAreaInput") {
    textAreaInput(element_code_name, 
                  label = element_label,
                  resize = "vertical", 
                  value = element_value,
                  placeholder = element_placeholder, ...)
  } else if (element$type == "dataTable") {
    mod_table_ui(element_code_name)
  } else if (element$type == "fileInput") {
    # create the fileInput and the corresponding delete button now so that
    # they can be aligned properly
    delete_button <- structure_lookup_list[[element$delete_button]]
    div(style = "display: flex;",
        div(style="flex-grow: 1;", 
            fileInput(element_code_name, 
                      label = element_label,
                      accept = element$filetype, ...)),
        div(style="margin-left: 5px; padding-top: 26px",
            shinyjs::hidden(actionButton(delete_button$code_name,
                                         label = delete_button$label, 
                                         class = "btn-warning")))
    )
  } else if (element$type == "dateRangeInput") {
    dateRangeInput(element_code_name, 
                   label = element_label,
                   separator = "-",
                   weekstart = 1,
                   max = Sys.Date())
  } else if (element$type == "actionButton") {
    # these are always fileInput delete buttons and are handled there
  }
  
  # put the new element in a conditionalPanel. If no condition is specified,
  # the element will be visible by default
  #new_element <- conditionalPanel(condition = element$condition, new_element)
  
  # if there are sub-elements to create, do that
  if (!is.null(element$sub_elements)) {
    return(list(new_element, 
                create_ui(element$sub_elements, create_border = FALSE)))
  }
  
  return(new_element)
}

# return choices for a selectInput given its structure 
# (as read from ui_structure.json)
get_selectInput_choices <- function(element_structure, language) {
  # the choices for a selectInput element can be stored in
  # three ways: 
  # 1) the code names of the choices are given as a vector
  # 2) for site and block selectors, there is IGNORE:
  # this means that the choices should not be updated here (return NULL)
  # 3) the category name for the choices is given.
  # in the following if-statement, these are handled
  # in this same order
  if (is.null(element_structure$choices)) {
    choices <- NULL
  } else if (length(element_structure$choices) > 1) {
    choices <- c("", element_structure$choices)
    names(choices) <- c("", get_disp_name(
      element_structure$choices,
      language = language))
  } else if (element_structure$choices == "IGNORE") {
    choices <- NULL
  } else {
    # get_category_names returns both display names and 
    # code names
    choices <- c(
      "",
      get_category_names(element_structure$choices,
                         language = language)
    )
  }
  
  return(choices)
}

#' Update value, label etc. of a UI element.
#'
#' Determines the type of the element and updates its value using shiny's update
#'   functions.
#' @param session Current shiny session
#' @param code_name The code name of the UI element to update
#' @param value An atomic vector holding the desired value of the UI element. If
#'   NULL, the value of the element is not altered.
#' @param clear_value If set to TRUE, the value of the element is cleared (and
#'   any value supplied to value is ignored)
#' @param ... Additional arguments (such as label) to pass to Shiny's update-
#'   functions.
#' @importFrom glue glue
update_ui_element <- function(session, code_name, value = NULL, 
                              clear_value = FALSE, ...) {
  # find the element from the UI structure lookup list, which has been
  # generated in ui_builder.R 
  element <- structure_lookup_list[[code_name]]
  
  # didn't find the element corresponding to code_name
  # this should not happen if the element is in 
  # sidebar_ui_structure.json
  if (is.null(element$type)) {
    stop("UI element type not found, could not update")
  }
  if (!is.atomic(value)) {
    stop("The value given to update_ui_element should be an atomic vector")
  }
  
  # if value is NULL, we need to determine on a widget type basis how to 
  # clear the value. If it isn't, replace missingvals with ""
  if (!is.null(value)) {
    # replace missingvals with empty strings
    missing_indexes <- value == missingval
    if (any(missing_indexes)) {
      value[missing_indexes] <- ""
    }
  } 
  
  
  if (element$type == "selectInput") {
    # if value is a list (e.g. multiple crops selected in harvest_crop)
    # turn it into a character vector
    # if (is.list(value)) {
    #     print("List was turned to vector when updating selectInput")
    #     value <- value[[1]]
    # }
    if (clear_value) value <- ""
    # setting the selected value to NULL doesn't change the widget's value
    updateSelectInput(session, code_name, selected = value,  ...)
  } else if (element$type == "dateInput") {
    # setting value to NULL will reset the date to the current date
    value <- if (clear_value) {
      NULL
    } else {
      tryCatch(expr = as.Date(value, format = date_format_json),
               warning = function(cnd) NULL)
    }
    updateDateInput(session, code_name, value = value, ...)
  } else if (element$type == "textAreaInput") {
    if (clear_value) value <- ""
    updateTextAreaInput(session, code_name, value = value, ...)
    #} else if (element$type == "checkboxInput") {
    #    updateCheckboxInput(session, code_name, value = value, ...)
  } else if (element$type == "actionButton") {
    updateActionButton(session, code_name, ...)
  } else if (element$type == "textInput") {
    if (clear_value) value <- ""
    updateTextInput(session, code_name, value = value, ...)
  } else if (element$type == "numericInput") {
    # if we are given a non-numeric value, we don't want to start converting
    # it. Let's replace it with an empty string (the default value)
    # if (!is.numeric(value)) {value <- ""}
    if (clear_value) { value <- "" }
    updateNumericInput(session, code_name, value = value, ...)
  } else if (element$type == "dateRangeInput") {
    
    if (!is.null(value) & length(value) != 2) {
      value <- NULL
      warning(glue("Value supplied to the dateRangeInput was not of ", 
                   "length 2, resetting it"))
    }
    
    start <- if (is.null(value) | clear_value) NULL else value[1]
    end <- if (is.null(value) | clear_value) NULL else value[2]
    
    tryCatch(warning = function(cnd) {shinyjs::reset(code_name)},
             updateDateRangeInput(session, code_name, 
                                  start = start, end = end))
  } else if (element$type == "fileInput") {
    
    if (identical(value, "")) {
      value <- NULL
      clear_value <- TRUE
    }
    
    if (!is.null(value)) {
      #value <- "1 file uploaded"
      session$sendCustomMessage(type = "fileInput-value",
                                message = list(id = code_name, 
                                               value = value))
      # show file delete button
      #message(glue("Showing {element$delete_button}"))
      shinyjs::show(element$delete_button)
    }
    
    if (clear_value) { 
      # this clears the text on the widget, but not is value
      shinyjs::reset(code_name) 
      # save current value. Whenever the save button is pressed, we check
      # whether the current value then differs from this saved value. In
      # this way this is equivalent to clearing the value
      session$userData$previous_fileInput_value[[code_name]] <-
        session$input[[code_name]]
      # hide file delete button
      #message(glue("Hiding {element$delete_button}"))
      shinyjs::hide(element$delete_button)
    }
    
    if (methods::hasArg(label)) {
      label <- list(...)$label
      session$sendCustomMessage(type = "fileInput-label",
                                message = list(id = code_name,
                                               value = label))
    }
  }
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


