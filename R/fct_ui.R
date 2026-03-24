# Builds the ui based on a json file
# e.g. builds additional options for the different activity types
# Otto Kuusela 2021

structure_file_path <- function() system.file("extdata", "ui_structure.json",
                                   package = "fieldactivity")
structure <- jsonlite::fromJSON(structure_file_path(), simplifyMatrix = FALSE)


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
  } else {
    return(NULL)
  }
  
}

#' Build lookup list for UI elements
#' @description Build a list where the names are the code names of UI elements
#' and the values are the corresponding element structures (lists) found in 
#' ui_structure.json. Now only contains app chrome elements.
#' @return The lookup list.
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
# We also need the code names of fileInput delete buttons to set up observers
# for them
text_output_code_names <- NULL
fileInput_code_names <- NULL
for (element in structure_lookup_list) {
  if (element$type == "textOutput") {
    text_output_code_names <- c(text_output_code_names, element$code_name)
  } else if (element$type == "fileInput") {
    fileInput_code_names <- c(fileInput_code_names, element$code_name)
  }
}

#' Find the choices for a selectInput given its code name
#'
#' @param selectInput_code_name The code name of the selectInput
#' @param language The language to show the options in.
#'
#' @return A vector of choices (code names). If language was supplied, the names
#'   will be the names of the vector.
get_selectInput_choices <- function(selectInput_code_name, language) {
  # the choices for a selectInput element can be stored in
  # three ways:
  # 1) the code names of the choices are given as a vector
  # 2) for site and block selectors, there is IGNORE:
  # this means that the choices should not be updated here (return NULL)
  # 3) the category name for the choices is given.
  # in the following if-statement, these are handled
  # in this same order

  element_structure <- structure_lookup_list[[selectInput_code_name]]

  if (!identical(element_structure$type, "selectInput") || 
      is.null(element_structure$choices)) {
    return(NULL)
  }
  
  if (length(element_structure$choices) > 1) {
    choices <- c("", element_structure$choices)
    names(choices) <- c("", get_disp_name(element_structure$choices,
                                          language = language))
  } else if (element_structure$choices == "IGNORE") {
    choices <- NULL
  } else {
    choices <- c(
      "",
      get_category_names(element_structure$choices,
                         language = language)
    )
  }
  
  return(choices)
}

