#' Find the table matching a variable name
#' 
#' If a variable's value is entered in a table, return the name of that table
#' @param variable_name The name of the variable of interest
#' @return The code name of the table where the variable is entered, or NULL
#'   if not found.
get_variable_table <- function(variable_name) {
  
  for (table_code_name in data_table_code_names) {
    table <- structure_lookup_list[[table_code_name]]
    
    table_variables <- get_table_variables(table_code_name)
    
    if (variable_name %in% table_variables) {
      return(table_code_name)
    }
  }
  
  return(NULL)
}

#' Find the variables whose value can be entered through a given table
#'
#' @param table_code_name The name of the table whose variables to fetch.
#' @return A vector of variable names whose values are entered in a table.
#' @note If a table has a dynamic row group whose rows are determined by an
#'   input widget's value, that widget's variable name will not be returned even
#'   though it could be read from the list returned by the table module.
get_table_variables <- function(table_code_name) {
  structure <- structure_lookup_list[[table_code_name]]
  variables <- NULL
  
  # if a table has a dynamic row group, add the variables present on the columns
  if (!is.null(structure$columns)) {
    variables <- c(variables, structure$columns)
  }
  
  # add the variables from all static row groups
  for (row_group in structure$rows) {
    if (row_group$type == "static") {
      variables <- c(variables, row_group$variables)
    }
  }
  
  return(variables)
}

#' Determine the dynamic rows based on row variable value
#' 
#' This is used to go from the value of a variable determining the rows in a
#' dynamic row group to the rows themselves. If the row variable is a
#' selectInput, the rows equal the value, but if the row variable is a
#' numericInput, a vector of rows is generated instead
#' 
#' @return An atomic vector of rows, either option code names or numbers
get_dynamic_rows_from_value <- function(variable, value) {
  row_variable_structure <- structure_lookup_list[[variable]]
  
  if (row_variable_structure$type == "numericInput") {
    if (!isTruthy(value) || value == missingval) {
      NULL
    } else {
      1:as.integer(value)
    }
  } else if (row_variable_structure$type == "selectInput") {
    value
  }
}