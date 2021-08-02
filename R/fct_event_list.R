#' event_list 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# takes a list of events and makes a data table with given variables in columns.
# also adds a column with the complete event list and a final column for
# ordering the list by date.
# The function doesn't replace code names with display names. That is done
# separately so that when the app language is switched, we can change the
# table display names without having to create it again.
get_data_table <- function(events, variable_names) {
  # initialise table
  display_data_table <- data.frame()
  for (variable_name in variable_names) {
    # get corresponding element and initialise the corresponding table column
    element <- structure_lookup_list[[variable_name]]
    display_data_table[[variable_name]] <- list()
  }
  # the event column will hold the complete event information as a list
  display_data_table$event <- list()
  # the date_ordering column will hold dates for ordering table
  display_data_table$date_ordering <- character()
  
  row_number <- 1
  for (event in events) {
    
    for (variable_name in variable_names) {
      
      value <- event[[variable_name]]
      if (is.null(value)) {
        value <- ""
      }
      # if value is a vector, it will be turned into a single string
      # when the table is converted to a table with display names
      
      display_data_table[[row_number, variable_name]] <- value
    }
    
    # double brackets allow saving a list nicely
    display_data_table[[row_number, "event"]] <- event
    display_data_table[row_number, "date_ordering"] <- 
      as.Date(event$date, format = date_format_json)
    
    row_number <- row_number + 1
  }
  
  return(display_data_table)
}