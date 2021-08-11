#' Turn a list of events into a data frame
#'
#' Takes a list of events and makes a data frame with given
#'   variables in columns. Also adds a column with the complete event list and a
#'   final column for ordering the list by date.
#'   
#' @param events The list of events to turn into a data frame
#' @param variable_names The variables which should be displayed in the columns
#'   of the data frame.
#'
#' @return A data frame with the events as rows and variable names as columns.
#'
#' @note The function doesn't replace code names with display names. That is
#'   done separately so that when the app language is switched, we can change
#'   the table display names without having to create it again.
get_data_table <- function(events, variable_names) {
  # initialise table
  display_data_table <- data.frame()
  for (variable_name in variable_names) {
    # initialise the corresponding table column
    display_data_table[[variable_name]] <- list()
  }
  # the event column will hold the complete event information as a list
  display_data_table$event <- list()
  # the date_ordering column will hold dates for ordering the table
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

#' Find the index of an event in a list
#' 
#' Find the first index corresponding to the given event in a list of events.
#' An event is considered equal to another if they have exactly the same
#' variables (though not necessarily in the same order) and these variables
#' have exactly the same values.
#' 
#' @param event The event whose index to identify
#' @param event_list The list of events where event is to be found
#' 
#' @return The index if found, NULL otherwise
find_event_index <- function(event, event_list) {
  
  if (length(event_list) == 0) { return(NULL) }
  
  # sort the items in the lists to the same order (alphabetical)
  event <- event[order(names(event))]
  
  # go through all rows in the event list and check if any of them match
  for (i in 1:length(event_list)) {
    
    list_event <- event_list[[i]]
    # sort the items in this particular list event, to allow comparison
    list_event <- list_event[order(names(list_event))]
    
    if (identical(event, list_event)) {
      return(i)
    }
    
  }
  
  # We didn't find a match, so return NULL
  return(NULL)
}