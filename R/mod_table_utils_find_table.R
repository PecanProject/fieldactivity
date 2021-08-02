# if a variable is in a table (e.g. planting_depth is in a table when 
# planted_crop has multiple values), return the code name of that table. 
# Otherwise return NULL.
# only_values determines which variables we seek:
# TRUE: only return the table code name if the variable is one whose
# value is entered in the table, e.g. all variables in fertilizer_element_table
# but not harvests_crop in harvest_crop_table, since harvest_crop's value is
# entered in a regular widget.
# FALSE: return table code name if variable is present in the table in any
# form, e.g. harvest_crop also returns harvest_crop_table since it is on the 
# rows of that table
get_variable_table <- function(variable_name, only_values = FALSE) {
  
  for (table_code_name in data_table_code_names) {
    table <- structure_lookup_list[[table_code_name]]
    
    # determine the variable names to check if they match variable_name
    names_to_check <- if (is.null(table$columns)) {
      # if columns is not defined, the table is in custom mode (e.g. 
      # fertilizer_element_table) and all variables (stored in rows) are
      # to be checked
      unlist(table$rows)
    } else {
      # in a normal table, the “value variables” are given in the columns
      if (!only_values) {
        c(table$rows, table$columns)
      } else {
        table$columns
      }
    }
    
    if (variable_name %in% names_to_check) {
      return(table_code_name)
    }
  }
  
  return(NULL)
}