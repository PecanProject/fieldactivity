# Helper functions to go from code names to pretty display names in the
# correct language
# Otto Kuusela 2021

# read the csv file containing the display names
display_name_dict_path <- "data/display_names.csv"
display_names_dict <- read.csv(display_name_dict_path)

# find all code names and display names belonging to a given category
# display names are set as the names, code names are the values
get_category_names <- function(category1, language) {
  
  #if (is.null(language)) {
  #  return("Could not find display names")
  #}
  
  category_names <- subset(display_names_dict, category == category1)
  
  disp_names <- category_names[[language]]
  code_names <- category_names$code_name
  names(code_names) <- disp_names
  
  return(code_names)
}

# get the display name(s) corresponding to a code name (/names)
# returns the display names(s) as a value
get_disp_name <- function(code_name1, language) {
  
  # if language_column is null, it means our UI has not initialised yet
  #if (is.null(language)) {
  #  return("Could not find display name")
  #}
  
  row_indexes <- match(code_name1, display_names_dict$code_name)
  
  return(display_names_dict[row_indexes, language])
}
