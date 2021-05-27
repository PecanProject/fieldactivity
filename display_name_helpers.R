# Helper functions to go from code names to pretty display names
# Otto Kuusela 2021

# read the csv file containing the display names
display_name_dict_path <- "data/display_names.csv"
display_names_dict <- read.csv(display_name_dict_path)

# find all display names belonging to a certain category
# e.g. find all activity types
# TODO: update documentation
get_category_display_names <- function(category1, language) {
  
  if (is.null(language)) {
    return("Could not find display names")
  }
  
  category_names <- subset(display_names_dict, category == category1)
  
  disp_names <- category_names[[language]]
  code_names <- category_names$code_name
  names(code_names) <- disp_names
  
  return(code_names)
}

# get the display name corresponding to a code name
get_disp_name <- function(code_name1, language) {
  
  # if language_column is null, it means our UI has not initialised yet
  if (is.null(language)) {
    return("Could not find display name")
  }
  
  return(subset(display_names_dict, code_name == code_name1)[[language]])
}
