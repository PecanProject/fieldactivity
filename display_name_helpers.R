# Helper functions to go from code names to pretty display names
# Otto Kuusela 2021

# read the csv file containing the display names
display_name_dict_path <- "data/display_names.csv"
display_names_dict <- read.csv(display_name_dict_path)

# find all display names belonging to a certain category
# e.g. find all activity types
get_category_display_names <- function(category1) {
  return(subset(display_names_dict, category == category1)$display_name)
}

# get the code name corresponding to a display name
get_code_name <- function(display_name1) {
  return(subset(display_names_dict, display_name == display_name1)$code_name)
}

# get the display name corresponding to a code name
get_disp_name <- function(code_name1) {
  return(subset(display_names_dict, code_name == code_name1)$display_name)
}
