# null-coalescing operator (not importing rlang just for this)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# missing value in the ICASA standard
missingval <- "-99.0"
date_format_json <- "%Y-%m-%d"
date_format_display <- "%d/%m/%Y"

# read the csv file containing the sites
# the path is wrapped inside a function because of this:
# https://developer.r-project.org/Blog/public/2019/02/14/staged-install/index.html
# see: "Paths hard-coded in R code"
sites_file_path <- function() system.file("extdata", "FOsites.csv",
                               package = "fieldactivity")
sites <- read.csv(sites_file_path())
# converts block info from csv (e.g. "[0;1]") to vectors of strings ("0" "1")
blocks_to_vector <- function(x) strsplit(substr(x, start = 2, stop = nchar(x)-1), ";")
sites$blocks <- sapply(sites$blocks, blocks_to_vector)

# options for UI languages
# languages match the names of columns in display_names.csv
# when you give a named vector as the choices for selectInput, the names
# rather than the values will be displayed
# the \U codes are UTF-8 flag emojis
languages <- c("English \U0001f1ec\U0001f1e7" = "disp_name_eng",
               "suomi \U0001f1eb\U0001f1ee" = "disp_name_fin",
               "svenska \U0001f1f8\U0001f1ea" = "disp_name_swe")
init_lang <- languages[1]

# Load and parse the management-event schema at package load time.
# Wrapped in tryCatch so the package can still load if the schema file is
# missing or malformed (e.g. during development or testing).
mgmt_schema <- tryCatch(
  load_schema(),
  error = function(e) {
    message("Failed to load management-event schema: ", conditionMessage(e))
    message("The application may not function correctly without the schema.")
    list(
      raw = list(),
      event_registry = list(),
      property_registry = list(),
      property_reverse_index = list(),
      common_properties = character(0),
      event_type_choices = list()
    )
  }
)

# whether to print debug information (short for debug print)
# set the boolean value below to FALSE to suppress prints
dp <- function() TRUE #&& golem::app_dev()
