# Functions for finding the display names of things in different languages
# Otto Kuusela 2021

# the path is wrapped inside a function because of this: 
# https://developer.r-project.org/Blog/public/2019/02/14/staged-install/index.html
# see: “Paths hard-coded in R code”
display_name_dict_path <- function() system.file("extdata", "display_names.csv", 
                                      package = "fieldactivity")
display_names_dict <- read.csv(display_name_dict_path(), comment.char = "#")

#' Find code and display names belonging to a given category
#'
#' The categories are defined in the display_names.csv file. If language is
#' undefined, only code names will be returned. If a language is also supplied,
#' then the corresponding display names are set as the names of the vector of
#' code names.
#'
#' @param category The category (e.g. "variable_name") to find the names for
#' @param language (optional) The language of the display names
#'
#' @return A vector of code names. If language was supplied, the display names
#'   corresponding to the code names will be the names of the vector.
get_category_names <- function(category, language = NULL) {
  # rename argument to make it work with subset
  category1 <- category
  
  category_names <- subset(display_names_dict, 
                           display_names_dict$category == category1)
  code_names <- category_names$code_name
  
  if (!is.null(language)) {
    disp_names <- category_names[[language]]
    names(code_names) <- disp_names
  }
  
  return(code_names)
}

#' Get the display names corresponding to given code names
#'
#' @param code_name A vector of code names to get the display names for
#' @param language The language ("disp_name_eng" or "disp_name_fin") of the
#'   resulting display names
#' @param is_variable_name If set to TRUE, then only variable names will be
#'   searched for display names. If FALSE (the default), only non-variable names
#'   will be searched.
#' @param as_names Should the display names be set as the names of the vector
#'   of code names? Default is FALSE.
#'
#' @details is_variable_name is needed because there might be clashes between
#'   the variable and non-variable code names. E.g. organic_material is both an
#'   option in mgmt_operations_event and a variable. The language names
#'   ("disp_name_eng" and "disp_name_fin") correspond to the names of the
#'   columns in the display_names.csv file.
#'
#' @return The display name(s) as a vector of character strings in the same
#'   order as the code names. If a display name is not found or language is
#'   undefined, the code name is returned. If as_names is TRUE, the display
#'   names are the names of the vector and code names are the values.
get_disp_name <- function(code_name, language = NULL, 
                          is_variable_name = FALSE, as_names = FALSE) {
  if (is.null(code_name)) return(NULL)
  if (is.null(language)) return(code_name)
  
  if (is_variable_name) {
    rows_to_check <- subset(display_names_dict, 
                            display_names_dict$category == "variable_name")
  } else {
    rows_to_check <- subset(display_names_dict, 
                            display_names_dict$category != "variable_name")
  }
  
  row_indexes <- match(code_name, rows_to_check$code_name)
  display_name <- rows_to_check[row_indexes, language]
  
  # replace missing display names with the corresponding code names
  display_name[is.na(display_name)] <- code_name[is.na(display_name)]
  display_name[display_name == missingval] <- ""
  
  if (as_names) {
    names(code_name) <- display_name
    display_name <- code_name
  }
  
  return(display_name)
}

#' Replace code names with display names in an event data frame
#'
#' Also replaces missingvals with "".
#'
#' @param events_with_code_names The data frame with code names that should be
#'   turned to display names
#' @param language The language of the display names
#'
#' @return A data frame of the same size but with entries with code names
#'   replaced with display names
replace_with_display_names <- function(events_with_code_names, language) {
  events_with_display_names <- events_with_code_names
  
  for (variable_name in names(events_with_code_names)) {
    # determine the type of element the variable corresponds to
    element <- structure_lookup_list[[variable_name]]
    
    if (is.null(element$type)) {
      # this could be e.g. the date_ordering or event column
      next
    }
    
    if (element$type == "selectInput") {
      # the pasting is done to ensure we get a nicely formatted name
      # when x is a character vector
      events_with_display_names[[variable_name]] <-
        sapply(events_with_code_names[[variable_name]],
               FUN = function(x) {
                 name <- get_disp_name(x, language = language)
                 if (length(name) > 1) {
                   name <- paste(ifelse(name=="", "-", name), 
                                 collapse = ", ")
                 }
                 name
               })
    } else if (element$type %in% 
               c("textAreaInput", "textInput", "numericInput")) {
      events_with_display_names[[variable_name]] <-
        sapply(events_with_code_names[[variable_name]],
               FUN = function(x) {
                 if (length(x) > 1) {
                   paste(ifelse(x==missingval,"-",x), 
                         collapse = ", ")
                 } else {
                   ifelse(x==missingval,"",x)
                 }
               })
    } else if (element$type %in% c("dateInput", "dateRangeInput")) {
      events_with_display_names[[variable_name]] <-
        sapply(events_with_code_names[[variable_name]], 
               FUN = function(x) { 
                 paste(format(as.Date(x, format = date_format_json), 
                              date_format_display),
                       collapse = " - ")
               })
    }
    
  }
  
  return(events_with_display_names)
}

#' This function sets the labels on the shinymanager login UI
#'
#' @param language The language which should be displayed (either
#'   "disp_name_fin" or disp_name_eng) 
#'   
# TODO: move the actual labels to display_names.csv?
set_login_language <- function(language) {
  
  # remove possible names from language vector
  language <- unname(language)

  # yes we are overwriting the English language. This is by far
  # the simplest method
  if (identical(language, "disp_name_fin")) {
    shinymanager::set_labels(
      language = "en",
      # the \U codes are UTF-8 codes for Finnish letters a and o with dots
      "Please authenticate" = "Kirjaudu sy\U00f6tt\U00e4\U00e4ksesi tapahtumia",
      "Username:" = "Sijainti",
      "Password:" = "Salasana",
      "Login" = "Kirjaudu",
      "Logout" = "Kirjaudu ulos"
    )
  } else if (identical(language, "disp_name_eng")) {
    shinymanager::set_labels(
      language = "en",
      "Please authenticate" = "Log in to enter management events",
      "Username:" = "Site",
      "Password:" = "Password",
      "Login" = "Login",
      "Logout" = "Logout"
    )
  }
}


#' UI side of the displayed texts in login page
#'
#' @param id 
#'
#' @noRd
mod_select_lan <- function(id) {
  ns <- NS(id)
  
  tagList(
    p(textOutput(ns("frontpage1")),style="text-align: justify;"),
    br(),
    readLines(system.file("user_doc", "inst_frontpage.txt", package = "fieldactivity"), warn = F)[3],
    br(),
    readLines(system.file("user_doc", "inst_frontpage.txt", package = "fieldactivity"), warn = F)[4],
    br(),
    tags$hr(style="border-color: steelblue;"),
    div(style="display: inline-block;vertical-align:middle;", textOutput(ns("frontpage2"))),
    div(style="display: inline-block;vertical-align:middle;", tags$a(href="https://github.com/Ottis1/fieldactivity/issues", target="_blank", textOutput(ns("frontpage3")))))
    #style="display: inline-block;"))
}




#' Server side of the language selected in login page
#'
#' @param id 
#' @param language This comes from the selected login language
#'
#' @noRd
mod_auth_page_server <- function(id, language) {
  
  # Create a reactive value, language is not one in this case
  i <- reactiveVal()
  #stopifnot(is.reactive(language))
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

    # Give i a value based on the login input language that has been chosen.
    i(ifelse(identical(language, "disp_name_eng"), 0, 1))

    # Outputs for login page, change the short introduction and path to github page
    output$frontpage1 <- renderText(readLines(system.file("user_doc", "inst_frontpage.txt", package = "fieldactivity"), warn = F)[1+i()])
    output$frontpage2 <- renderText(readLines(system.file("user_doc", "inst_frontpage.txt", package = "fieldactivity"), warn = F)[5+i()])
    output$frontpage3 <- renderText(ifelse(i() == 0, "here", "täällä"))
    
  })
}