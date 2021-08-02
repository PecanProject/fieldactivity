#' Evalute some javascript conditions from ui_structure.json in R
#'
#' Takes a condition written in javascript notation (visibility conditions in
#' ui_structure.json) and evaluates it in R.
#'
#' @param js_condition The javascript condition to evaluate as a string
#' @return Returns either TRUE or FALSE. If the condition could not be
#'   evaluated, returns NULL.
#'
#' @note Might not be best coding practice, but works as long as the
#'   js_condition doesn't have any typos. eval(parse(...)) is dangerous if it is
#'   used directly with user input, but here that is not the case. The user has
#'   no access to the ui_structure.json file.
evaluate_condition <- function(js_condition, session) {
  
  if (is.null(js_condition) || !is.character(js_condition)) {
    return(NULL)
  }
  
  # substitute dots with dollar signs 
  # (fixed = TRUE means we don't use regex)
  condition <- gsub("input.", "session$input$", js_condition, fixed = TRUE)
  
  # if the condition relates to the length of something, modify it
  # to look like R. e.g. change "thing.length > 1" to "length(thing) > 1"
  if (stringr::str_detect(condition, ".length")) {
    length_index <- stringr::str_locate(condition, ".length")
    start <- stringr::str_sub(condition, end = length_index[,"start"]-1)
    condition <- paste0("length(", start, ")", 
                        stringr::str_sub(condition, 
                                         start = length_index[,"end"] + 1))
  }
  
  # replace 'true' with 'TRUE' and 'false' with 'FALSE'
  if (condition == "true") {
    condition <- "TRUE"
  } else if (condition == "false") {
    condition <- "FALSE"
  }
  
  #message(glue("Evaluating condition {condition}"))
  
  # parse string into an expression and evaluate it
  tryCatch(
    expr = eval(parse(text = condition)),
    error = function(cnd) {
      message(glue("Condition {condition} could not be evaluated: {cnd}"))
      NULL
    }
  )
}