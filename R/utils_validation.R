#' Check whether the value of a dateRangeInput is valid
#'
#' @description Both dates need to be supplied for the value to be considered
#'   valid, and the start date needs to be on or before the end date
#'   
#' @param value The value of the dataRangeInput to validate
#'
#' @return TRUE if value is valid, FALSE if not
valid_dateRangeInput <- function(value) {
  date1 = value[1]
  date2 = value[2]
  isTruthy(date1) && isTruthy(date2) && date1 <= date2
}