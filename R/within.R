#' Operator to see if value between values of vector
#'
#' Custom operator that takes a value and a numeric vector, returns \code{TRUE}
#' if values between minimum and maximum of vector.
#'
#' @param x a number
#' @param vec a numeric vector
#'
#' @examples
#'
#' my_vec <- c(1, 4, 8)
#'
#' 1 %within% my_vec  # should return TRUE
#' 1.3 %within% my_vec  # should return TRUE
#' 0 %within% my_vec  # should return FALSE
#'
#' @export
"%within%" <- function(x, vec) {
  (x >= min(vec)) & (x <= max(vec))
}
