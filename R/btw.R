#' Operator to see if value between values of vector
#'
#' Custom operator that takes a single value or a numeric vector and returns \code{TRUE}
#' if values between minimum and maximum of a given vector.
#'
#' @usage x \%btw\% vec
#' @usage x \%sbtw\% vec
#'
#' @param x a numeric value or vector
#' @param vec a numeric vector
#'
#' @examples
#'
#' my_vec <- c(1, 4, 8)
#'
#' 1 %btw% my_vec  # returns TRUE
#' 1.3 %btw% my_vec  # returns TRUE
#' 0 %btw% my_vec  # returns FALSE
#' c(0,1.3,8, 12) %sbtw% my_vec # returns FALSE TRUE FALSE FALSE
#'
#' @export

"%btw%" <- function(x, vec) {
  (x >= min(vec, na.rm=TRUE)) & (x <= max(vec, na.rm=TRUE))
}

#' @rdname grapes-btw-grapes
#' @export

"%sbtw%"<-function (x, vec) {
  (x > min(vec, na.rm=TRUE)) & (x < max(vec, na.rm=TRUE))
}
