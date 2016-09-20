#' Head of a matrix
#'
#' This function makes it easier to show the head of a matrix object without
#' all the columns. It takes a matrix object and a number and show the
#' given number of rows and columns
#'
#' @param given_object a \code{matrix} object
#' @param n an integer number ofthe number of rows and columns to show
#'
#' @examples
#' m <- matrix(rnorm(1000), ncol = 1000)
#' mhead(m)
#'
#' @export
mhead <- function(given_object, n = 5) {
  head(given_object, n = n)[, 1:n]
}
