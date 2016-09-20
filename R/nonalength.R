# Function to return length of vector with NA in it
#' Length of vector without 'NA'
#'
#' Returns the length of a vector (character, numeric or other) taking out the
#' \code{NA} values from it.
#'
#' @param given_vector a character, numeric or factor vector
#'
#' @examples
#'
#' v <- c(1, 3, 4)
#' length(v) == nonalength(v)
#'
#' v_na <- c(1, 3, 4)
#' length(v)
#' nonalength(v)
#' @export
nonalength <- function(given_vector) {
  return(length(na.omit(given_vector)))
}
