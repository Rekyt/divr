# Function to return length of vector with NA in it
nonalength <- function(given_vector) {
  return(length(na.omit(given_vector)))
}
