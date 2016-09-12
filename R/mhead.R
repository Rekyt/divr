# Function to see head of matrix
mhead <- function(given_object, n = 5) {
  head(given_object, n = n)[, 1:n]
}
