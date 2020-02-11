#' Simple internal function to create a vector of characters from a string.
#' @export
string_to_vector <- function(x) as.numeric(strsplit(x,"")[[1]])
