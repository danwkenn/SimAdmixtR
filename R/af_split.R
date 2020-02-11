#' Internal function used for reading in allele frequency file.
#'
#' Converts a string to vector of frequencies. e.g. "0.01/0.89" to c(0.01,0.89,0.1)
#' @param x character string containing numeric values separated by backslashes.
#' @param n_vari number of variants.
af_split <- function(x,n_vari){
  temp <- as.numeric(
    strsplit(x,"/")[[1]])
  if(length(temp) == (n_vari - 1)){
    temp <- c(temp, 1 - sum(temp))
  }
  return(temp)
}
