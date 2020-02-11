#' Simple function to pair up the elements of two or more equal-length lists. Function works similar to zip in python.
#' @param list1 A list.
#' @param ... More lists of the same length.
#' @return A list of the same length as the original lists. Each element of the list is itself a list, containing two elements: the corresponding element of the first list, and the corresponding element of the second list.
#' The function should will also work with vectors of equal length.
#' @examples
#' listA = list(1,2,3,4)
#' listB = list("A","B","C","D")
#' stitch(listA,listB)
stitch <- function(list1,...){

  # Extract other arguments as a list of lists:
  input_list = list(...)
  #input_list <- as.list(substitute(list(...)))

  # Create additional heirarchy for the first list:
  for(i in 1:length(list1)){
    list1[[i]] <- list(list1[[i]])
  }

  # "stitch" on the elements of the other list(s):
  for(j in 1:length(list1)){
    for(i in 1:length(input_list)){
      list1[[j]][[i+1]] <- input_list[[i]][[j]]
    }
  }

  return(list1)
}

