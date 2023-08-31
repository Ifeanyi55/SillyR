#' Shorten Vector
#'
#'This function shortens the length of vector
#' @param vector This a vector of string or numeric values
#' @param by This is by how many values the vector should be shortened
#'
#' @return Returns either a string vector or a numeric vector
#' @export
#'
#' @examples
#' shorten.vector(c("apple","pear","grape","peach"), by = 2)
shorten.vector <- function(vector,by){
  # get last elements
  vec_new <- vector |> tail(by) 
  
  # get index of last elements
  index <- c()
  for(i in vec_new){
    values <- which(vector == i) 
    index <- c(index,values)
  }
  
  # delete index from vector
  final_vec <- vector[-c(index)]
  
  # return final output
  return(final_vec)
  
  
}

