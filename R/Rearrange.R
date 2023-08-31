#' Rearrange
#' 
#' This function returns the number of possible ways in which a word or string can be rearranged
#' @param string This any string value passed to the function to be rearranged
#' @param as_report This takes a boolean value. If TRUE, the function prints a short report in the console. If FALSE, it prints out the number of possible ways in which a string value can be rearranged. It defaults to TRUE.
#'
#' @return Returns either a numeric value or short report.
#' @export
#'
#' @examples
#' rearrangeWays("tension",as_report = FALSE)
rearrangeWays <- function(string,as_report = TRUE){
  
  if(as_report){ # conditional statement
    
    # split the string into letters
    string_split <- c(unlist(strsplit(string,split = "")))
    
    # get the factorial of the letters vector
    possible_ways <- factorial(length(string_split))
    
    # create the answer text
    answer <- paste(string, "can be rearranged in", possible_ways, "possible ways")
    
    
    return(noquote(answer))
    
    
  }else{
    
    # split the string into letters
    string_split <- c(unlist(strsplit(string,split = "")))
    
    # get the factorial of the letters vector
    possible_ways <- factorial(length(string_split))
    
    return(possible_ways)
    
  }
  
  
}