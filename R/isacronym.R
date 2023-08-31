#' Is Acronym
#'
#'This function checks whether or not a string is an acronym
#' @param string This is any string value or word which the function checks
#'
#' @return Returns a boolean value
#' @export
#'
#' @examples
#' is.acronym("UNICEF")
is.acronym <- function(string) {
  
  # splits the string into individual words
  string_split <- c(unlist(strsplit(string, split = " ")))
  
  if (length(string_split) > 1) { # checks the length of the word vector
    return(FALSE)
  } else if (length(string_split) == 1) {
    # splits a string into characters
    split_string <- c(unlist(strsplit(string, split = "")))
    string_upper <- toupper(split_string)
    
    # checks if the characters in the string match their capitalized forms
    if (all(split_string %in% string_upper) == TRUE) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

