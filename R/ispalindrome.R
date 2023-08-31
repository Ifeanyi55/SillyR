#' Is Palindrome
#'
#'This function checks whether or not a string or numeric value is a palindrome
#' @param input This can either be a string or numeric value
#'
#' @return Returns a boolean value
#' @export
#'
#' @examples
#' is.palindrome(858)
#' is.palindrome("Bot")
is.palindrome <- function(input) {
  if (is.numeric(input)) {
    # convert the numeric input value to character
    input_con <- as.character(input)
    # split the string into characters
    input_split <- c(unlist(strsplit(input_con, split = "")))
    # reverse the characters
    input_rev <- rev(input_split)
    # conditional statement to compare split string
    # with the reversed string
    if (all(input_split == input_rev) != TRUE) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else if (is.character(input)) {
    # split the string into characters
    input_split <- c(unlist(strsplit(tolower(input), split = "")))
    # reverse the characters
    input_rev <- rev(input_split)
    # conditional statement to compare split string
    # with the reversed string
    if (all(input_split == input_rev) != TRUE) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}


