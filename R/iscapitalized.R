#' Is Capitalized
#'
#'This function checks whether or not a string is capitalized
#' @param string This is any string value which the function checks for evidence of capitalization
#'
#' @return Returns a boolean value
#' @export
#'
#' @examples
#' is.capitalized("message")
is.capitalized <- function(string) {
  string_split <- c(unlist(strsplit(string, split = "")))

  check_case <- string_split[1] == toupper(string_split[1])

  if (check_case == FALSE) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


