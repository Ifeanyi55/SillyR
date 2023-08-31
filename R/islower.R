#' Is Lower
#'
#'This function checks whether or not a string is in lowercase characters
#' @param string This is any string value which the function checks
#'
#' @return Returns a boolean value
#' @export
#'
#' @examples
#' is.lower("maximum")
is.lowercase <- function(string) {
  string_split <- c(unlist(strsplit(string, split = "")))

  check_case <- string_split == tolower(string_split)

  if (all(check_case) == TRUE) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



