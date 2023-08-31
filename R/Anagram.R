#' Anagram Checker
#'
#'This function checks if two words are anagrams of each other
#' @param word1 This is the first word to be checked against the second word 
#' @param word2 This is the second word to be checked against the first word 
#'
#' @return Returns a boolean value
#' @export
#'
#' @examples
#' anagramChecker("tight","light")
anagramChecker <- function(word1, word2) {
  # Convert words to lowercase
  word1 <- tolower(word1)
  word2 <- tolower(word2)
  
  # Check if the words have the same length
  if (nchar(word1) != nchar(word2)) {
    return(FALSE)
  }
  
  # Check if the sorted characters of the words are the same
  sorted_word1 <- sort(strsplit(word1, "")[[1]])
  sorted_word2 <- sort(strsplit(word2, "")[[1]])
  
  if (identical(sorted_word1, sorted_word2)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
