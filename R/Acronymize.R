#' Acronymize
#'
#'This function turns a phrase into an acronym
#' @param phrase This is a string value that will turned into an acronym
#' @param separator This can either be "" or .
#'
#' @return Returns an acronym of the input string
#' @export
#'
#' @examples
#' Acronymize("International Monetary Fund", separator = "")
Acronymize <- function(phrase,separator) {

  # split phrase into individual words
  phrase_split <- c(unlist(strsplit(phrase, split = " ")))

  # loop through each word and capitalize the first letter
  split_words <- c()
  for (i in phrase_split) {
    word_split <- toupper(c(c(unlist(strsplit(i, split = ""))))[1])
    split_words <- c(split_words, word_split)
  }

  # group the capitalized letters together
  acronym <- noquote(paste(split_words, collapse = separator))

  return(acronym)
  
}

