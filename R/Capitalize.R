#' Capitalize
#'
#'This function capitalizes any word passed to it
#' @param string This any word passed to the function
#'
#' @return Returns a capitalized version of the word passed to the function
#' @export
#'
#' @examples
#' Capitalize("institution")
Capitalize <- function(string){
  
if(length(string) == 1){  
  
Capitalize_word <- function(string) {
    
    # check that string is in lowercase
    split.word <- c(unlist(strsplit(string,split = "")))
    check <- split.word %in% letters
    
    if(all(check) == FALSE){
      
    # convert uppercase to lowercase
    split.word <- tolower(split.word)
    
    # capitalize first letter in string
    letter.cap <- toupper(split.word[1])
    
    # replace the first letter in string with capitalized letter
    split.word[1] <- letter.cap
    
    # group letters together
    split.group <- paste(split.word,collapse = "")
    
    return(split.group)
    
  } else {

  
    # capitalize the first letter in string
    cap.letter <- toupper(split.word[1])
  
    # replace the first letter in string with capitalized letter
    split.word[1] <- cap.letter
  
    # group letters together
    cap.word <- paste(split.word, collapse = "")
  
    return(cap.word)
    
  }
  
}

} else if(length(string) > 1){
Capitalize_word <- function(string) {
    
      # check that string is in lowercase
      split.word <- c(unlist(strsplit(string,split = "")))
      check <- split.word %in% letters
      
      if(all(check) == FALSE){
        
      # convert uppercase to lowercase
      split.word <- tolower(split.word)
      
      # capitalize first letter in string
      letter.cap <- toupper(split.word[1])
      
      # replace the first letter in string with capitalized letter
      split.word[1] <- letter.cap
      
      # group letters together
      split.group <- paste(split.word,collapse = "")
      
      return(split.group)
      
    } else {
      
      
      # capitalize the first letter in string
      cap.letter <- toupper(split.word[1])
      
      # replace the first letter in string with capitalized letter
      split.word[1] <- cap.letter
      
      # group letters together
      cap.word <- paste(split.word, collapse = "")
      
      return(cap.word)
      
    }
    
}
  
}

caps <- c()
for (i in 1:length(string)) {
  result <- Capitalize_word(string[i])
  caps <- c(caps, result)
}

  return(noquote(caps))

}

