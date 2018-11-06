#' @title Is_Hex Function
#' @description computes if the input is a valid hex color
#' @param  hexadecimal notation
#' @return TRUE or FALSE

is_hex<- function(x){
  if (!is.character(x)==1){
    stop("invalid input; a string was expected")
  }
  if(nchar(x)!=7){
    return(FALSE)
  }
  str_chars = toupper(strsplit(x,'')[[1]])
  valid_chars = c(0:9, LETTERS[1:6])
  if(str_chars[1] =="#") {
    for (i in 2:nchar(x)) {
      if (!str_chars[i] %in% valid_chars) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

#' @title Is_Hex_Alpha Function
#' @description computes if the input is a valid hex color with alpha 
#' @param  hexadecimal notation
#' @return TRUE or FASLE
is_hex_alpha<- function(x){
  if (!is.character(x)==1){
    stop("invalid input; a string was expected")
  }
  if(nchar(x)!=9){
    return(FALSE)
  }
  str_chars = toupper(strsplit(x,'')[[1]])
  valid_chars = c(0:9, LETTERS[1:6])
  if(str_chars[1] =="#") {
    for (i in 2:nchar(x)) {
      if (!str_chars[i] %in% valid_chars) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

#好像就是这样。。吧




