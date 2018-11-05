#' @title Count Vowels Function
#' @description computes how many each vowel there is in a string
#' @param  a string
#' @return a numeric vector/ with names for vowels

count_vowels <- function (y){
  if(is.character(y)==1){
    y=casefold(y)
    dat=sapply(vowels, function(x) x<-sum(x==unlist(strsplit(y,""))))
    return(dat)
  }else{
    stop("invalid input; a string was expected")
  }
}

