#' @title Reverse Char Function
#' @description reverse string by character
#' @param  a string
#' @return reversed string

reverse_chars <- function(string){
    string_split = strsplit(as.character(string), split = "")
    rev_order = nchar(string):1
    reversed_chars = string_split[[1]][nchar(string):1]
    rev=paste(reversed_chars, collapse = "")
    return(rev)
}
