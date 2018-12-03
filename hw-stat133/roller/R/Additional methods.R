#' @export
"[.rolls" = function(x, i){
  x$rolls[i]
}

#' @export
"[<-.rolls" = function(x, i, value){
  if (! value %in% x$sides){
    stop("replacing value must be one of sides")
  }
  x$rolls[i] = value
  res = list(rolls = x$rolls,
             sides = x$sides,
             prob = x$prob,
             times = x$times)
  class(res) = "rolls"
  res
}


#' @export
"+.rolls" = function(x, inc){
  if (length(inc) != 1 | inc <= 0){
    stop("invalid increment")
  }
  more = roll(device = device(x$sides, x$prob), times = inc)
  res = list(rolls = c(x$rolls, more$rolls),
             sides = x$sides,
             prob = x$prob,
             times = x$times + inc)
  class(res) = "rolls"
  res
}

