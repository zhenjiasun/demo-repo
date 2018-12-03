
#' @description Creates an object of class \code{"device"}
#' @param sides vector of device sides
#' @param prob vector of side probabilities
#' @return an object of class \code{"device"}
#' @export
device <- function(sides = c(1, 2), prob = c(0.5, 0.5)){
  check_sides(sides)
  check_prob(prob)
  object <-list(sides = sides , prob = prob)
  class(object) <- "device"
  object
}


# private function to check vector of sides
check_sides <- function(sides) {
  if (length(sides) < 2) {
    stop("\n'sides' must be a vector of length greater than 1")
  }
  if (!is.numeric(sides) & !is.character(sides)) {
    stop("\n'sides' must be a character or numeric vector")
  }
  if (any(duplicated(sides))) {
    stop("\n'sides' cannot have duplicated elements")
  }
  TRUE
}

# private function to check vector of probabilities
check_prob <- function(prob) {
  if (length(prob) < 1 | !is.numeric(prob)) {
    stop("\n'prob' must be a numeric vector of at least length 2")
  }
  if (any(is.na(prob))) {
    stop("\n'prob' cannot contain missing values")
  }
  if (any(prob < 0) | any(prob > 1)) {
    stop("\n'prob' values must be between 0 and 1")
  }
  if (sum(prob) != 1) {
    stop("\nelements in 'prob' must add up to 1")
  }
  TRUE
}

#' @export
print.device <- function(x, ...) {
  cat('object "device"\n\n')
  print(data.frame(sides=x$sides, prob = x$prob))
}

#' @rdname device
#' @param x an R object
#' @export
is.device <- function(x) {
  if(is(x, "device")==TRUE){
    return("TRUE")
  }
  else{return("FALSE")}
}


#default call: creates a coin device
fair_coin <- device()
fair_coin

# die with non-standard sides
weird_die <- device(
  sides = c('i', 'ii', 'iii', 'iv'),
  prob = rep(1/4, 4))
weird_die

# create a loaded die
loaded_die <- device(
  sides = 1:6,
  prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35))
loaded_die
is.device(loaded_die)

# bad sides (there must be at least 2-sides)
invalid_device <- device(sides = c('a'))
invalid_device

# bad sides (duplicated sides)
bad_coin <- device(sides = c('heads', 'heads'))

# bad probability values for prob
bad_coin <- device(
  sides = c('a', 'b'),
  prob = c(0.2, 0.1))

# false device
is.device(c(1, 2, 3))


```

