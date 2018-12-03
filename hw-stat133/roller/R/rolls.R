

roll <- function(device, times = 1){
  if(class(device) != "device"){
    stop("Device is not of class 'device'")
  }
  else{
  rolls <- flip(device, times=times)
  check_times(times)
  total <- times
  object<-list(rolls=rolls,sides=device$sides, prob=device$prob,total=total)
  class(object) <- "rolls"
  object
  }
}


# private function
flip <- function(x, times = 1) {
  sample(x$sides, size = times, replace = TRUE, prob = x$prob)
}

# private function to check vector of 'times'
check_times <- function(times) {
  if (times <= 0 | !is.numeric(times)) {
    stop("\nargument 'times' must be a positive integer")
  } else {
    TRUE
  }
}


#' @export
print.rolls <- function(x, ...) {
  cat('object "rolls" \n\n')
  cat('$rolls \n\n')
  print(x$rolls)
  invisible(x)
}

# roll fair die 50 times
fair_die <- device()
set.seed(123)
fair50 <- roll(fair_die, times = 50)
fair50


# roll fair die 50 times
fair_die <- device(sides = 1:6, prob = rep(1/6, 6))
# roll 50 times
set.seed(123)
fair_50rolls <- roll(fair_die, times = 50)
# print
fair_50rolls
# what's in fair50?
names(fair50)
fair50$rolls
fair50$sides
fair50$prob
fair50$total

# string die
str_die <- device(
  sides = c('a', 'b', 'c', 'd', 'e', 'f'),
  prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35))
# roll 20 times
set.seed(123)


str_rolls <- roll(str_die, times = 20)
names(str_rolls)
str_rolls

