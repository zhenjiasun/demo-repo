library(dplyr)

#' @export
summary.rolls <- function(x, ...) {
  proportions <- c(

    sum(x$rolls == x$sides[1]) / x$total,
    sum(x$rolls == x$sides[2]) / x$total,
    sum(x$rolls == x$sides[3]) / x$total,
    sum(x$rolls == x$sides[4]) / x$total,
    sum(x$rolls == x$sides[5]) / x$total,
    sum(x$rolls == x$sides[6]) / x$total
  )
  freqs <- data.frame(
    side = x$sides,
    count=select(data.frame(table(x$rolls)),-1),
    prop = proportions)
  obj <- list(freqs = freqs)

  class(obj) <- "summary.rolls"
  obj
}

#side = x$sides,
#count = (table(x$rolls)

#' @export
print.summary.roll <- function(x, ...) {
  cat('summary "rolls"\n\n')
  print(x$freqs)
  invisible(x)
}


set.seed(123)
fair_50rolls <- roll(fair_die, times = 50)
fair_50rolls
fair50_sum <- summary(fair_50rolls)
fair50_sum


# what's in the summary
names(fair50_sum)
fair50_sum$freqs

