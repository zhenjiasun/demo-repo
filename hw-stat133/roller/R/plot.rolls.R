set.seed(123)
fair_50rolls <- roll(fair_die, times = 50)
fair50_sum <- summary(fair_50rolls)
fair50_sum

#' @export
plot.roll <-function(x){
  s<-summary(x)
  barplot((s$freqs)[,3],
          main = 'Relative Frequencies in a series of 50 rolls',
          xlab = 'sides of device',
          ylab = 'relative frequencies',
          names.arg = c("1", "2", "3","4","5","6")

  )
}

#plot(fair_50rolls)
plot.roll(fair_50rolls)



