time <- c(54, 70, 82, 75)
distance <- c(239, 1142, 1295, 1253)
i <- 1
prod(sapply(1:4, function(i) {
  x <- 0:time[i]
  sum(x * (time[i] - x) > distance[i])
}))

time <- 54708275
x <- 0:time
sum(x * (time - x) > 239114212951253)

b <- -time
c <- 239114212951253
(-b + c(1, -1) * sqrt(b^2 - 4 * c))/2
