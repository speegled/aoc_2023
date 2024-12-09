library(tidyverse)
dd <- read_lines("data/day_09")

sum(sapply(1:length(dd), function(i) {
  vals <- as.integer(str_split_1(dd[i], pattern = " "))
  xs <- 1:length(vals)
  mod <- lm(vals ~ poly(xs, length(vals) - 2))
  predict(mod, newdata = data.frame(xs = length(vals)  +1))
}))

sapply(1:length(dd), function(i) {
  vals <- as.integer(str_split_1(dd[i], pattern = " "))
  xs <- 1:length(vals)
  mod <- lm(vals ~ poly(xs, length(vals) -2))
  predict(mod, newdata = data.frame(xs = 0 ))
})
