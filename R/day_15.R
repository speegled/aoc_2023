dd <- data.frame(x = readLines("data/day_15"))
library(tidytable)

dd <- dd %>%
  separate_longer_delim(x, ",")

dd <- dd$x
hash <- function(x, curval) {
  curval <- ((curval + gtools::asc(x)) * 17) %% 256
}
tohash <- function(vec) {
  curval <- 0
  vec <- stringr::str_split_1(vec, pattern = "")
  for(i in 1:length(vec)) {
    curval <- hash(vec[i], curval)
  }
  curval
}
sum(sapply(dd, tohash))

