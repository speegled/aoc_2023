library(tidyverse)
dd <- read_lines("data/day_12")
x <- dd[1]


get_lengths <- function(x, vals) {
  v <- str_split_1(x, "")
  v[v == "?"] <- c(".", "#")[vals]
  str_length(str_extract_all(str_c(v, collapse = ""), "[\\#]+")[[1]])
}



needed <- as.integer(str_extract_all(x, "[0-9]")[[1]])
x <- str_remove(x, "[ 0-9,]+")
additional <- sum(needed) - str_count(x, "\\#")
sub_rows <- which(apply(vals, 1, function(x) sum(x == 2)) == additional)
suppressWarnings(sum(sapply(sub_rows, function(i) {
  all(get_lengths(x, vals[i,]) == needed)
})))
j <- 90
poss <- sapply(1:length(dd), function(j) {
  print(j)
  x <- dd[j]
  vals <- combinat::hcube(x = rep(2, str_count(x, "\\?")), scale = 1, translation = 0)
  needed <- as.integer(str_extract_all(x, "[0-9]+")[[1]])
  x <- str_remove(x, "[ 0-9,]+")
  additional <- sum(needed) - str_count(x, "\\#")
  sub_rows <- which(apply(vals, 1, function(x) sum(x == 2)) == additional)
  suppressWarnings(sum(sapply(sub_rows, function(i) {
    all(get_lengths(x, vals[i,]) == needed)
  })))
})

sum(poss)
x <- dd[11]
x <- dd[1]
needed <- as.integer(str_extract_all(x, "[0-9]+")[[1]])

x
needed

x <- ".##...#..###?"
needed <- c(1, 1, 5, 1)




x <- dd[1]
needed <- as.integer(str_extract_all(x, "[0-9]+")[[1]])
x <- str_remove(x, "[ 0-9,]+")
needed
x
is_ok <- Vectorize(function(x, vec) {
  browser()
  end <- min(str_locate(x, "\\?")[,1])
  if(end == 1) {
    return(1)
  }
  ss <- str_sub(x, 1, end - 1)
  ss
  lengths <- str_length(str_extract_all(ss,"[\\#]+")[[1]])
  if(length(lengths) > length(vec)) {
    return(0)
  }
  ll <- length(lengths)
  if(ll == 0) {
    return(1)
  }
  if(ll > 1) {
    if(any(lengths[1:(ll - 1)] != vec[1:(ll - 1)])) {
      return(0)
    }
  }

  if(lengths[ll] > vec[ll]) {
    return(0)
  }
  return(1)
}, vectorize.args = "x")

is_ok(x, needed)
x <- c(sapply(x, function(p) {
  str_replace(p, "\\?", "\\.")
}, USE.NAMES = F), sapply(x, function(p) {
  str_replace(p, pattern = "\\?", "\\#")
}, USE.NAMES = F))
x <- x[is_ok(x, needed) == 1]
length(x)
x
is_ok("..#.?#??.??????", needed)



