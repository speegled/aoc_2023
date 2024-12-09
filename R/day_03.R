library(tidyverse)
dd <- readLines("data/day_03")
dd <- c(str_c(rep(".", str_length(dd[1])), collapse = "") , dd, str_c(rep(".", str_length(dd[1])), collapse = ""))
dd <- str_c(".", dd, ".")

aa <- strsplit(dd, "")
dd <- matrix(unlist(aa), ncol = length(aa[[1]]), byrow = T)

zz <- ncol(dd)
get_neighbors <- Vectorize(function(x, locs = FALSE) {
  if(locs) {
    return(x + c(1, -1, zz, -zz, zz + 1, zz - 1, -zz + 1, -zz - 1))
  } else {
    return(dd[x + c(1, -1, zz, -zz, zz + 1, zz - 1, -zz + 1, -zz - 1)])
  }
}, vectorize.args = "x")

keep <- function(x, debug = FALSE) {
  if(debug) {browser()}
  if(dd[x - zz] %in% 0:9) {
    return(0)
  }
  if(dd[x] %in% 0:9) {
    vals <- x
    i <- 1
    while(dd[x + i * zz] %in% 0:9) {
      vals <- c(vals, x + i * zz)
      i <- i + 1
    }
    i <- -1
    while(dd[x + i * zz] %in% 0:9) {
      vals <- c(x + i * zz, vals)
      i <- i - 1
    }
    if(all(get_neighbors(vals) %in% c(".", 0:9))) {
      return(0)
    } else {
      return(as.integer(str_c(dd[vals], collapse = "")))
    }
  } else {
    return(0)
  }
}


sum(sapply(2:(ncol(dd) - 1), function(col) {
  sapply(2:(nrow(dd) - 1), function(row) {
    val <- (col - 1) * zz + row
    keep(val)
  })
}))


get_number <- Vectorize(function(x, locs = FALSE) {
  if(!(dd[x] %in% 0:9)) {
    return(NA)
  }
  vals <- x
  i <- 1
  while(dd[x + i * zz] %in% 0:9) {
    vals <- c(vals, x + i * zz)
    i <- i + 1
  }
  i <- -1
  while(dd[x + i * zz] %in% 0:9) {
    vals <- c(x + i * zz, vals)
    i <- i - 1
  }
  if(locs) {
    return(str_c(vals, collapse = ""))
  } else {
    return(as.integer(str_c(dd[vals], collapse = "")))
  }

}, vectorize.args = "x")

get_ratio <- Vectorize(function(val) {
  if(dd[val] != "*") {
    return(0)
  }
  if(sum(!is.na(unique( get_number(get_neighbors(val, T), locs = T)  ))) == 2) {
    return(prod(unique(get_number(get_neighbors(val, TRUE))), na.rm = T))
  } else{
    return(0)
  }
}, vectorize.args = "val")

sum(get_ratio(1:(nrow(dd) * ncol(dd))))


