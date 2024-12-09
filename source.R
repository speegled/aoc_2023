between <- Vectorize(function(x, val1, val2, strict = T) {
  if(val1 > val2) {
    temp <- val1
    val1 <- val2
    val2 <- temp
  }
  if(strict) {
    if(x > val1 && x < val2) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    if(x >= val1 && x <= val2) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}, vectorize.args = "x")

to_int <- function(r, c) {
  r + (c - 1) * nrow(mm)
}

to_rc <- Vectorize(function(val) {
  r <- val %% nrow(mm)
  if(r == 0) {
    r <- nrow(mm)
  }
  c <- ceiling(val/nrow(mm))
  return(list(r = r, c = c))
}, vectorize.args = "val")

get_maze <- function(file) {
  dd <- readLines(file)
  ncol = str_length(dd[1])
  dd <- t(sapply(dd, str_split_1, pattern = "", USE.NAMES = F))
  mm <- matrix(dd, ncol = ncol)
  return(mm)
}

