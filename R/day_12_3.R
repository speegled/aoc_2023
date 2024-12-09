str <- dd$springs[12]
pp <- as.integer(str_split_1(dd$arr[12], pattern = ","))

count_ways <- Vectorize(function(str, num) {
  #browser()
  str <- str_split_1(str, pattern = "")
  str <- c(str, ".")
  nn <- length(str)
  if(nn == 1) {
    return(NULL)
  }
  if(nn == 2) {
    if(str[1] == "?" || str[1] == "#") {
      return(1)
    } else {
      return(NULL)
    }
  }
  #vv <- str_locate_all(str, "\\?")[[1]][,1]
  which(sapply(1:nn, function(x) {
    if(all(str[1:(x - 1)] %in% c(".", "?")) && all(str[x:(x + num - 1)] %in% c("?", "#")) && str[x + num] %in% c(".", "?")) {
      return(TRUE)
    }
    return(FALSE)
  }))
}, vectorize.args = "str", USE.NAMES = FALSE)

#count_ways(, 2)

getposs <- function(str, pp, debug = F) {
  if(debug) {browser()}
  num <- pp[1]
  vals <- count_ways(str, num)
  str <- purrr::map_vec(vals, function(xx) {
      str_sub(str, xx, xx + num) <- str_c(c(rep("#", num), "."), collapse = "")
      str_sub(str, 1, xx) <- str_replace_all(str_sub(str, 1, xx), "\\?", "\\.")
      str
  })

  if(length(str) == 0) {
    return(NULL)
  }
  pp <- pp[-1]
  str <- str_remove(str, "^[\\.\\#]+")
  if(length(pp) == 0) {
    str[!str_detect(str, "\\#")]
    return(str)
  }
  str <- str[str_length(str) > 0]
  return(unlist(sapply(str, function(s) {
    getposs(s, pp, debug)
    }), use.names = FALSE))
}
str <- str_c(str, ".", collapse = "")
length(getposs(str, pp, F))

sapply(1:12, function(i) {
  print(i)
  str <- str_c(ss[i], ".", collapse = "")
  length(getposs(str, pp[i], F))
})

unlist(sapply(1:4, function(x) {
  runif(x)
}))

tt <- "asfasdfas"
str_sub(tt, 1, 2) <- "PP"
tt
