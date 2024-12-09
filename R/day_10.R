library(tidyverse)
library(igraph)

dd <- readLines("data/day_10")
rows <- length(dd)
cols <- str_length(dd[1])

mm <- t(sapply(dd, str_split_1, pattern = "",USE.NAMES  = F))
mm <- cbind(".", rbind(".", mm, "."), ".")
rows <- 2:(rows + 1)
cols <- 2:(cols + 1)

to_int <- function(r, c) {
  r + (c - 1) * nrow(mm)
}
to_rc <- function(val) {
  r <- val %% nrow(mm)
  if(r == 0) {
    r <- nrow(mm)
  }
  c <- ceiling(val/nrow(mm))
  return(list(r = r, c = c))
}
north <- c("|", "7", "F")
south <- c("|", "L", "J")
east <- c("-", "J", "7")
west <- c("-", "F", "L")
ndir <- -1
sdir <- 1
wdir <- -nrow(mm)
edir <- nrow(mm)
get_neighbors <- function(val, debug = F) {
  if(debug) browser()
  nbs <- NULL
  if(mm[val] == "|") {
    if(mm[val + ndir] %in% north ) {
      nbs <- c(nbs, val + ndir)
    }
    if(mm[val + sdir] %in% south ) {
      nbs <- c(nbs, val + sdir)
    }
  }
  if(mm[val] == "-") {
    if(mm[val + wdir] %in% west ) {
      nbs <- c(nbs, val + wdir)
    }
    if(mm[val + edir] %in% east) {
      nbs <- c(nbs, val + edir)
    }
  }
  if(mm[val] == "L") {
    if(mm[val + ndir] %in% north) {
      nbs <- c(nbs, val + ndir)
    }
    if(mm[val + edir] %in% east) {
      nbs <- c(nbs, val + edir)
    }
  }
  if(mm[val] == "J") {
    if(mm[val + ndir] %in% north) {
      nbs <- c(nbs, val + ndir)
    }
    if(mm[val + wdir] %in% west) {
      nbs <- c(nbs, val + wdir)
    }
  }
  if(mm[val] == "7") {
    if(mm[val + sdir] %in% south) {
      nbs <- c(nbs, val + sdir)
    }
    if(mm[val + wdir] %in% west) {
      nbs <- c(nbs, val + wdir)
    }
  }
  if(mm[val] == "F") {
    if(mm[val + sdir] %in% south) {
      nbs <- c(nbs, val + sdir)
    }
    if(mm[val + edir] %in% east) {
      nbs <- c(nbs, val + edir)
    }
  }
  return(nbs)
}
start <- which(mm == "S")
mm[start + c(ndir, sdir, wdir, edir)]
mm[start] <- "-" #real data
#mm[start] <- "F"     #test 2 this is done by hand :-(
nbs <- purrr::map_df(rows, function(r) {
  purrr::map_df(cols, function(c) {
    val <- to_int(r, c)
    nn <- get_neighbors(val)
    if(!is.null(nn)) {
      data.frame(x = val, y = nn)
    }
  })
})
gg <- igraph::graph_from_data_frame(nbs)
startver <- V(gg)[which(attr(V(gg), which = "name") == start)] #bleah, i forgot how i hate working like this. i should really learn the correct way to do these things!!

all_simple_paths(gg, from = startver, to = neighbors(gg, startver ))
path <- c(as.character(all_simple_paths(gg, from = startver, to = neighbors(gg, startver))[[2]]), startver)
floor(length(path)/2) #first star

pathdf <- purrr::map_df(path, function(val) {
  val <- as.integer(attr(V(gg)[V(gg) == val], "name"))
  as.data.frame(to_rc(val))
}) #why is this so slow, OMG

pp <- purrr::map_df(rows, function(r) {
  purrr::map_df(cols, function(c) {
    inpo <- sp::point.in.polygon(c, r, pathdf$c, pathdf$r) #figured this was a standard task already coded up
    data.frame(r = r, c = c, res = inpo)
  })
})
sum(pp$res== 1) #second star!

