dd <- readLines("data/day_11")
library(tidyverse)
source("source.R") #for between, which is actually **between**, e.g. between(3, 5, 1) should definitely be TRUE
ncol = str_length(dd[1])
dd <- t(sapply(dd, str_split_1, pattern = "", USE.NAMES = F))
mm <- matrix(dd, ncol = ncol)

row_expand <- sapply(1:nrow(mm), function(x) {
  all(mm[x,] == ".")
})

col_expand <- sapply(1:ncol(mm), function(x) {
  all(mm[,x] == ".")
})

dis <- function(p1, p2) {
  temp <- sum(abs(p2 - p1))
  temp + sum(between(which(row_expand), p1[1], p2[1])) + sum(between(which(col_expand), p1[2], p2[2]))
}


gal <- which(mm == "#", arr.ind = T)
tot <- 0
for(i in 1:nrow(gal)) {
  for(j in i:nrow(gal)) {
    tot <- tot + dis(gal[i,], gal[j,])
  }
}
tot #first star

dis <- function(p1, p2) {
  temp <- sum(abs(p2 - p1))
  temp + (1000000 - 1) * sum(between(which(row_expand), p1[1], p2[1])) + (1000000 - 1) * sum(between(which(col_expand), p1[2], p2[2]))
}
tot <- 0
for(i in 1:nrow(gal)) {
  for(j in i:nrow(gal)) {
    tot <- tot + dis(gal[i,], gal[j,])
  }
}
tot #second star

