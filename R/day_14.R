library(tidyverse)
dd <- readLines("data/day_14_test")
mm <- t(sapply(dd, str_split_1, pattern = "", USE.NAMES = F))
old <- mm
i <- 4
for(i in 2:nrow(mm)) {
  for(j in 1:ncol(mm)) {
    if(mm[i, j] == "O") {
      k <- i
      while(mm[k - 1, j] == ".") {
        k <- k - 1
        if(k == 1) {
          break
        }
      }
      mm[i, j] <- "."
      mm[k,j] <- "O"
    }
  }
}
rr <- as.data.frame(which(mm == "O", arr.ind = T))
rr$row <- nrow(mm) + 1 - rr$row
sum(rr$row)

#install.packages("hash")

rotate <- function(mat) {
  temp <- mat
  for(i in 1:ncol(mat)) {
    temp[i,] <- mat[nrow(mat):1,i]
  }
  temp
}

rotate(mm)

vals <- as.list(rep(0, 10000))
vals[[1]] <- mm

library(hash)
hh <- hash::hash(mm, NA)
hh[]



