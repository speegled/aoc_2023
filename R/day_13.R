library(tidyverse)
dd <- data.frame(x = readLines("data/day_13"))
dd$puzzle <- cumsum(!str_detect(dd$x, "[\\#\\.]")) + 1
dd$puzzle
dd <- dd %>%
  filter(str_detect(x, "[\\#\\.]"))

aa <- filter(dd, puzzle == 1) %>%
  pull(x)
aa <- t(sapply(aa, function(x) {
  str_split_1(x, pattern = "")
}, USE.NAMES = F))
nrow <- nrow(aa)
ncol <- ncol(aa)
aa <- as.data.frame(which(aa == "#", arr.ind = T))
i <- 4
rowsymm <- purrr::map_df(1:max(dd$puzzle), function(j) {
  aa <- filter(dd, puzzle == !!j) %>%
    pull(x)
  aa <- t(sapply(aa, function(x) {
    str_split_1(x, pattern = "")
  }, USE.NAMES = F))
  nrow <- nrow(aa)
  ncol <- ncol(aa)
  aa <- as.data.frame(which(aa == "#", arr.ind = T))
  purrr::map_df(1:(nrow - 1), function(i) {
    bb <- aa
    bb$row <- abs(bb$row - i - 0.5)
    if(filter(bb, row < min(nrow - i, i)) %>%
       group_by(row, col) %>%
       summarize(n = n(), .groups = "drop") %>%
       ungroup() %>%
       summarize(symm = all(n == 2)) %>%
       pull(symm)) {
      data.frame(puzzle = j, row = i + .5)
    }
  })
})

colsymm <- purrr::map_df(1:max(dd$puzzle), function(j) {
  aa <- filter(dd, puzzle == !!j) %>%
    pull(x)
  aa <- sapply(aa, function(x) {
    str_split_1(x, pattern = "")
  }, USE.NAMES = F)
  nrow <- nrow(aa)
  ncol <- ncol(aa)
  aa <- as.data.frame(which(aa == "#", arr.ind = T))
  purrr::map_df(1:(nrow - 1), function(i) {
    bb <- aa
    bb
    bb$row <- abs(bb$row - i - 0.5)
    bb
    if(filter(bb, row < min(nrow - i, i)) %>%
       group_by(row, col) %>%
       summarize(n = n(), .groups = "drop") %>%
       ungroup() %>%
       summarize(symm = all(n == 2)) %>%
       pull(symm)) {
      data.frame(puzzle = j, row = i + .5) %>%
        rename(col = row)
    }
  })
})
rowsymm
colsymm
sum((rowsymm$row - .5) * 100) + sum(colsymm$col - .5)





rowsymm <- purrr::map_df(1:max(dd$puzzle), function(j) {
  aa <- filter(dd, puzzle == !!j) %>%
    pull(x)
  aa <- t(sapply(aa, function(x) {
    str_split_1(x, pattern = "")
  }, USE.NAMES = F))
  nrow <- nrow(aa)
  ncol <- ncol(aa)
  aa <- as.data.frame(which(aa == "#", arr.ind = T))
  purrr::map_df(1:(nrow - 1), function(i) {
    bb <- aa
    bb
    bb$row <- abs(bb$row - i - 0.5)
    bb
    if(filter(bb, row < min(nrow - i, i)) %>%
       group_by(row, col) %>%
       summarize(n = n(), .groups = "drop") %>%
       ungroup() %>%
       summarize(symm = sum(n == 1) == 1) %>%
       pull(symm)) {
      data.frame(puzzle = j, row = i + .5)
    }
  })
})

colsymm <- purrr::map_df(1:max(dd$puzzle), function(j) {
  aa <- filter(dd, puzzle == !!j) %>%
    pull(x)
  aa <- sapply(aa, function(x) {
    str_split_1(x, pattern = "")
  }, USE.NAMES = F)
  nrow <- nrow(aa)
  ncol <- ncol(aa)
  aa <- as.data.frame(which(aa == "#", arr.ind = T))
  purrr::map_df(1:(nrow - 1), function(i) {
    bb <- aa
    bb
    bb$row <- abs(bb$row - i - 0.5)
    bb
    if(filter(bb, row < min(nrow - i, i)) %>%
       group_by(row, col) %>%
       summarize(n = n(), .groups = "drop") %>%
       ungroup() %>%
       summarize(symm = sum(n == 1) == 1) %>%
       pull(symm)) {
      data.frame(puzzle = j, row = i + .5) %>%
        rename(col = row)
    }
  })
})
rowsymm
colsymm
sum((rowsymm$row - .5) * 100) + sum(colsymm$col - .5)



