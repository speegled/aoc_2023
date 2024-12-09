library(tidyverse)
library(tidytable)
source("source.R")
mm <- get_maze("data/day_16")
str_length(mm[6, 10])
mm[mm == "\\"] <- "7"
dd <- data.frame(time = 0, row = 1, col = 0, dir = "r")
j <- 0
system.time(while(max(dd$time) == j) {
  j <- j + 1
  dd <- distinct(bind_rows(dd,
                           map_df(1:nrow(slice_max(dd, time)), function(x) {
                             #browser()
                             temp <- slice_max(dd, time)[x,]
                             temp$time <- temp$time + 1
                             if(temp$dir == "r" && temp$col < ncol(mm)) {
                               temp$col <- temp$col + 1
                               val <- mm[temp$row, temp$col]
                               if(val == "." || val == "-") {
                                 return(temp)
                               }
                               if(val == "|") {
                                 temp <- bind_rows(
                                   temp, temp
                                 )
                                 temp$dir[1] = "u"
                                 temp$dir[2] = "d"
                                 return(temp)
                               }
                               if(val == "7") {
                                 temp$dir = "d"
                                 return(temp)
                               }
                               if(val == "/") {
                                 temp$dir = "u"
                                 return(temp)
                               }
                             }
                             if(temp$dir == "l" && temp$col > 1)  {
                               temp$col <- temp$col - 1
                               val <- mm[temp$row, temp$col]
                               if(val == "." || val == "-") {
                                 return(temp)
                               }
                               if(val == "|" ) {
                                 temp <- bind_rows(
                                   temp, temp
                                 )
                                 temp$dir[1] ="u"
                                 temp$dir[2] = "d"
                                 return(temp)
                               }
                               if(val == "7") {
                                 temp$dir = "u"
                                 return(temp)
                               }
                               if(val == "/") {
                                 temp$dir = "d"
                                 return(temp)
                               }
                             }
                             if(temp$dir == "u" && temp$row > 1)  {
                               temp$row <- temp$row - 1
                               val <- mm[temp$row, temp$col]
                               if(val == "." || val == "|") {
                                 return(temp)
                               }
                               if(val == "-") {
                                 temp <- bind_rows(
                                   temp, temp
                                 )
                                 temp$dir[1] = "l"
                                 temp$dir[2] = "r"
                                 return(temp)
                               }
                               if(val == "7") {
                                 temp$dir = "l"
                                 return(temp)
                               }
                               if(val == "/") {
                                 temp$dir ="r"
                                 return(temp)
                               }
                             }
                             if(temp$dir == "d" && temp$row < nrow(mm))  {
                               temp$row <- temp$row + 1
                               val <- mm[temp$row, temp$col]
                               if(val == "." || val == "|") {
                                 return(temp)
                               }
                               if(val == "-") {
                                 temp <- bind_rows(
                                   temp, temp
                                 )
                                 temp$dir[1] = "l"
                                 temp$dir[2] = "r"
                                 return(temp)
                               }
                               if(val == "7") {
                                 temp$dir = "r"
                                 return(temp)
                               }
                               if(val == "/") {
                                 temp$dir = "l"
                                 return(temp)
                               }
                             }
                           })), row, col, dir, .keep_all = T)
})

nrow(distinct(dd[-1,], row, col))

distinct(dd[-1,], row, col, dir)


77/80 * 200
