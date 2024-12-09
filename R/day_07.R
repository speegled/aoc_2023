library(tidyverse)
get_hand <- function(a) {
  a <- str_split_1(a, "")[1:5]
  tt <- sort(table(a))
  # case_when(
  #   tt[1] == 5 ~ 7,
  #   tt[2] == 4 ~ 6,
  #   tt[1] == 2 && tt[2] == 3 ~ 5,
  #   tt[1] == 1 && tt[3] == 3 ~ 4,
  #
  # )

  if(length(tt) == 1) {
    return(7)
  }
  if(any(tt == 4)) {
    return(6)
  }
  if(tt[1] == 2 && tt[2] == 3) {
    return(5)
  }
  if(tt[1] == 1 && tt[3] == 3) {
    return(4)
  }
  if(tt[2] == 2 && tt[3] == 2) {
    return(3)
  }
  if(tt[4] == 2) {
    return(2)
  }
  if(length(tt) == 5) {
    return(1)
  }



  warning("fall through")
}



lt <- function(a, b) {
  handa <- get_hand(a)
  handb <- get_hand(b)
  if(handa > handb) {
    return(FALSE)
  }
  if(handb > handa ) {
    return(TRUE)
  }
  a <- factor(str_split_1(a, "")[1:5], ordered = T, levels = rev(c("A", "K", "Q", "J", "T", as.character(9:2))))
  b <- factor(str_split_1(b, "")[1:5], ordered = T, levels = rev(c("A", "K", "Q", "J", "T", as.character(9:2))))
  for(i in 1:5) {
    if(a[i] < b[i]) {
      return(TRUE)
    }
    if(b[i] < a[i]) {
      return(FALSE)
    }
  }
  warning("fall through")
}

bubble_sort = function(array) {
  count = 0
  while(1) {
    count_swaps = 0
    for (j in 1 : (length(array) - 1 - count)) {
      if (lt(array[j + 1], array[j])) {
        s = array[j]
        array[j] = array[j+1]
        array[j+1] = s
        count_swaps = count_swaps + 1
      }
    }
    count = count + 1
    print(count)
    if(count_swaps == 0) break
  }
  array
}

array <- readLines("data/day_07")
array <- bubble_sort(array)

get_hand <- function(a) {
  a <- str_split_1(a, "")[1:5]
  tt <- sort(table(a))
  # case_when(
  #   tt[1] == 5 ~ 7,
  #   tt[2] == 4 ~ 6,
  #   tt[1] == 2 && tt[2] == 3 ~ 5,
  #   tt[1] == 1 && tt[3] == 3 ~ 4,
  #
  # )

  if(length(tt) == 1) {
    return(7)
  }

  if(any(a == "J")) {
    tt2 <- sort(table( a[a!="J"]  ))
    change_to <- attr(which.max(tt2), which = "name")
    a <- str_replace_all(a, "J", change_to)
  }

  tt <- sort(table(a))

  if(length(tt) == 1) {
    return(7)
  }

  if(any(tt == 4)) {
    return(6)
  }
  if(tt[1] == 2 && tt[2] == 3) {
    return(5)
  }
  if(tt[1] == 1 && tt[3] == 3) {
    return(4)
  }
  if(tt[2] == 2 && tt[3] == 2) {
    return(3)
  }
  if(tt[4] == 2) {
    return(2)
  }
  if(length(tt) == 5) {
    return(1)
  }



  warning("fall through")
}
hands <- sapply(array, get_hand)
lt <- function(a, b) {
  handa <- hands[a]
  handb <- hands[b]
  if(handa > handb) {
    return(FALSE)
  }
  if(handb > handa ) {
    return(TRUE)
  }
  a <- factor(str_split_1(a, "")[1:5], ordered = T, levels = rev(c("A", "K", "Q", "T", as.character(9:2), "J" )))
  b <- factor(str_split_1(b, "")[1:5], ordered = T, levels = rev(c("A", "K", "Q", "T", as.character(9:2), "J" )))
  for(i in 1:5) {
    if(a[i] < b[i]) {
      return(TRUE)
    }
    if(b[i] < a[i]) {
      return(FALSE)
    }
  }
  warning("fall through")
}

bubble_sort(array)
array <- .Last.value
sum(as.integer(str_extract(array, "[0-9]*$")) * (1:length(array)) )
