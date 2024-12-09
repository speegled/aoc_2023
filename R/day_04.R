library(tidyverse)
dd <- readLines("data/day_04")
dd <- str_remove(dd, "Card[ ]*[0-9]*: ")
winners <- t(str_extract(dd, ".*\\|") %>%
  str_remove(" \\|") %>%
  sapply(function(x) {
    #browser()
    aa <- unlist(str_extract_all(x, "[0-9]*"))
    aa <- aa[aa != ""]
    aa
  }, USE.NAMES = F))

cards <- t(str_extract(dd, "\\|.*") %>%
             str_remove("\\|[ ]*") %>%
             sapply(function(x) {
               #browser()
               aa <- unlist(str_extract_all(x, "[0-9]*"))
               aa <- aa[aa != ""]
               aa
             }, USE.NAMES = F))

matches <- sapply(1:nrow(winners), function(x) {
  sum(sapply(1:length(cards[x,]), function(y) {
    cards[x, y] %in% winners[x,]
  }))
})
matches <- matches[matches != 0]
sum(2^(matches - 1))

matches <- sapply(1:nrow(winners), function(x) {
  sum(sapply(1:length(cards[x,]), function(y) {
    cards[x, y] %in% winners[x,]
  }))
})

cards <- rep(1, length.out = length(matches))
for(i in 1:length(matches)) {
  if(matches[i] > 0) {
    cards[(i + 1):(i + matches[i])] <- cards[(i + 1):(i + matches[i])] + cards[i]
  }
  print(cards[i])
}
sum(cards)


