library(tidyverse)
dd <- data.frame(x = readLines("data/day_01"))
dd %>%
  mutate(x1 = str_extract_all(x, "^[a-z]*[0-9]"),
         x2 = str_extract(x, "[0-9][a-z]*$")) %>%
  transmute(x = as.integer(str_c(str_extract(x1, "[0-9]"), str_extract(x2, "[0-9]")))) %>%
  summarize(val = sum(x))

locs <- str_locate(dd$x, "one|two|three|four|five|six|seven|eight|nine")
vals <- c("one","two","three","four","five","six","seven","eight","nine")
nums <- as.character(1:9)
for(i in 1:nrow(dd)) {
  if(!is.na(locs[i, 1])) {
    for(val in 1:9) {
      if(str_sub(dd$x[i], locs[i,1], locs[i,2]) == vals[val]) {
        dd$x[i] <- str_replace(dd$x[i], vals[val], str_c(nums[val], vals[val]))
      }
    }
  }
}
dd <- dd %>%
  mutate(x = stringi::stri_reverse(x))
vals <- stringi::stri_reverse(vals)
search_string <- paste(vals, collapse = "|")
locs <- str_locate(dd$x, search_string)
for(i in 1:nrow(dd)) {
  if(!is.na(locs[i, 1])) {
    for(val in 1:9) {
      if(str_sub(dd$x[i], locs[i,1], locs[i,2]) == vals[val]) {
        dd$x[i] <- str_replace(dd$x[i], vals[val], nums[val])
      }
    }
  }
}
dd <- dd %>%
  mutate(x = stringi::stri_reverse(x))
dd %>%
  mutate(x1 = str_extract_all(x, "^[a-z]*[0-9]"),
         x2 = str_extract(x, "[0-9][a-z]*$")) %>%
  transmute(x = as.integer(str_c(str_extract(x1, "[0-9]"), str_extract(x2, "[0-9]")))) %>%
  summarize(val = sum(x))
