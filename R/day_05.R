dd <- data.frame(x = readLines("data/day_05_test"))
library(tidyverse)
maps <- dd %>%
  mutate(group = cumsum(str_detect(x, ":"))) %>%
  filter(str_detect(x, "[0-9]") & group > 1) %>%
  separate_wider_delim(cols = x, names = c("dest", "source", "length"), delim = " ") %>%
  mutate(across(1:3, as.numeric))

seeds <- dd %>%
  filter(str_detect(x, "seeds:")) %>%
  mutate(x = str_remove(x, "seeds: ")) %>%
  pull(x) %>%
  str_split_1(pattern = " ") %>%
  as.numeric()

min(sapply(seeds, function(seed) {
  cur <- seed
  for(i in 2:8) {
    mm <- filter(maps, group == i)
    match <- which(mm$source <= cur & mm$source + mm$length - 1 >= cur)
    if(length(match) != 0) {
      cur <- mm$dest[match] + cur - mm$source[match]
    }
  }
  cur
}))

get_loc <- Vectorize(function(seed) {
  cur <- seed
  for(i in 2:8) {
    mm <- filter(maps, group == i)
    match <- which(mm$source <= cur & mm$source + mm$length - 1 >= cur)
    if(length(match) != 0) {
      cur <- mm$dest[match] + cur - mm$source[match]
    }
  }
  return(cur)
}, vectorize.args = "seed")

vv <- numeric(0)
for(jj in 1:10) {
  seedsmall <- seeds[c(2 * jj - 1, 2 * jj)]
  aa <- data.frame(level = 1, start = seedsmall[1], end =  sum(seedsmall)  - 1)

  aa <- aa %>%
    rowwise() %>%
    mutate(split = diff(sapply(c(start, end), get_loc)) != diff(c(start, end)),
           done = split)

  while(any(aa$split)) {

    rows <- which(aa$split)
    aa <- bind_rows(
      aa %>% mutate(done = TRUE),
      purrr::map_df(rows, function(x) {
        data.frame(level = c(aa$level[x] + 1, aa$level[x] + 1),
                   start = c(aa$start[x], ceiling( mean(c(aa$end[x], aa$start[x]) ) ) + 1),
                   end = c(ceiling( mean(c(aa$end[x], aa$start[x]) )), aa$end[x]),
                   done = FALSE,
                   split = NA
        )
      })
    )

    aa <- aa %>%
      rowwise() %>%
      mutate(split = case_when(
        done ~ FALSE,
        !done ~ (diff(sapply(c(start, end), get_loc)) != diff(c(start, end)) & end > start + 1)
      ),
      done = !split)
    print(nrow(aa))
  }
  vv <- c(vv, min(sapply(c(aa$start, aa$end), get_loc)))
  print(vv)
}
min(vv)







