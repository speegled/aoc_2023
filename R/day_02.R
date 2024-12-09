library(tidyverse)
dd <- data.frame(x = readLines(here::here("data/day_02")))
dd %>%
  separate_longer_delim(x, delim = ";") %>%
  mutate(game = cumsum(str_detect(x, "Game")),
         x = str_remove(x, "Game [0-9]+:[ ]*")) %>%
  separate_longer_delim(x, delim = ",") %>%
  mutate(x = str_remove(x, "^[ ]*")) %>%
  separate(x, sep = " ", into = c("num", "color"), convert = T) %>%
  group_by(game, color) %>%
  summarize(max = max(num)) %>%
  group_by(game) %>%
  mutate(legal = first(max) < 15 && nth(max, 2) < 14 && last(max) < 13) %>%
  summarize(legal = all(legal)) %>%
  filter(legal) %>%
  pull(game) %>% sum()

dd %>%
  separate_longer_delim(x, delim = ";") %>%
  mutate(game = cumsum(str_detect(x, "Game")),
         x = str_remove(x, "Game [0-9]+:[ ]*")) %>%
  separate_longer_delim(x, delim = ",") %>%
  mutate(x = str_remove(x, "^[ ]*")) %>%
  separate(x, sep = " ", into = c("num", "color"), convert = T) %>%
  group_by(game, color) %>%
  summarize(max = max(num)) %>%
  summarize(val = prod(max)) %>%
  pull(val) %>% sum()
