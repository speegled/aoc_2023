library(tidyverse)
dd <- data.frame(x = read_lines("data/day_08"))

directions <- str_split_1(dd$x[1], pattern = "")
dd <- filter(dd, str_detect(x, "\\("))
dd <- dd %>% separate(col = x, into = c("source", "left", "right", "garbage"), sep = "[, =\\(\\)]+") %>%
  select(-garbage)
vecleft <- dd$left
vecright <- dd$right
names(vecleft) <- dd$source
names(vecright) <- dd$source

plus <- function(x) {
  val <- (x + 1) %% length(directions)
  ifelse(val == 0, length(directions), val)
}

cur <- "AAA"
steps <- 0
ll <- length(directions)

while(cur != "ZZZ") {
  steps <- steps + 1
  val <- steps %% ll
  val <- ifelse(val == 0, ll, val )
  if(directions[val] == "L") {
    cur <- vecleft[cur]
  } else {
    cur <- vecright[cur]
  }
  if(steps %% 5000 == 0) {
    print(steps)
  }
}
steps


stop_cond <- function(cur) {
  all(str_extract(cur, "[A-Z]$") == "Z")
}

all_cur <- names(vecleft)[(str_extract(names(vecleft), "[A-Z]$") == "A")]

vals <- sapply(all_cur, function(cur) {
  steps <- 0
  while(!stop_cond(cur)) {
    steps <- steps + 1
    val <- steps %% ll
    val <- ifelse(val == 0, ll, val )
    if(directions[val] == "L") {
      cur <- vecleft[cur]
    } else {
      cur <- vecright[cur]
    }
    if(steps %% 10000 == 0) {
      print(steps)
    }
  }
  steps
})

steps <- vals[1]
for(i in 2:6) {
  steps <- pracma::Lcm(steps, vals[i])
}
print(steps, digits = 20)


