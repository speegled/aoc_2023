library(tidyverse)
dd <- data.frame(x = read_lines("data/day_12_test"))
dd <- dd %>%
  separate_wider_delim(x, delim = " ", names = c("springs", "arr"))
ss <- dd$springs
aa <- dd$arr
toets <- 5
tt <- str_c(rep(ss[8], toets), collapse = "?")
pp <- str_c(rep(aa[8], toets), collapse = ",")
str <- tt
str
pp
#pp <- as.integer(str_split_1(pp, ","))

mem_count_poss <- memoise::memoise(count_poss)

system.time(sapply(1:12, function(jj) {
  print(jj)
  tt <- str_c(rep(ss[jj], toets), collapse = "?")
  pp <- str_c(rep(aa[jj], toets), collapse = ",")
  str <- tt
  mem_count_poss(str, pp)
}))
count_poss(str, pp)
# #str <- str_c(stringi::stri_reverse(tt), ".", collapse = "")
# #pp <- stringi::stri_reverse(pp)
# pp
# tt
# dd$springs[17]
# dd$arr[17]
count_poss <- function(str, pp, debug = F) {
  if(debug){browser()}
  str <- str_remove(str, "^[\\.]+")
  # print(str)
  # print(pp)
  # print(str_length(str))
  num <- as.integer(str_extract(pp, "[0-9]+"))
  if(is.na(num)) {
    # print("yep")
    # print(str)
    # print(pp)
    if(!str_detect(str, "\\#")) {
      # print("return 1")

      return(1)
    } else {
      # print(str)
      # print(pp)
      # print("yep")
      return(0)
    }
  }
  if(!str_detect(str, "[\\?\\#]")) {
    # print(pp)
    # print(str)
    return(0)
  }
  temp <- str_extract("##???ASF", "^[\\#]+(?=\\.)")
  if(!is.na(temp)) {
    if(str_length(temp) != num) {
      return(0)
    }
  }
  val <- str_length(str_extract(str, "^[\\?\\#]+"))

  # if(is.na(val)) {
  #   print("here")
  #   val <- 0
  # }
  val3 <- str_length(str_extract(str, "^[\\?]+(?=\\.)")) #this isn't working correctly
  if(is.na(val3)) {
    val3 <- Inf
  }
  val2 <- str_length(str_extract(str, "^[\\#]+"))
  # print(str)
  # print(val2)
  if(is.na(val2)) {val2 <- -Inf}
  if(val2 > num) {
    # print(str)
    # print(pp)
    return(0)
  } else if(val3 < num) {
    str <- str_replace(str, "\\?+", "") #this isn't working correctly
    count_poss(str, pp)
  } else if(val3 == num) {
    str2 <- str_sub(str, start = num + 2)
    #print(str2)
    pp2 <- str_remove(pp, "^[0-9]+[,]*")
    #just added
    #return(count_poss(str2, pp2))
    str3 <- str_replace(str, "^\\?+", "")
    return(count_poss(str3, pp) + count_poss(str2, pp2))
  } else if(val < num) {
    #print(str)
    # print(pp)
    return(0)
    # str <- str_remove(str, "^[\\#\\?]+")
    #
    # if(str_length(str) == 0) {
    #   return(0)
    # } else {
    #   count_poss(str, pp)
    # }
  } else if(val == num) {
    # print(str)
    # print(pp)
    str2 <- str_sub(str, start = num + 2)
    #print(str2)
    pp2 <- str_remove(pp, "^[0-9]+[,]*")
#just added
    return(count_poss(str2, pp2))
  } else{
    s1 <- str
    s2 <- str
    loc <- str_locate(s1, "\\?")[1]
    # print(str)
    # print(loc)
    if(!is.na(loc)) {
      str_sub(s1, loc, loc) <- "."
      str_sub(s2, loc, loc) <- "#"
      return(count_poss(s1, pp) + count_poss(s2, pp))
    } else {
      print("what")
    }
  }
}

dd[which(poss != poss2),]
str <- "?.#.###"
pp <- "1,3"
count_poss(str, pp, F)

dd[poss!= poss2 & poss2 < 2,]




sum(sapply(1:1000, function(j) {
  count_poss(ss[j], aa[j])
}))
 str
dd[17,]
count_poss(str, pp)
dstr
pp
poss2[17]
poss[17]
poss[17]
poss3 <- sapply(1:1000, function(j) {
  print(j)
  count_poss(ss[j], aa[j])
})

sum(poss3)







sum(.Last.value)
dd
j <- 1
sapply(1, function(j) {
  print(j)
  x <- dd$x[j]
  vals <- combinat::hcube(x = rep(2, str_count(x, "\\?")), scale = 1, translation = 0)
  needed <- as.integer(str_extract_all(x, "[0-9]+")[[1]])
  x <- str_remove(x, "[ 0-9,]+")
  additional <- sum(needed) - str_count(x, "\\#")
  sub_rows <- which(apply(vals, 1, function(x) sum(x == 2)) == additional)
  suppressWarnings(sum(sapply(sub_rows, function(i) {
    all(get_lengths(x, vals[i,]) == needed)
  })))
})




