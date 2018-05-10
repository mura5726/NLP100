# package

library(knitr)
library(dplyr)
library(stringr)
library(stringi)
knitr::opts_chunk$set(comment = NA)

# 00. 文字列の逆順
# 文字列"stressed"の文字を逆に（末尾から先頭に向かって）並べた文字列を得よ．

text <-
  str_split("stressed", pattern = "") %>%
    unlist() %>%
    t() 

text[length(text):1] %>% 
  str_c(collapse = "")

