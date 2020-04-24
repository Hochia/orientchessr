library(tidyverse)

df <- tibble(name = "name", x = 1, y = 1)

cba <-
  tibble(x = rep(1:9, 10),
         y = rep(1:10, each = 9)) %>%
  mutate(xy = str_c("(", x, ",", y, ")"))

cl <- tibble(sx = c(rep(1, 11), rep(4, 4), 9, rep(2:8, 2)),
             sy = c(1:10, 1, 1, 3, 8, 10, 1, rep(c(1, 6), each = 7)),
             ex = c(rep(9, 10), 1, rep(6, 4), 9, rep(2:8, 2)),
             ey = c(1:10, 10, 3, 1, 10, 8, 10, rep(c(5, 10), each = 7)))

ccname <- c("帥", "仕", "相", "俥", "傌", "炮", "兵")

cp <- tibble(name = c("帥", rep(c("仕", "相", "俥", "傌", "炮"), each = 2), rep("兵", 5),
                      "將", rep(c("士", "象", "車", "馬", "包"), each = 2), rep("卒", 5)),
             p1 = rep(c(TRUE, FALSE), each = 16),
             x = rep(c(5, 4, 6, 3, 7, 1, 9, 2, 8, 2, 8, 1, 3, 5, 7, 9), 2),
             y = c(rep(1, 9), rep(3, 2), rep(4, 5), rep(10, 9), rep(8, 2), rep(7, 5)))

usethis::use_data(cba, cl, df, ccname, cp, internal = TRUE, overwrite = T)
