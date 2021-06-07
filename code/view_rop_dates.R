

library(tidyverse)


zyear = 2020
rop_dates <- read.csv("data/rop_dates.csv") %>% 
  filter(year == zyear) %>% 
  mutate(across(contains("date"), mdy)) %>% 
  mutate(rop.mid = end.date - 1,
         rop = paste("rop", rop, sep = ".")) 
