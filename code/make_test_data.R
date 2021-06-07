
library(tidyverse)
library(lubridate)

  set.seed(5)
  
  zyear = 2020

  s123 <- read.csv("data/downloaded/HEP_2020_0.csv") 

  nest.increase <- c(rep(3, 2), rep(5, 2), rep(8, 2), rep(10, 14))
  
  nest.nudge = round(rnorm(20), 0)

  obs.nest.increase = nest.increase + nest.nudge
  
  prop.stage = 0.9
  prop.brood = 0.8
  
  
  props <- data.frame(zmonth =       c(  2,   3,   4,   5,   6,   7),
                      prop.stage.1 = c(0.2, 0.1, 0.0, 0.0, 0.0, 0.0),
                      prop.stage.2 = c(0.0, 0.6, 0.2, 0.1, 0.0, 0.0),
                      prop.stage.3 = c(0.0, 0.0, 0.4, 0.3, 0.2, 0.0),
                      prop.stage.4 = c(0.0, 0.0, 0.4, 0.6, 0.2, 0.1),
                      prop.stage.5 = c(0.0, 0.0, 0.0, 0.0, 0.6, 0.2))
  
  

  
  
    
test_data <- s123 %>% 
  filter(useforsummary == "y") %>% 
  mutate(zdate = as.Date(as.POSIXct(Start.Time, format = "%m/%d/%Y %I:%M:%S %p", tz = "GMT")),
         zmonth = month(zdate)) %>% 
  distinct(zdate, .keep_all = TRUE) %>%
  sample_n(size = 20, replace = FALSE) %>% 
  arrange(Start.Time) %>% 
  left_join(., props) %>% 
  mutate(nest.increase = obs.nest.increase,
         ï..ObjectID = NA,
         GlobalID = NA,
         Select.Colony = 599,
         Recording.Observer = "ScottJennings",
         Observer.2 = "DavidLumpkin",
         Observer.3 = NA,
         Observer.4 = NA,
         Observer.5 = NA,
         Observer.6 = NA,
         Other.Observer.Name. = NA,
         Great.Blue.Heron.Colony.Count = nest.increase,
         Great.Egret.Colony.Count = nest.increase * 6,
         Black.Crowned.Night.Heron.Colony.Count = nest.increase * 4,
         Cattle.Egret.Colony.Count = nest.increase * 3,
         Snowy.Egret.Colony.Count = nest.increase * 5,
         Double.Crested.Cormorant.Colony.Count = nest.increase,
         Number.of.stage.1.GBHE.nests = floor(Great.Blue.Heron.Colony.Count * prop.stage * prop.stage.1),
         Number.of.stage.2.GBHE.nests =  floor(Great.Blue.Heron.Colony.Count * prop.stage * prop.stage.2),
         Number.of.stage.3.GBHE.nests =  floor(Great.Blue.Heron.Colony.Count * prop.stage * prop.stage.3),
         Number.of.stage.4.GBHE.nests =  floor(Great.Blue.Heron.Colony.Count * prop.stage * prop.stage.4),
         Number.of.stage.5.GBHE.nests =  floor(Great.Blue.Heron.Colony.Count * prop.stage * prop.stage.5),
         Number.of.stage.1.GREG.nests =  floor(Great.Egret.Colony.Count * prop.stage * prop.stage.1),
         Number.of.stage.2.GREG.nests =  floor(Great.Egret.Colony.Count * prop.stage * prop.stage.2),
         Number.of.stage.3.GREG.nests =  floor(Great.Egret.Colony.Count * prop.stage * prop.stage.3),
         Number.of.stage.4.GREG.nests =  floor(Great.Egret.Colony.Count * prop.stage * prop.stage.4),
         Number.of.stage.5.GREG.nests =  floor(Great.Egret.Colony.Count * prop.stage * prop.stage.5),
         GBHE.Nests.with.1.chick = floor(Number.of.stage.4.GBHE.nests * prop.brood * 0.3),
         GBHE.Nests.with.2.chicks = floor(Number.of.stage.4.GBHE.nests * prop.brood * 0.3),
         GBHE.Nests.with.3.chicks = floor(Number.of.stage.4.GBHE.nests * prop.brood * 0.3),
         GBHE.Nests.with.4.chicks = floor(Number.of.stage.4.GBHE.nests * prop.brood * 0.1),
         GBHE.Nests.with.5.chicks = floor(Number.of.stage.4.GBHE.nests * prop.brood * 0.01),
         GREG.Nests.with.1.chick = floor(Number.of.stage.4.GREG.nests * prop.brood * 0.3),
         GREG.Nests.with.2.chicks = floor(Number.of.stage.4.GREG.nests * prop.brood * 0.3),
         GREG.Nests.with.3.chicks = floor(Number.of.stage.4.GREG.nests * prop.brood * 0.3),
         GREG.Nests.with.4.chicks = floor(Number.of.stage.4.GREG.nests * prop.brood * 0.1),
         GREG.Nests.with.5.chicks = floor(Number.of.stage.4.GREG.nests * prop.brood * 0.01),
         useforsummary = "y") %>% 
  select(-zdate, -zmonth, -nest.increase, -contains("prop.stage"))

saveRDS(test_data, "data/downloaded/test_data")


                                                                                                                          

#####################


test_nest_abund <- expand.grid(year = seq(2000, 2019),
                              species = c("GREG", "GBHE", "SNEG", "BCNH", "CAEG", "DCCO")) %>% 
  mutate(code = 599,
         peakactvnsts = c(rpois(20, 66), rpois(20, 11), rpois(20, 55), rpois(20, 44), rpois(20, 33), rpois(20, 11)))


saveRDS(test_nest_abund, "data/test_nest_abund")

