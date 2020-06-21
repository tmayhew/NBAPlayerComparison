library(tidyverse)
d = read.csv('finaldf.csv')[,-1]
d %>% filter(Player == "Manu Ginobili")
