library(rvest)
library(tidyverse)
options(stringsAsFactors = F)

loadings = read.csv('newdata/loadings.csv')[,-1]
levels(as.factor(loadings$stat))
loadings %>% ggplot(aes(x = year, y = loading)) + geom_bar(stat = "identity", aes(fill = stat), position = "stack") + scale_x_continuous(breaks = seq(1955,2020,5)) + ggtitle("Share of Loadings") + scale_y_continuous('Loading Value', breaks = 1:5) + theme_bw()


