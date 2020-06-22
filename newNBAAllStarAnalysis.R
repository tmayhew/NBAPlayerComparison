library(rvest)
library(tidyverse)
options(stringsAsFactors = F)

loadings = read.csv('newdata/loadings.csv')[,-1]
levels(as.factor(loadings$stat))
volnames = c("AST", "BLK", "DWS", "OWS", "PER", "PTS", "STL", "TRB", "WS")
effnames = c("ASTtTOV", "FTr", "SPBtPFR", "TS", "x2PAVG", "x3PAVG", "xFTAVG")
loadings %>% filter(stat %in% volnames) %>% ggplot(aes(x = year, y = loading)) + geom_bar(stat = "identity", aes(fill = stat), position = "stack") + scale_x_continuous(breaks = seq(1955,2020,5)) + ggtitle("Share of Loadings - Volume") + scale_y_continuous('Loading Value', breaks = 1:5) + theme_bw()
loadings %>% filter(stat %in% effnames) %>% ggplot(aes(x = year, y = loading)) + geom_bar(stat = "identity", aes(fill = stat), position = "stack") + scale_x_continuous(breaks = seq(1955,2020,5)) + ggtitle("Share of Loadings - Efficiency") + scale_y_continuous('Loading Value', breaks = 1:5) + theme_bw()



