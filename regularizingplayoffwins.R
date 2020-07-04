library(tidyverse)
options(stringsAsFactors = F)
tc = read.csv("playoffwins/teamcodes.csv")[,-1]

'for (year in 1950:2019){
  d = read.csv(paste0("playoffwins/", year, ".csv"))[,-1]
  maxW = max(d$W)
  d$W = d$W/maxW
  for (i in 1:nrow(d)){d$Tm[i] = tc$Abb[which(tc$Tm == d$Tm[i])]}
  d$Yr = year
  write.csv(d, paste0("playoffwins/", year, ".csv"))
}'
fin = NULL
for (year in 1950:2019){
  fin = rbind.data.frame(fin, read.csv(paste0("playoffwins/", year, ".csv"))[,-1])
}
write.csv(arrange(fin, desc(W), desc(Yr)), 'playoffwins/tmPlayoffs.csv')

read.csv('playoffwins/tmPlayoffs.csv')[,-1]


