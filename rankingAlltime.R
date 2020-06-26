library(rvest)
library(tidyverse)
options(stringsAsFactors = F)

finaldf = read.csv("finaldf.csv")[,-1]
x1bar = finaldf %>% filter(allstar == 1) %>% summarise(x1 = mean(Eff),x2 = mean(Vol)) %>% as.matrix() %>% t()
x2bar = finaldf %>% filter(allstar == 0) %>% summarise(x1 = mean(Eff),x2 = mean(Vol)) %>% as.matrix() %>% t()
S1 = finaldf %>% filter(allstar == 1) %>% select(Eff, Vol) %>% cov()
S2 = finaldf %>% filter(allstar == 0) %>% select(Eff, Vol) %>% cov()
Sp = ((finaldf %>% filter(allstar == 1) %>% nrow() - 1)*S1 + (finaldf %>% filter(allstar == 0) %>% nrow() - 1)*S2)/(finaldf %>% filter(allstar == 0) %>% nrow() + finaldf %>% filter(allstar == 1) %>% nrow() - 2)
w = solve(Sp)%*%(x1bar - x2bar)

for (i in 1:nrow(finaldf)){
  finaldf$Score[i] = (t(w))%*%c(finaldf$Eff[i], finaldf$Vol[i])
}

finaldf %>% arrange(desc(Score)) %>% select(Player, Yr, Score) %>% head(25)
ord = finaldf %>% group_by(Player) %>% summarise(sumSc = sum(Score)) %>% arrange(desc(sumSc))
ord$Player %>% head(25)



