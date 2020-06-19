library(rvest)
library(tidyverse)
options(stringsAsFactors = F)
adv_scrape = function(year){
  link1 = paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_advanced.html")
  html = read_html(link1)
  adv_ = html_table(html)[[1]]
  adv_ = adv_[,-c(which(names(adv_) == ""))]
  adv_ = adv_[-which(adv_$Rk == "Rk"),]
  
  adv = adv_[,-1]
  for (i in 1:nrow(adv)){
    for (j in 1:ncol(adv)){
      if (adv[i,j] == ""){
        adv[i,j] = 0
      } else{
        adv[i,j] = adv[i,j]
      }
    }
  }
  adv$Yr = year
  adv = adv %>% distinct(Player, .keep_all = T)
  adv = adv %>% select(Player, Yr, PER, WS, OWS, DWS, `TS%`, FTr)
  return(adv)
}
tot_scrape = function(year){
  link1 = paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_totals.html")
  html = read_html(link1)
  adv_ = html_table(html)[[1]]
  adv_ = adv_[-which(adv_$Rk == "Rk"),]
  
  adv = adv_[,-1]
  for (i in 1:nrow(adv)){
    for (j in 1:ncol(adv)){
      if (adv[i,j] == ""){
        adv[i,j] = 0
      } else{
        adv[i,j] = adv[i,j]
      }
    }
  }
  adv$Yr = year
  adv = adv %>% distinct(Player, .keep_all = T)
  adv = adv %>% select(Player, Yr, PTS, TRB, AST, G, MP, `3P`, `3PA`, `3P%`, `2P`, `2PA`, `2P%`)
  return(adv)
}
for (i in 1952:2020){
  year = i
  adv = adv_scrape(year)
  tot = tot_scrape(year)
  df = full_join(tot, adv, by = c("Player", "Yr"))
  for (i in 1:nrow(df)){
    df[i,3:ncol(df)] = as.double(df[i,3:ncol(df)])
  }
  write.csv(df, paste0('datasets/d', year, '.csv'))
  df = read.csv(paste0('datasets/d', year, '.csv'))[,-1]
  for (i in 1:nrow(df)){
    playersp = strsplit(df$Player[i], "")[[1]]
    letters = c()
    for (j in 1:length(playersp)){
      if (playersp[j] == "*"){
        letters = letters
      } else{
        letters = c(letters, playersp[j])
      }
    }
    df$Player[i] = paste(letters, collapse = "")
  }
  
  lgAvg3P = sum(df$X3P)/sum(df$X3PA)
  lgAvg2P = sum(df$X2P)/sum(df$X2PA)
  lgAvgMP = sum(df$MP)/nrow(df)
  lgAvgG = max(df$G)
  fdf = df %>% transmute(Player, Yr, x3PAVG = ((X3P. - lgAvg3P)*(X3PA)),x2PAVG = ((X2P. - lgAvg2P)*(X2PA)),PTS = PTS/G,TRB = TRB/G,AST = AST/G,PER = PER*(MP/(lgAvgMP))*(G/lgAvgG),WS = WS/G,OWS = OWS/G,DWS = DWS/G,TS = TS.,FTr = FTr)
  
  ## Principal Components Analysis
  fdf[3:ncol(fdf)] = scale(fdf[3:ncol(fdf)])
  if (all(is.nan(fdf$x3PAVG))){
    fdf$x3PAVG = 0
  }
  R1 = cov(fdf[,c(3, 4, 12, 13)])
  R2 = cov(fdf[,c(5, 6, 7, 8, 9, 10, 11)])
  lam1 = eigen(R1)$values
  lam2 = eigen(R2)$values
  loa1 = eigen(R1)$vectors
  loa2 = eigen(R2)$vectors
  PC1 = c()
  PC2 = c()
  for (i in 1:nrow(fdf)){
    PC1 = c(PC1, t(abs(loa1[,1]))%*%t(as.matrix(fdf[i,c(3, 4, 12, 13)])))
    PC2 = c(PC2, t(abs(loa2[,1]))%*%t(as.matrix(fdf[i,c(5, 6, 7, 8, 9, 10, 11)])))
  }
  fdf_ = cbind.data.frame(fdf, PC1, PC2)
  fdf_$Player = iconv(fdf_$Player, from="UTF-8",to="ASCII//TRANSLIT")
  as.list <- read.csv(paste0('aslists/as', year, '.csv'))$Player
  for (i in 1:nrow(fdf_)){
    if (fdf_$Player[i] %in% as.list){
      fdf_$allstar[i] = 1
    } else{
      fdf_$allstar[i] = 0
    }
  }
  finaldf = fdf_ %>% select(Player, PC1, PC2, allstar)
  write.csv(finaldf, paste0('datasets/f', year, '.csv'))
}

# Bind all PC dataframes into one dataset
fin = NULL
for (i in 1952:2020){
  fin = rbind.data.frame(fin, cbind.data.frame(read.csv(paste0('datasets/f', i, '.csv'))[,-1], i))
}
fin = fin %>% drop_na()
write.csv(fin, 'finaldf.csv')














