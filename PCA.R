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
  adv = adv %>% select(-Pos, -Age, -Tm, -G, -MP)
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
  return(adv)
}
for (i in 1952:2020){
  year = i
  adv = adv_scrape(year)
  tot = tot_scrape(year)
  df = full_join(tot, adv, by = c("Player", "Yr"))
  write.csv(df, paste0('newdata/', year, '.csv'))
  df = read.csv(paste0("newdata/", year, ".csv"))[,-1] %>% select(Player, Yr, everything())
  for (i in 1:nrow(df)){df[i,6:ncol(df)] = as.double(df[i,6:ncol(df)])} # ensuring numeric values
  for (i in 1:nrow(df)){
    playersp = strsplit(df$Player[i], "")[[1]]
    letters = c()
    for (j in 1:length(playersp)){if (playersp[j] == "*"){letters = letters} else{letters = c(letters, playersp[j])}}
    df$Player[i] = paste(letters, collapse = "")
  } # taking away asterisks for HOF players
  lgAvg3P = sum(df$X3P)/sum(df$X3PA);lgAvg2P = sum(df$X2P)/sum(df$X2PA);lgAvgMP = sum(df$MP)/nrow(df);lgAvgG = max(df$G)
  
  fdf = df %>% transmute(Player, Yr, x3PAVG = ((X3P. - lgAvg3P)*(X3PA)),x2PAVG = ((X2P. - lgAvg2P)*(X2PA)),xFTAVG = ((FT. - lgAvgFT)*(FTA)),PTS = PTS/G,TRB = TRB/G,AST = AST/G,STL = STL/G,BLK = BLK/G,ASTtTOV = TOV/G,SPBtPFR = (STL + BLK)/PF,PER = PER*(MP/(lgAvgMP))*(G/lgAvgG),WS = WS/G,OWS = OWS/G,DWS = DWS/G,TS = TS.,FTr = FTr)
  for (i in 1:nrow(fdf)){if (fdf$ASTtTOV[i] != 0){fdf$ASTtTOV[i] = (fdf$AST[i])/(fdf$ASTtTOV[i])} else{fdf$ASTtTOV[i] = 0}}
  for (i in 1:nrow(fdf)){if (is.infinite(fdf$SPBtPFR[i])){fdf$SPBtPFR[i] = 0} else{fdf$SPBtPFR[i] = fdf$SPBtPFR[i]}}
  fdf[3:ncol(fdf)] = scale(fdf[3:ncol(fdf)])
  for (i in 1:nrow(fdf)){for (j in 1:ncol(fdf)){if (is.nan(fdf[i,j])){fdf[i,j] = 0}}}
  eff = c(3, 4, 5, 11, 12, 17, 18);names(fdf)[eff]
  vol = c(6, 7, 8, 9, 10, 13, 14, 15, 16);names(fdf)[vol]
  
  R1 = cov(fdf[,eff]);lam1 = eigen(R1)$values;loa1 = abs(eigen(R1)$vectors);d1 = cbind.data.frame(names(fdf)[eff], loa1[,1]);names(d1) = c("stat", "loading")
  R2 = cov(fdf[,vol]);lam2 = eigen(R2)$values;loa2 = abs(eigen(R2)$vectors);d2 = cbind.data.frame(names(fdf)[vol], loa2[,1]);names(d2) = c("stat", "loading")
  loadings = rbind.data.frame(d1, d2) %>% cbind(year)
  write.csv(loadings, paste0('newdata/', year, 'loadings.csv'))
  
  Eff = c();Vol = c()
  for (i in 1:nrow(fdf)){
    Eff = c(Eff, t(abs(loa1[,1]))%*%t(as.matrix(fdf[i,eff])))
    Vol = c(Vol, t(abs(loa2[,1]))%*%t(as.matrix(fdf[i,vol])))
  }
  fdf_ = cbind.data.frame(fdf, Eff, Vol)
  fdf_$Player = iconv(fdf_$Player, to="ASCII//TRANSLIT")
  as.list <- read.csv(paste0('aslists/as', year, '.csv'))$Player
  for (i in 1:nrow(fdf_)){if (fdf_$Player[i] %in% as.list){fdf_$allstar[i] = 1} else{fdf_$allstar[i] = 0}}
  finaldf = fdf_ %>% select(Player, Eff, Vol, allstar)
  write.csv(finaldf, paste0('newdata/', year, 'PC.csv'))
}

# Bind all PC dataframes into one dataset
fin = NULL
for (i in 1952:2020){
  fin = rbind.data.frame(fin, cbind.data.frame(read.csv(paste0('newdata/', i, 'PC.csv'))[,-1], i))
}
fin = fin %>% drop_na()
for (i in 1:nrow(fin)){
  if (fin$Player[i] == "Nene"){
    fin$Player[i] = "Nene Hilario"
  }
}
write.csv(fin, 'finaldf.csv')














