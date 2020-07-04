library(rvest)
library(tidyverse)
options(stringsAsFactors = F)

adv_scrape = function(year){
  link1 = paste0("https://www.basketball-reference.com/playoffs/NBA_", year, "_advanced.html")
  html = read_html(link1)
  adv_ = html_table(html)[[1]]
  adv_ = adv_[,-c(which(names(adv_) == ""))]
  adv = adv_[-which(adv_$Rk == "Rk"),]
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
  adv = adv %>% distinct(Rk, .keep_all = T)
  adv = adv %>% select(-Pos, -Age, -Tm, -G, -MP, -Rk)
  for (i in 2:nrow(adv)){
    if (adv$Player[i] == adv$Player[i-1]){
      adv$Player[i] = paste(adv$Player[i], "(2)", collapse = "")
    } else{
      adv$Player[i] = adv$Player[i]
    }
  }
  return(adv)
}
tot_scrape = function(year){
  link1 = paste0("https://www.basketball-reference.com/playoffs/NBA_", year, "_totals.html")
  html = read_html(link1)
  tot_ = html_table(html)[[1]]
  tot = tot_[-which(tot_$Rk == "Rk"),]
  for (i in 1:nrow(tot)){
    for (j in 1:ncol(tot)){
      if (tot[i,j] == ""){
        tot[i,j] = 0
      } else{
        tot[i,j] = tot[i,j]
      }
    }
  }
  tot$Yr = year
  if (length(which(names(tot) == "FT")) == 2){
    names(tot)[which(names(tot) == "FT")[2]] = "FT."
  }
  tot = tot %>% distinct(Rk, .keep_all = T)
  tot = tot[,-1]
  for (i in 2:nrow(tot)){
    if (tot$Player[i] == tot$Player[i-1]){
      tot$Player[i] = paste(tot$Player[i], "(2)", collapse = "")
    } else{
      tot$Player[i] = tot$Player[i]
    }
  }
  
  return(tot)
}
g1 = read.csv('newdata/G1loadings.csv')[,-1]
g2 = read.csv('newdata/G2loadings.csv')[,-1]
g3 = read.csv('newdata/G3loadings.csv')[,-1]
g4 = read.csv('newdata/G4loadings.csv')[,-1]

for (i in 1952:2019){
  year = i
  adv = adv_scrape(year)
  tot = tot_scrape(year)
  df = full_join(tot, adv, by = c("Player", "Yr"))
  write.csv(df, paste0('newdata/', year, 'playoffs.csv'))
  df = read.csv(paste0("newdata/", year, "playoffs.csv"))[,-1] %>% select(Player, Yr, everything())
  for (i in 1:nrow(df)){df[i,6:ncol(df)] = as.double(df[i,6:ncol(df)])} # ensuring numeric values
  for (i in 1:nrow(df)){
    playersp = strsplit(df$Player[i], "")[[1]]
    letters = c()
    for (j in 1:length(playersp)){if (playersp[j] == "*"){letters = letters} else{letters = c(letters, playersp[j])}}
    df$Player[i] = paste(letters, collapse = "")
  } # taking away asterisks for HOF players
  lgAvg3P = sum(df$X3P)/sum(df$X3PA);lgAvg2P = sum(df$X2P)/sum(df$X2PA);lgAvgMP = sum(df$MP)/nrow(df)
  if (any(names(df) == "FT.1")){
    names(df)[which(names(df) == "FT.1")] = "FT."
  }
  fdf = df %>% transmute(Player, Yr, x3PAVG = ((X3P. - lgAvg3P)*(X3PA)),x2PAVG = ((X2P. - lgAvg2P)*(X2PA)),xFTAVG = ((FT. - lgAvgFT)*(FTA)),PTS = PTS/G,TRB = TRB/G,AST = AST/G,STL = STL/G,BLK = BLK/G,PER = PER*(MP/(lgAvgMP)),WS = WS/G,OWS = OWS/G,DWS = DWS/G, Tm)
  original = fdf %>% transmute(PTS, TRB, AST, BLK, STL, adj.PER = PER, WS.G = WS, OWS.G = OWS, DWS.G = DWS, x3PAVG = x3PAVG, x2PAVG = x2PAVG, xFTAVG = xFTAVG)
  names(original) = paste0("orig.", names(original))
  for (i in 1:nrow(original)){for (j in 1:ncol(original)){if (is.nan(original[i,j])){original[i,j] = 0}}}
  
  fdf[3:(ncol(fdf)-1)] = scale(fdf[3:(ncol(fdf)-1)], center = F)
  for (i in 1:nrow(fdf)){for (j in 1:ncol(fdf)){if (is.nan(fdf[i,j])){fdf[i,j] = 0}}}
  
  if (fdf$Yr[1] < 1974){
    loa = g1$loading[c(1:3,8:16)]
    loa[1:3] = 0.50*(loa[1:3])
  } else if (fdf$Yr[1] < 1978){
    loa = g2$loading[c(1:3,8:16)]
    loa[1:3] = 0.50*(loa[1:3])
  } else if (fdf$Yr[1] < 1980){
    loa = g3$loading[c(1:3,8:16)]
    loa[1:3] = 0.50*(loa[1:3])
  } else{
    loa = g4$loading[c(1:3,8:16)]
    loa[1:3] = 0.50*(loa[1:3])
  }
  Contrib = c()
  for (i in 1:nrow(fdf)){
    Contrib = c(Contrib, t(abs(loa))%*%t(as.matrix(fdf[i,3:14])))
  }
  fdf_ = cbind.data.frame(fdf, Contrib)
  
  fdf_$Player = iconv(fdf_$Player, to="ASCII//TRANSLIT")
  finaldf = fdf_ %>% select(Player, Contrib, everything())
  finaldf = cbind.data.frame(finaldf, original)
  finaldf = left_join(finaldf, read.csv('playoffwins/tmPlayoffs.csv')[,-1], by = c("Tm","Yr"))
  finaldf$W[which(is.na(finaldf$W))] = 0
  contribTable = finaldf %>% group_by(Tm) %>% summarise(sumContrib = sum(Contrib), .groups = "drop")
  finaldf = full_join(finaldf, contribTable, by = c("Tm")) %>% arrange(Tm)
  finaldf$Contrib[which(finaldf$Contrib < 0)] = 0
  meanContrib = mean(contribTable$sumContrib)
  finaldf = finaldf %>% mutate(playoffSc = (Contrib*W)/meanContrib, playoffShare = (Contrib/sumContrib)*W) %>% select(Player, Contrib, Yr, Tm, W, sumContrib, playoffSc, playoffShare, everything())
  write.csv(finaldf, paste0('newdata/', year, 'playoffsPC.csv'))
}

fin = NULL
for (i in 1952:2019){fin = rbind.data.frame(fin, cbind.data.frame(read.csv(paste0('newdata/', i, 'playoffsPC.csv'))[,-1], i))}
fin = fin %>% drop_na()
for (i in 1:nrow(fin)){
  if (fin$Player[i] == "Nene"){
    fin$Player[i] = "Nene Hilario"
  }
  if (fin$Player[i] == "Gheorghe Mure<U+0219>an"){
    fin$Player[i] = "Gheorghe Muresan"
  }
  if (fin$Player[i] == "Horacio Llamas"){
    fin$Player[i] = "Horacio Llamas Grey"
  }
  if (fin$Player[i] == "Kiwane Lemorris Garris"){
    fin$Player[i] = "Kiwane Garris"
  }
  if (fin$Player[i] == "Mark Baker"){
    fin$Player[i] = "LaMark Baker"
  }
  if (fin$Player[i] == "Efthimis Rentzias"){
    fin$Player[i] = "Efthimi Rentzias"
  }
  if (fin$Player[i] == "Gigi Datome"){
    fin$Player[i] = "Luigi Datome"
  }
  if (fin$Player[i] == "Mo Bamba"){
    fin$Player[i] = "Mohamed Bamba"
  }
  if (fin$Player[i] == "Taurean Prince"){
    fin$Player[i] = "Taurean Waller-Prince"
  }
}
for (i in 1:nrow(fin)){
  if (fin$Player[i] == "Chris Wright" & fin$Yr[i] == 2013){
    fin$Player[i] = "Chris Wright (2)"
  }
  if (fin$Player[i] == "Sam Williams" & fin$Yr[i] >= 1980){
    fin$Player[i] = "Sam Williams (2)"
  }
  if (fin$Player[i] == "Reggie Williams" & fin$Yr[i] >= 1998){
    fin$Player[i] = "Reggie Williams (2)"
  }
  if (fin$Player[i] == "Greg Smith" & fin$Yr[i] >= 1996){
    fin$Player[i] = "Greg Smith (2)"
  }
  if (fin$Player[i] == "Jack Turner" & fin$Yr[i] >= 1960){
    fin$Player[i] = "Jack Turner (2)"
  }
  if (fin$Player[i] == "Chris Smith" & fin$Yr[i] >= 1996){
    fin$Player[i] = "Chris Smith (2)"
  }
  if (fin$Player[i] == "Walker Russell" & fin$Yr[i] >= 1989){
    fin$Player[i] = "Walker Russell (2)"
  }
  if (fin$Player[i] == "Jim Paxson" & fin$Yr[i] >= 1959){
    fin$Player[i] = "Jim Paxson (2)"
  }
  if (fin$Player[i] == "George King" & fin$Yr[i] >= 1959){
    fin$Player[i] = "George King (2)"
  }
  if (fin$Player[i] == "Mark Jones" & fin$Yr[i] >= 1987){
    fin$Player[i] = "Mark Jones (2)"
  }
  if (fin$Player[i] == "Bobby Jones" & fin$Yr[i] >= 1987){
    fin$Player[i] = "Bobby Jones (2)"
  }
  if (fin$Player[i] == "Dee Brown" & fin$Yr[i] >= 2003){
    fin$Player[i] = "Dee Brown (2)"
  }
  if (fin$Player[i] == "Mark Davis" & fin$Yr[i] >= 1990){
    fin$Player[i] = "Mark Davis (2)"
  }
  if (fin$Player[i] == "Mike Davis" & fin$Yr[i] >= 1980){
    fin$Player[i] = "Mike Davis (2)"
  }
  if (fin$Player[i] == "Mike Dunleavy" & fin$Yr[i] >= 1991){
    fin$Player[i] = "Mike Dunleavy Jr."
  }
  if (fin$Player[i] == "Patrick Ewing" & fin$Yr[i] >= 2003){
    fin$Player[i] = "Patrick Ewing Jr."
  }
  if (fin$Player[i] == "Cedric Henderson" & fin$Yr[i] >= 1988){
    fin$Player[i] = "Cedric Henderson (2)"
  }
  if (fin$Player[i] == "Gerald Henderson" & fin$Yr[i] >= 1994){
    fin$Player[i] = "Gerald Henderson (2)"
  }
  if (fin$Player[i] == "Luke Jackson" & fin$Yr[i] >= 1994){
    fin$Player[i] = "Luke Jackson (2)"
  }
  if (fin$Player[i] == "Mike James" & fin$Yr[i] >= 2015){
    fin$Player[i] = "Mike James (2)"
  }
  if (fin$Player[i] == "Chris Johnson" & fin$Yr[i] >= 2014){
    fin$Player[i] = "Chris Johnson (2)"
  }
  if (fin$Player[i] == "Eddie Johnson" & fin$Yr[i] >= 1988){
    fin$Player[i] = "Eddie Johnson (2)"
  }
  if (fin$Player[i] == "George Johnson" & fin$Yr[i] >= 1979){
    fin$Player[i] = "George Johnson (2)"
  }
  if (fin$Player[i] == "Ken Johnson" & fin$Yr[i] >= 2002){
    fin$Player[i] = "Ken Johnson (2)"
  }
  if (fin$Player[i] == "Larry Johnson" & fin$Yr[i] >= 1979){
    fin$Player[i] = "Larry Johnson (2)"
  }
  if (grepl("Charles Jones", fin$Player[i]) & fin$Yr[i] >= 1999){
    fin$Player[i] = "Charles Jones (3)"
  }
  if (grepl("Charles Smith", fin$Player[i]) & fin$Yr[i] >= 1998){
    fin$Player[i] = "Charles Smith (3)"
  }
  if (grepl("Michael Smith", fin$Player[i]) & fin$Yr[i] >= 1996){
    fin$Player[i] = "Michael Smith (2)"
  }
}

fin$Player[which(grepl("Eddie Johnson",fin$Player)&fin$Yr==1986)] = "Eddie Johnson (2)"
fin$Player[which(grepl("Charles Jones",fin$Player)&fin$Yr==1985)][1] = "Charles Jones"
fin$Player[which(grepl("Charles Jones",fin$Player)&fin$Yr==1985)][2] = "Charles Jones (2)"
fin$Player[which(grepl("Charles Smith",fin$Player)&fin$Yr==1996)][1] = "Charles Smith"
fin$Player[which(grepl("George Johnson",fin$Player)&(fin$Yr==1975))][1] = "George Johnson (2)"
fin$Player[which(grepl("George Johnson",fin$Player)&(fin$Yr==1976))][1] = "George Johnson (2)"
fin$Player[which(grepl("George Johnson",fin$Player)&(fin$Yr==1981))][2] = "George Johnson (3)"
fin$Player[which(grepl("George Johnson",fin$Player)&(fin$Yr==1985))][2] = "George Johnson (3)"

mismatch = unique(fin$Player[(fin$Player %in% cdf$names) == F])
fin$issue = ifelse(fin$Player %in% mismatch, 1, 0)
for (i in 1:nrow(fin)){
  if (fin$issue[i] == 0){
    fin$issue[i] = 0
  } else{
    sp = strsplit(fin$Player[i], "")[[1]]
    sh = paste(sp[1:(length(sp)-4)], collapse = "")
    if (sh %in% cdf$names & sp[length(sp)] != ")"){
      fin$Player[i] = sh
      fin$issue[i] = 0
    } else{
      sh = paste(sp[1:(length(sp)-3)], collapse = "")
      if (sh %in% cdf$names){
        fin$Player[i] = sh
        fin$issue[i] = 0
      } else{
        fin$issue[i] = 1
      }
    }
  }
}

fin = fin %>% mutate(totPlayoffShare = (playoffSc + playoffShare)/2) %>% arrange(desc(totPlayoffShare)) %>% select(Player, Tm, Contrib, Yr, totPlayoffShare, W, everything())
paste("Number of issues =", sum(fin$issue))
write.csv(fin, 'playoffsfinaldf.csv')



