library(rvest)
library(tidyverse)
library(formattable)
library(shiny)
library(shinydashboard)
library(DescTools)
options(stringsAsFactors = F)

cdf = read.csv("cdf.csv")[,-1]   # comes from NBAplayerLinks.R -- attaches a list of player names to corresponding basketball-reference links
options = read.csv("options.csv")[,-1]  # comes from options.R -- simply a list of available statistics for the user to choose from
statindex = read.csv("options2.csv")[,-1]

tmhex = read.csv('newdata/teamabbreviations.csv')[,-1] # team hex colors
finaldf = read.csv("finaldf.csv")[,-1]
cdf$names = as.factor(cdf$names)
opch = read.csv('lboptions.csv')[,-1]

gatherseasons <- function(svector){
  if (any(is.na(svector))){
    log = "NA"
  } else{
    svector = data.frame(svector)
    season = c()
    for (i in 1:nrow(svector)){
      sp = strsplit(svector$svector[i], "")[[1]]
      if (all(sp[6:7] == c("0", "0"))){
        ft = as.character(as.numeric(paste(sp[1:2],collapse="")) + 1)
      } else{
        ft = as.character(as.numeric(paste(sp[1:2],collapse="")))
      }
      lt = paste(sp[6:7],collapse="")
      season = c(season, paste0(ft,lt,collapse=""))
    }
    season = sort(as.numeric(season))
    ind = c(0)
    if (length(season) > 2){
      for (i in 2:(length(season)-1)){
        if (season[i+1]-season[i]==1 & season[i]-season[i-1]==1){
          ind = c(ind,i)
        } else{
          ind = c(ind,0)
        }
      }
    } else{
      ind = ind
    }
    if (length(season) > 1){
      ind = c(ind,0)
    }
    
    df = data.frame(season, ind)
    log = c()
    for (i in 1:nrow(df)){
      if(df$ind[i] == 0){
        log = c(log,df$season[i])
      } else{
        if (df$ind[i-1] == 0){
          log = c(log,"-")
        } else{
          log = log
        }
      }
    }
    if (length(log) > 1){
      for (i in 1:(length(log)-1)){
        if (log[i+1] == "-" | log[i] == "-"){
          log[i] = log[i]
        } else{
          log[i] = paste0(log[i],"; ",collapse="")
        }
      }
      log = paste0(log, collapse = "")
    }
  }
  
  return(log)
}
get_htmlnba = function(playerName){
  user_player = playerName
  user_df = cdf %>% filter(names == user_player) %>% select(actual_link)
  user_link = user_df[1,1]
  html = read_html(user_link)
  return(html)
} # scrapes the corresponding html given an NBA player name
scrape_nbadf = function(htmlinput, user_player){ 
  html = htmlinput
  whole_body = html %>% html_nodes(xpath = '//comment()') %>% html_text() %>% paste(collapse = "") %>% read_html() # basketball-reference stores nodes as comments, so in order to scrape data further down the page, must pull comments first -- https://stackoverflow.com/questions/40616357/how-to-scrape-tables-inside-a-comment-tag-in-html-with-r
  
  # Totals
  table = whole_body %>% html_node("#totals") %>% html_table()
  data = table
  for (i in 1:nrow(data)){if(data$Season[i] == "Career"){data_ = data[1:(i-1),]}else{}} #takes out career statistics
  dat_ = NULL
  for (i in 1:nrow(data_)){if(is.na(as.numeric(data_$PTS[i]))){dat_ = dat_}else{dat_ = rbind.data.frame(dat_, data_[i,])}} #standardizes data
  for (i in 1:nrow(dat_)){for(j in 6:ncol(dat_)){if(dat_[i,j] == "" | is.na(dat_[i,j])){dat_[i,j] = 0}else{dat_[i,j] = dat_[i,j]}}} #std. data
  if (any(names(dat_) == '')){dat_ = dat_[,-(which(names(dat_) == ''))]} else{dat_ = dat_}
  if (any(names(dat_) == 'Trp Dbl')){dat_ = dat_[,-(which(names(dat_) == 'Trp Dbl'))]} else{dat_ = dat_}
  
  dat_ = dat_ %>% mutate(G = as.double(G),PTS = as.double(PTS),PF = as.double(PF),AST = as.double(AST),`FT%` = as.double(`FT%`),
                         FTA = as.double(FTA),FT = as.double(FT),`FG%` = as.double(`FG%`),FGA = as.double(FGA),FG = as.double(FG))
  dat_ = dat_ %>% mutate(TOV = if(is.null(dat_$TOV)){0} else{as.double(TOV)},BLK = if(is.null(dat_$BLK)){0} else{as.double(BLK)},
                         DRB = if(is.null(dat_$DRB)){0} else{as.double(DRB)},ORB = if(is.null(dat_$ORB)){0} else{as.double(ORB)},
                         `eFG%` = if(is.null(dat_$`eFG%`)){0} else{as.double(`eFG%`)},`2P%` = if(is.null(dat_$`2P%`)){0} else{as.double(`2P%`)},
                         `2PA` = if(is.null(dat_$`2PA`)){0} else{as.double(`2PA`)},`2P` = if(is.null(dat_$`2PA`)){0} else{as.double(`2P`)},
                         `3P%` = if(is.null(dat_$`3P%`)){0} else{as.double(`3P%`)},`3PA` = if(is.null(dat_$`3PA`)){0} else{as.double(`3PA`)},
                         `3P` = if(is.null(dat_$`3PA`)){0} else{as.double(`3P`)},STL = if(is.null(dat_$STL)){0} else{as.double(STL)},
                         MP = if(is.null(dat_$MP)){0} else{as.double(MP)},
                         GS = if(is.null(dat_$GS)){0} else{as.double(GS)},
                         TRB = if(is.null(dat_$TRB)){0} else{as.double(TRB)})
  dat = dat_
  dat.names = select(dat, Season, Age, Tm, Lg, Pos);dat.numeric = select(dat, -Season, -Age, -Tm, -Lg, -Pos);dat.numeric = dat.numeric %>% select_if(colSums(.) != 0)
  dat = cbind.data.frame(dat.names, dat.numeric)
  dat = dat %>% distinct(Season, .keep_all = T)
  totals_dat = dat
  
  if (is.null(totals_dat$GS)){
    totals_dat = totals_dat %>% select(-Age, -Tm, -Lg, -Pos, -G)
  } else{
    totals_dat = totals_dat %>% select(-Age, -Tm, -Lg, -Pos, -G, -GS)
  }
  
  # PER GAME
  dat_ = dat %>% transmute(Season, Age, Tm, Lg, Pos, G, 
                           FG = ifelse("FG" %in% names(dat),FG/G,0),
                           FGA = ifelse("FGA" %in% names(dat),FGA/G,0),
                           FT = ifelse("FT" %in% names(dat),FT/G,0),
                           FTA = ifelse("FTA" %in% names(dat),FTA/G,0),
                           PTS = ifelse("PTS" %in% names(dat),PTS/G,0),
                           PF = ifelse("PF" %in% names(dat), PF/G,0),
                           AST = ifelse("AST" %in% names(dat), AST/G,0),
                           MP = if(is.null(dat$MP)){0} else{MP/G},
                           TRB = if(is.null(dat$TRB)){0} else{TRB/G},
                           TOV = if(is.null(dat$TOV)){0} else{TOV/G},
                           BLK = if(is.null(dat$BLK)){0} else{BLK/G},
                           DRB = if(is.null(dat$DRB)){0} else{DRB/G},
                           ORB = if(is.null(dat$ORB)){0} else{ORB/G},
                           `2PA` = if(is.null(dat$`2PA`)){0} else{`2PA`/G},
                           `2P` = if(any(names(dat) == "2P")){`2P`/G} else{0},
                           `3PA` = if(is.null(dat$`3PA`)){0} else{`3PA`/G},
                           `3P` = if(any(names(dat) == "3P")){`3P`/G} else{0},
                           STL = if(is.null(dat$STL)){0} else{STL/G},
                           GS = if(is.null(dat$GS)){0} else{as.double(GS)})
  dat = dat_ %>% transmute(Season, Age, Tm, Lg, Pos, G, GS, `MP/G` = MP,`FG/G` = FG,`FGA/G` = FGA,`3P/G` = `3P`,`3PA/G` = `3PA`,`2P/G` = `2P`,`2PA/G` = `2PA`,`FT/G` = `FT`,`FTA/G` = `FTA`,`ORB/G` = `ORB`,`DRB/G` = `DRB`,`TRB/G` = `TRB`,`AST/G` = `AST`,`STL/G` = `STL`,`BLK/G` = `BLK`,`TOV/G` = `TOV`,`PF/G` = `PF`,`PTS/G` = `PTS`)
  dat.names = select(dat, Season, Age, Tm, Lg, Pos);dat.numeric = select(dat, -Season, -Age, -Tm, -Lg, -Pos);dat.numeric = dat.numeric %>% select_if(colSums(.) != 0)
  dat = cbind.data.frame(dat.names, dat.numeric)
  dat = dat %>% distinct(Season, .keep_all = T)
  pergame_dat = dat
  
  # Per 100 Poss
  if (is.na(whole_body %>% html_node("#per_poss"))){
    table = table
  } else{
    table = whole_body %>% html_node("#per_poss") %>% html_table()
  }
  data = table
  for (i in 1:nrow(data)){if(data$Season[i] == "Career"){data_ = data[1:(i-1),]}else{}} #takes out career statistics
  dat_ = NULL
  for (i in 1:nrow(data_)){if(is.na(as.numeric(data_$PTS[i]))){dat_ = dat_}else{dat_ = rbind.data.frame(dat_, data_[i,])}} #standardizes data
  for (i in 1:nrow(dat_)){for(j in 6:ncol(dat_)){if(dat_[i,j] == "" | is.na(dat_[i,j])){dat_[i,j] = 0}else{dat_[i,j] = dat_[i,j]}}} #std. data
  if (any(names(dat_) == '')){dat_ = dat_[,-(which(names(dat_) == ''))]} else{dat_ = dat_}
  if (any(names(dat_) == 'Trp Dbl')){dat_ = dat_[,-(which(names(dat_) == 'Trp Dbl'))]} else{dat_ = dat_}
  dat_ = dat_ %>% mutate(G = as.double(G),PTS = as.double(PTS),PF = as.double(PF),`FT%` = as.double(`FT%`),
                         FTA = as.double(FTA),FT = as.double(FT),`FG%` = as.double(`FG%`),FGA = as.double(FGA),FG = as.double(FG))
  dat_ = dat_ %>% mutate(TOV = if(is.null(dat_$TOV)){0} else{as.double(TOV)},BLK = if(is.null(dat_$BLK)){0} else{as.double(BLK)},
                         DRB = if(is.null(dat_$DRB)){0} else{as.double(DRB)},ORB = if(is.null(dat_$ORB)){0} else{as.double(ORB)},
                         `ORtg` = if(is.null(dat_$`ORtg`)){0} else{as.double(`ORtg`)},`DRtg` = if(is.null(dat_$`DRtg`)){0} else{as.double(`DRtg`)},
                         `2P%` = if(is.null(dat_$`2P%`)){0} else{as.double(`2P%`)}, AST = if(is.null(dat_$AST)){0} else{as.double(AST)},
                         `2PA` = if(is.null(dat_$`2PA`)){0} else{as.double(`2PA`)},`2P` = if(is.null(dat_$`2P`)){0} else{as.double(`2P`)},
                         `3P%` = if(is.null(dat_$`3P%`)){0} else{as.double(`3P%`)},`3PA` = if(is.null(dat_$`3PA`)){0} else{as.double(`3PA`)},
                         `3P` = if(is.null(dat_$`3P`)){0} else{as.double(`3P`)},STL = if(is.null(dat_$STL)){0} else{as.double(STL)},
                         MP = if(is.null(dat_$MP)){0} else{as.double(MP)},
                         GS = if(is.null(dat_$GS)){0} else{as.double(GS)},
                         TRB = if(is.null(dat_$TRB)){0} else{as.double(TRB)})
  dat = dat_ %>% transmute(Season, Age, Tm, Lg, Pos, G, GS, `FG/100P` = FG,`FGA/100P` = FGA,`3P/100P` = `3P`,`3PA/100P` = `3PA`,`2P/100P` = `2P`,`2PA/100P` = `2PA`,`FT/100P` = `FT`,`FTA/100P` = `FTA`,`ORB/100P` = `ORB`,`DRB/100P` = `DRB`,`TRB/100P` = `TRB`,`AST/100P` = `AST`,`STL/100P` = `STL`,`BLK/100P` = `BLK`,`TOV/100P` = `TOV`,`PF/100P` = `PF`,`PTS/100P` = `PTS`)
  dat.names = select(dat, Season, Age, Tm, Lg, Pos);dat.numeric = select(dat, -Season, -Age, -Tm, -Lg, -Pos);dat.numeric = dat.numeric %>% select_if(colSums(.) != 0)
  dat = cbind.data.frame(dat.names, dat.numeric)
  dat = dat %>% distinct(Season, .keep_all = T)
  per100poss_dat = dat 
  if(is.null(per100poss_dat$GS)){
    per100poss_dat = per100poss_dat %>% select(-Age, -Tm, -Lg, -Pos, -G)
  } else{
    per100poss_dat = per100poss_dat %>% select(-Age, -Tm, -Lg, -Pos, -G, -GS)
  }
  
  if (all(per100poss_dat[,ncol(per100poss_dat)] == totals_dat[,ncol(totals_dat)])){
    per100poss_dat = NULL
  }
  
  # Advanced
  table = whole_body %>% html_node("#advanced") %>% html_table()
  data = table
  for (i in 1:nrow(data)){if(data$Season[i] == "Career"){data_ = data[1:(i-1),]}else{}} #takes out career statistics
  dat_ = NULL
  for (i in 1:nrow(data_)){if(is.na(as.numeric(data_$PER[i]))){dat_ = dat_}else{dat_ = rbind.data.frame(dat_, data_[i,])}} #standardizes data
  for (i in 1:nrow(dat_)){for(j in 6:ncol(dat_)){if(dat_[i,j] == "" | is.na(dat_[i,j])){dat_[i,j] = 0}else{dat_[i,j] = dat_[i,j]}}} #std. data
  ind = c()
  for (j in 6:ncol(dat_)){
    if (names(dat_)[j] != "WS" & names(dat_)[j] != "OWS" & names(dat_)[j] != "DWS"){
      if (sum(as.double(dat_[,j])) == 0){
        ind = c(ind, j)
      } else{
        ind = ind
      }
    } else{
      ind = ind
    }
  } 
  
  dat_ = dat_[, -ind]
  dat_ = dat_ %>% mutate(G = as.double(G),`TS%` = as.double(`TS%`),FTr = as.double(FTr),OWS = as.double(OWS),DWS = as.double(DWS),WS = as.double(WS))
  dat = dat_ %>% mutate(`ORB%` = if(is.null(dat_$`ORB%`)){0} else{as.double(`ORB%`)},`DRB%` = if(is.null(dat_$`DRB%`)){0} else{as.double(`DRB%`)},
                        `TRB%` = if(is.null(dat_$`TRB%`)){0} else{as.double(`TRB%`)},`AST%` = if(is.null(dat_$`AST%`)){0} else{as.double(`AST%`)},
                        `STL%` = if(is.null(dat_$`STL%`)){0} else{as.double(`STL%`)},`BLK%` = if(is.null(dat_$`BLK%`)){0} else{as.double(`BLK%`)},
                        `TOV%` = if(is.null(dat_$`TOV%`)){0} else{as.double(`TOV%`)},`USG%` = if(is.null(dat_$`USG%`)){0} else{as.double(`USG%`)},
                        OBPM = if(is.null(dat_$OBPM)){0} else{as.double(OBPM)},DBPM = if(is.null(dat_$DBPM)){0} else{as.double(DBPM)},
                        BPM = if(is.null(dat_$BPM)){0} else{as.double(BPM)},VORP = if(is.null(dat_$VORP)){0} else{as.double(VORP)},
                        MP = as.double(MP),PER = as.double(PER),`WS/48` = as.double(`WS/48`))
  dat.names = select(dat, Season, Age, Tm, Lg, Pos);dat.numeric = select(dat, -Season, -Age, -Tm, -Lg, -Pos);dat.numeric = dat.numeric #%>% select_if(colSums(.) != 0)
  dat = cbind.data.frame(dat.names, dat.numeric)
  dat = dat %>% distinct(Season, .keep_all = T)
  advanced_dat = dat %>% select(-Age, -Tm, -Lg, -Pos, -G, -MP)
  
  
  # Merging Dataframes
  pgt = full_join(pergame_dat, totals_dat, by = c("Season"))
  pgta = full_join(pgt, advanced_dat, by = "Season")
  
  if (is.null(per100poss_dat)){
    finaldf = pgta
  } else{
    finaldf = full_join(pgta, per100poss_dat, by = c("Season"))
  }
  
  finaldf$Yr = 1:nrow(finaldf)
  finaldf$Player = user_player
  finaldf = finaldf %>% select(Player, Yr, everything())
  for (i in 1:nrow(finaldf)){for (j in 1:ncol(finaldf)){if (is.na(finaldf[i,j])){finaldf[i,j] = 0}}}
  return(finaldf)
} # given a player name and corresponding html, returns career dataframe including all statistics
scrape_accolades = function(html_input, user_player){
  li.a = html_input %>% html_nodes("li") %>% html_node("a") %>% html_text()
  firs = match("Full Site Menu Below", li.a) + 1
  last = match(paste(user_player, "Overview"), li.a) - 1
  if (is.na(last)){
    last = 50
  } else{
    last = last
  }
  acc = li.a[firs:last]
  
  hof = c();as = c();roy = c();mvp = c();fmvp = c();scmp = c();chmp = c();allnba = c();alld = c();allr = c();x6m = c()
  for (i in 1:length(acc)){
    s = strsplit(acc[i], "")[[1]]
    if (paste(s, collapse = "") == "Hall of Fame"){hof = c(hof,1)}else{hof = c(hof,0)}
    if (length(s) < 7){
      as = as
    } else{
      if (paste(s[(length(s)-7):length(s)], collapse = "") == "All Star"){
        as = c(paste(s[grepl("[0-9]",s)], collapse = ""),as) %>% as.numeric()
      } else{
        as = as
      }
    }
    
    if (length(s) < 3){
      roy = roy
    } else{
      if (paste(s[(length(s)-2):length(s)], collapse = "") == "ROY"){
        roy = c(roy,1)
      } else{
        roy = roy
      }
    }
    
    if (length(s) < 5){
      mvp = mvp
    } else{
      if (paste(s[(length(s)-2):length(s)], collapse = "") == "MVP" & paste(s[(length(s)-4):length(s)], collapse = "") != "s MVP" & paste(s[(length(s)-4):length(s)], collapse = "") != "S MVP"){
        mvp = c(paste(s[grepl("[0-9]",s)], collapse = ""),mvp) %>% as.numeric()
      } else{
        mvp = mvp
      }
    }
    
    if (length(s) < 10){
      fmvp = fmvp
    } else{
      if (paste(s[(length(s)-9):length(s)], collapse = "") == "Finals MVP"){
        fmvp = c(paste(s[grepl("[0-9]",s)], collapse = ""),fmvp) %>% as.numeric()
      } else{
        fmvp = fmvp
      }
    }
    
    if (length(s) < 13){
      scmp = scmp
    } else{
      if (paste(s[(length(s)-12):length(s)], collapse = "") == "Scoring Champ"){
        scmp = c(paste(s[grepl("[0-9]",s)], collapse = ""),scmp) %>% as.numeric()
      } else{
        scmp = scmp
      }
    }
    
    if (length(s) < 9){
      chmp = chmp
    } else{
      if (paste(s[(length(s)-8):length(s)], collapse = "") == "NBA Champ"){
        chmp = c(paste(s[grepl("[0-9]",s)], collapse = ""),chmp) %>% as.numeric()
      } else{
        chmp = chmp
      }
    }
    
    if (length(s) < 7){
      allnba = allnba
    } else{
      if (paste(s[(length(s)-6):length(s)], collapse = "") == "All-NBA"){
        allnba = c(paste(s[grepl("[0-9]",s)], collapse = ""),allnba) %>% as.numeric()
      } else{
        allnba = allnba
      }
    }
    
    if (length(s) < 13){
      alld = alld
    } else{
      if (paste(s[(length(s)-12):length(s)], collapse = "") == "All-Defensive"){
        alld = c(paste(s[grepl("[0-9]",s)], collapse = ""),alld) %>% as.numeric()
      } else{
        alld = alld
      }
    }
    
    if (length(s) < 10){
      allr = allr
    } else{
      if (paste(s[(length(s)-9):length(s)], collapse = "") == "All-Rookie"){
        allr = c(allr,1)
      } else{
        allr = allr
      }
    }
    
    if (length(s) < 9){
      x6m = x6m
    } else{
      if (paste(s[(length(s)-8):length(s)], collapse = "") == "Sixth Man"){
        x6m = c(paste(s[grepl("[0-9]",s)], collapse = ""),x6m) %>% as.numeric()
      } else{
        x6m = x6m
      }
    }
  }
  
  accdf = data.frame(player = user_player,
                     allstar = ifelse(is.null(as),0,ifelse(as > 40,1,as)),
                     allnba = ifelse(is.null(allnba),0,ifelse(allnba > 40,1,allnba)),
                     alld = ifelse(is.null(alld),0,ifelse(alld > 40,1,alld)),
                     x6m = ifelse(is.null(x6m),0,ifelse(x6m > 40,1,x6m)),
                     scmp = ifelse(is.null(scmp),0,ifelse(scmp > 40,1,scmp)),
                     mvp = ifelse(is.null(mvp),0,ifelse(mvp > 40,1,mvp)),
                     fmvp = ifelse(is.null(fmvp),0,ifelse(fmvp > 40,1,fmvp)),
                     chmp = ifelse(is.null(chmp),0,ifelse(chmp > 40,1,chmp)),
                     roy = ifelse(sum(roy) == 1,"Yes","No"),
                     allr = ifelse(sum(allr) == 1,"Yes","No"),
                     hof = ifelse(sum(hof) == 1,"Yes","No"))
  return(accdf)
} # scrapes the player's awards an honors (found at the top of the basketball-reference page)
primedf = function(player1d, player2d, i = 1){
  if (all(names(player1d) != "AST/G")){p1p1 = NULL} else{p1p1 = player1d[1:i,] %>% summarise(Player = Player[1], G = sum(G),`PTS/G` = round(sum(PTS)/G,2),`TRB/G` = round(sum(TRB)/G,2),`AST/G` = round(sum(AST)/G,2),`TS%` = round(sum((`TS%`*FGA))/sum(FGA),4),`FT%` = round(sum((`FT%`*FTA))/sum(FTA),4),PER = round(sum(PER*G)/(G*i),2),WS = round(sum(WS*G)/(G*i),2))}
  if (all(names(player2d) != "AST/G")){p2p1 = NULL} else{p2p1 = player2d[1:i,] %>% summarise(Player = Player[1], G = sum(G),`PTS/G` = round(sum(PTS)/G,2),`TRB/G` = round(sum(TRB)/G,2),`AST/G` = round(sum(AST)/G,2),`TS%` = round(sum((`TS%`*FGA))/sum(FGA),4),`FT%` = round(sum((`FT%`*FTA))/sum(FTA),4),PER = round(sum(PER*G)/(G*i),2),WS = round(sum(WS*G)/(G*i),2))}
  if (is.null(p1p1) & is.null(p2p1)){table = NULL} else{table = rbind.data.frame(p1p1, p2p1)}
  return(table)
} # takes in both players' data and a number (x) and returns the best x years data averages ("best" calculated by a linear combination of simplified GmSc, PER, WS)
sm = sample(1:(levels(as.factor(finaldf$Player)) %>% length()),2);p1 = finaldf$Player[sm[1]];p2 = finaldf$Player[sm[2]]

ui <- fluidPage(
  headerPanel("NBA Player Comparison"),
  sidebarPanel(
    selectInput('player1', "Player 1", c(levels(as.factor(finaldf$Player))),
                selected = p1),
    selectInput('player2', "Player 2", c(levels(as.factor(finaldf$Player))),
                selected = p2),
    titlePanel("Career Accolades"),
    tableOutput("accolades"),
    titlePanel("Statistics Glossary"),
    tableOutput('statindex'),
    #br(),br(),br(),
    titlePanel("Leaderboard Search"),
    div(style="display: inline-block;vertical-align:top, width = 75px",selectInput('lbchoice', "Statistic of Interest:", opch$lboptions,selected = "PTS")),
    div(style="display: inline-block;vertical-align:top; width = 25px",textInput('lbyear', "Year:")),
    formattableOutput('leaderboard'),
    "To qualify for individual statistic leaderboard, players must average 40th percentile volume statistic scores."

  ),
  mainPanel(
    titlePanel(h1("Career Production Comparison")),
    plotOutput("comparison"),
    titlePanel(h1("Prime Comparison")),
    selectInput('pkyrs', "Years of Interest:", paste0(1:22, "-year peak")),
    formattableOutput("prime"),
    titlePanel(h1("Statistical Progression")),
    selectInput('statint', "Statistic of Interest:", options,
                selected = "WS"),
    plotOutput("prog"),
    titlePanel(h1("Seasons Comparison")),
    plotOutput('seasonscomp'),
    br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
    titlePanel(h1("Individual Season Search")),
    div(style="display: inline-block;vertical-align:top",selectInput('indplayer', "Player:", c(levels(as.factor(finaldf$Player))), selected = p1)),
    div(style="display: inline-block;vertical-align:top; width: 150px",textInput('indyear', "Year:")),
    plotOutput('indseason')
  )
)

server <- function(input, output, session) {
  player1SC <- reactive({scrape_nbadf(get_htmlnba(input$player1), input$player1)})
  player1AC <- reactive({scrape_accolades(get_htmlnba(input$player1), input$player1)})
  player2SC <- reactive({scrape_nbadf(get_htmlnba(input$player2), input$player2)})
  player2AC <- reactive({scrape_accolades(get_htmlnba(input$player2), input$player2)})
  primey <- reactive({input$pkyrs})
  stat <- reactive({input$statint})
  selPlayer <- reactive({input$indplayer})
  selYear <- reactive({input$indyear})
  lbChoice <- reactive({input$lbchoice})
  lbYear <- reactive({input$lbyear})
  
  output$accolades <- renderTable({
    acc1 = player1AC()
    acc2 = player2AC()
    if (input$player1 != input$player2){accdf = rbind.data.frame(acc1, acc2)} else{accdf = acc1}
    names(accdf) = c("Player", "All Star Selections", "All-NBA Selections","All-Defensive Team Selections", "6th Man of the Year Awards", "Scoring Titles","Most Valuable Player Awards", "Finals Most Valuable Player Awards",  "Championships", "Rookie Of the Year", "All-Rookie Team", "Hall of Fame Induction")
    accdf = t(accdf)
    colnames(accdf) = accdf[1,]
    accdf = as.data.frame(accdf[-1,])
    if (names(accdf)[1] == "accdf[-1, ]"){
      colnames(accdf)[1] = input$player1
    }
    greater_bold <- formatter("span", style = x ~ style("font-weight" = ifelse(x > mean(x), "bold", NA)))
    formattable(accdf)
  },rownames = T)
  output$comparison <- renderPlot({
    player1d = player1SC()
    player2d = player2SC()
    g = player1car = player1sum = player2car = player2sum = NULL
    commonvars = intersect(names(player1d), names(player2d))
    bind1 = player1d[,commonvars]
    bind2 = player2d[,commonvars]
    compdf = rbind.data.frame(bind1, bind2)
    steals = c();blocks = c();x3pt = c();x3pta = c()
    for (i in 1:length(colnames(compdf))){
      if (colnames(compdf)[i] == "STL"){steals = c(steals,1)} else{steals = steals}
      if (colnames(compdf)[i] == "BLK"){blocks = c(blocks,1)} else{blocks = blocks}
      if (colnames(compdf)[i] == "3P"){x3pt = c(x3pt,1)} else{x3pt = x3pt}
      if (colnames(compdf)[i] == "3PA"){x3pta = c(x3pta,1)} else{x3pta = x3pta}}
    
    player1car = player1d %>% select(MP, FG, FGA, FT,  FTA, TRB, AST, PTS)
    if (!is.null(steals)){player1car = cbind.data.frame(player1car, STL = player1d[,"STL"])} else{player1car = player1car}
    if (!is.null(blocks)){player1car = cbind.data.frame(player1car, BLK = player1d[,"BLK"])} else{player1car = player1car}
    if (!is.null(x3pt)){player1car = cbind.data.frame(player1car, `3P` = player1d[,"3P"])} else{player1car = player1car}
    if (!is.null(x3pta)){player1car = cbind.data.frame(player1car, `3PA` = player1d[,"3PA"])} else{player1car = player1car}
    player1sum = player1car %>% summarise(Player = input$player1,MP = sum(MP),FG = sum(FG),FGA = sum(FGA),FT = sum(FT),FTA = sum(FTA),TRB = sum(TRB),AST = sum(AST),PTS = sum(PTS))
    if (!is.null(steals)){player1sum = cbind.data.frame(player1sum, STL = sum(player1d[,"STL"]))} else{player1sum = player1sum}
    if (!is.null(blocks)){player1sum = cbind.data.frame(player1sum, BLK = sum(player1d[,"BLK"]))} else{player1sum = player1sum}
    if (!is.null(x3pt)){player1sum = cbind.data.frame(player1sum, `3P` = sum(player1d[,"3P"]))} else{player1sum = player1sum}
    if (!is.null(x3pta)){player1sum = cbind.data.frame(player1sum, `3PA` = sum(player1d[,"3PA"]))} else{player1sum = player1sum}
    
    player2car = player2d %>% select(MP, FG, FGA, FT,  FTA, TRB, AST, PTS)
    if (!is.null(steals)){player2car = cbind.data.frame(player2car, STL = player2d[,"STL"])} else{player2car = player2car}
    if (!is.null(blocks)){player2car = cbind.data.frame(player2car, BLK = player2d[,"BLK"])} else{player2car = player2car}
    if (!is.null(x3pt)){player2car = cbind.data.frame(player2car, `3P` = player2d[,"3P"])} else{player2car = player2car}
    if (!is.null(x3pta)){player2car = cbind.data.frame(player2car, `3PA` = player2d[,"3PA"])} else{player2car = player2car}
    player2sum = player2car %>% summarise(Player = input$player2,MP = sum(MP),FG = sum(FG),FGA = sum(FGA),FT = sum(FT),FTA = sum(FTA),TRB = sum(TRB),AST = sum(AST),PTS = sum(PTS))
    if (!is.null(steals)){player2sum = cbind.data.frame(player2sum, STL = sum(player2d[,"STL"]))} else{player2sum = player2sum}
    if (!is.null(blocks)){player2sum = cbind.data.frame(player2sum, BLK = sum(player2d[,"BLK"]))} else{player2sum = player2sum}
    if (!is.null(x3pt)){player2sum = cbind.data.frame(player2sum, `3P` = sum(player2d[,"3P"]))} else{player2sum = player2sum}
    if (!is.null(x3pta)){player2sum = cbind.data.frame(player2sum, `3PA` = sum(player2d[,"3PA"]))} else{player2sum = player2sum}
    
    p1ord = player1d %>% mutate(GmSc = 100*PER + 100*WS + 2*PTS + (0.4*FG) - (0.7*FGA) - (0.4*(FTA - FT)) + TRB + (0.7*AST) - (0.4*PF)) %>% arrange(desc(GmSc))
    team = p1ord %>% select(Tm) %>% head(5) %>% group_by(Tm) %>% summarize(n = n(),.groups = 'drop') %>% arrange(desc(n))
    playerTeam = ifelse(team$n[1] > 0.50*(min(nrow(player1d), 5)), team$Tm[1], "None")
    if (playerTeam %in% tmhex$abb){color = tmhex$hex[grep(playerTeam, tmhex$abb)]} else{color = "black"}
    
    careertot = rbind.data.frame(player1sum, player2sum)
    g = careertot %>% select(-MP) %>% gather("Stat", "Value", -Player)
    if (input$player1 != input$player2 & (is.null(g)==F)){
      g$Player = factor(g$Player, levels = c(input$player1, input$player2))
    } else{
      g$Player = g$Player
    }
    
    if (is.null(g)){
      ggplot() + ggtitle("One or both of the players selected have not registered enough statistics. Please try another!") + theme_minimal()    
    } else{
      g %>% ggplot(aes(x = Stat, y = Value)) + geom_bar(stat = "identity", position = "dodge", width = I(1/2), aes(fill = Player)) + theme_classic() + scale_fill_manual("",values = c(color, "grey70")) +
        scale_y_continuous("") + scale_x_discrete("") +
        theme(legend.position = c(0.15,0.90))
    }
    
  })
  output$prime <- renderFormattable({
    num = primey()
    s = strsplit(num, "")[[1]]
    num = as.numeric(paste(s[grepl("[0-9]", s)], collapse = ""))
    player1s = finaldf %>% filter(Player == input$player1) %>% mutate(sc = 0.1460142*Eff + 1.2299956*Vol) %>% arrange(desc(sc)) %>% select(Player, Yr, sc)
    player1d = player1SC()
    for (i in 1:nrow(player1d)){
      player1d$Yr[i] = as.numeric(paste0(strsplit(player1d$Season[i], "")[[1]][1:4], collapse = ""))+1
    }
    player1d = left_join(player1d, player1s, by = c("Player","Yr")) %>% drop_na() %>% arrange(desc(sc))
    
    player2s = finaldf %>% filter(Player == input$player2) %>% mutate(sc = 0.1460142*Eff + 1.2299956*Vol) %>% arrange(desc(sc)) %>% select(Player, Yr, sc)
    player2d = player2SC()
    for (i in 1:nrow(player2d)){
      player2d$Yr[i] = as.numeric(paste0(strsplit(player2d$Season[i], "")[[1]][1:4], collapse = ""))+1
    }
    player2d = left_join(player2d, player2s, by = c("Player","Yr")) %>% drop_na() %>% arrange(desc(sc))
    p1null = primedf(player1d,player1d,1);p2null = primedf(player2d,player2d,1)
    
    if (is.null(p1null) & is.null(p2null)){
      d = data.frame(c("Both players entered are missing statistics crucial to evaluating prime years. Please choose different player(s)."))
      names(d) = " "
      formattable(d)
    } else{
      player1seasons = player1d$Season[1:num]
      player2seasons = player2d$Season[1:num]
      `Season(s)` = c(gatherseasons(player1seasons),gatherseasons(player2seasons))
      
      table = NULL
      table = primedf(player1d, player2d, i = num)
      table = cbind.data.frame(table,`Season(s)`)
      
      if (is.null(p1null)){table = table[2,]} else if(is.null(p2null)){table = table[1,]};rownames(table) = NULL
      
      greater_bold <- formatter("span", style = x ~ style("font-weight" = ifelse(x > mean(x), "bold", NA)))
      if (is.na(as.numeric(table[2,4]))){table = table[-2,]}
      if (is.na(as.numeric(table[1,4]))){
        table = table[-1,]
        rownames(table) = NULL
      }
      if (input$player1 == input$player2 & dim(table)[1] != 0){table = table[1,]} else{table=table}
      
      if (dim(table)[1] == 0){
        d = data.frame(c("No player(s) entered played that many years. Try choosing a shorter peak!"))
        names(d) = " "
        formattable(d)
      } else{
        formattable(table, list(`G` = greater_bold, `PTS/G` = greater_bold, `TRB/G` = greater_bold, `AST/G` = greater_bold, `TS%` = greater_bold, `FT%` = greater_bold, PER = greater_bold, WS = greater_bold))
      }
    }
  })
  output$prog <- renderPlot({
    sel = stat()
    player1d = player1SC()
    player2d = player2SC()
    commonvars = intersect(names(player1d), names(player2d))
    bind1 = player1d[,commonvars]
    bind2 = player2d[,commonvars]
    compdf = rbind.data.frame(bind1, bind2)
    if (input$player1 != input$player2){compdf$Player = factor(compdf$Player, levels = c(input$player1, input$player2))} else{compdf$Player = compdf$Player}
    for (i in 1:nrow(compdf)){
      sp = strsplit(compdf$Season[i],"")[[1]]
      l2 = paste(sp[6:7], collapse = "")
      compdf$Season[i] = paste0("'",l2)
    }
    
    ind = which(names(compdf) == sel)
    
    p1ord = player1d %>% mutate(GmSc = 100*ifelse("PER" %in% names(player1d),PER,0) + 
                                  100*ifelse("WS" %in% names(player1d),WS,0) + 
                                  2*ifelse("PTS" %in% names(player1d),PTS,0) + 
                                  (0.4*ifelse("FG" %in% names(player1d),FG,0)) - 
                                  (0.7*ifelse("FGA" %in% names(player1d),FGA,0)) - 
                                  ifelse("TRB" %in% names(player1d),TRB,0) + 
                                  (0.7*ifelse("AST" %in% names(player1d),AST,0)) - 
                                  (0.4*ifelse("PF" %in% names(player1d),PF,0))) %>% arrange(desc(GmSc))
    team = p1ord %>% select(Tm) %>% head(5) %>% group_by(Tm) %>% summarize(n = n(),.groups = 'drop') %>% arrange(desc(n))
    playerTeam = ifelse(team$n[1] > 0.50*(min(nrow(player1d), 5)), team$Tm[1], "None")
    if (playerTeam %in% tmhex$abb){color = tmhex$hex[grep(playerTeam, tmhex$abb)]} else{color = "black"}
    
    if (is_empty(ind)){
      ggplot() + ggtitle("The statistic you selected is not available for at least one player!", "Try choosing another.") + theme_minimal()    
    } else{
      ggplot(compdf, aes(x = compdf$Yr, y = compdf[,ind])) +
        geom_line(aes(color = compdf$Player)) + 
        geom_point(aes(color = compdf$Player), size = I(7.5)) +
        scale_x_continuous("Year of Career", breaks = seq(min(compdf$Yr),max(compdf$Yr),1)) + scale_y_continuous(paste(sel)) +
        theme_bw() + scale_color_manual("",values = c(color, "grey70")) + geom_text(aes(label = Season), color = "white")
    }
  })
  output$statindex <- renderTable({
    statindex
  })
  output$seasonscomp <- renderPlot({
    player1 = input$player1
    player2 = input$player2
    
    x1bar = finaldf %>% filter(allstar == 1) %>% summarise(x1 = mean(Eff),x2 = mean(Vol)) %>% as.matrix() %>% t()
    x2bar = finaldf %>% filter(allstar == 0) %>% summarise(x1 = mean(Eff),x2 = mean(Vol)) %>% as.matrix() %>% t()
    S1 = finaldf %>% filter(allstar == 1) %>% select(Eff, Vol) %>% cov()
    S2 = finaldf %>% filter(allstar == 0) %>% select(Eff, Vol) %>% cov()
    Sp = ((finaldf %>% filter(allstar == 1) %>% nrow() - 1)*S1 + (finaldf %>% filter(allstar == 0) %>% nrow() - 1)*S2)/(finaldf %>% filter(allstar == 0) %>% nrow() + finaldf %>% filter(allstar == 1) %>% nrow() - 2)
    w = solve(Sp)%*%(x1bar - x2bar)
    limit1 = (0.50)*t(w)%*%(x1bar+x2bar) + 6.3979
    limit2 = (0.50)*t(w)%*%(x1bar+x2bar) + 1.1425
    user_names = c(player1, player2)
    toplot = finaldf %>% filter(Player %in% user_names)
    for (i in 1:nrow(toplot)){
      sp = strsplit(as.character(toplot$i[i]),"")[[1]]
      l2 = paste(sp[3:4], collapse = "")
      toplot$Yr[i] = paste0("'",l2)
    }
    player1d = player1SC()
    p1ord = p1ord = player1d %>% mutate(GmSc = 100*ifelse("PER" %in% names(player1d),PER,0) + 
                                          100*ifelse("WS" %in% names(player1d),WS,0) + 
                                          2*ifelse("PTS" %in% names(player1d),PTS,0) + 
                                          (0.4*ifelse("FG" %in% names(player1d),FG,0)) - 
                                          (0.7*ifelse("FGA" %in% names(player1d),FGA,0)) - 
                                          ifelse("TRB" %in% names(player1d),TRB,0) + 
                                          (0.7*ifelse("AST" %in% names(player1d),AST,0)) - 
                                          (0.4*ifelse("PF" %in% names(player1d),PF,0))) %>% arrange(desc(GmSc))
    team = p1ord %>% select(Tm) %>% head(5) %>% group_by(Tm) %>% summarize(n = n(),.groups = 'drop') %>% arrange(desc(n))
    playerTeam = ifelse(team$n[1] > 0.50*(min(nrow(player1d), 5)), team$Tm[1], "None")
    if (playerTeam %in% tmhex$abb){color = tmhex$hex[grep(playerTeam, tmhex$abb)]} else{color = "black"}
    
    if (input$player1 != input$player2){toplot$Player = factor(toplot$Player, levels = c(player1, player2))} else{toplot$Player = toplot$Player}
    
    toplot %>% 
      ggplot(aes(x = Eff, y = Vol, color = Player)) + 
      geom_hline(yintercept = 0, linetype = "dashed", alpha = I(.55)) + 
      geom_vline(xintercept = 0, linetype = "dashed", alpha = I(.55)) + 
      scale_x_continuous("Efficiency Principal Component") + scale_y_continuous("Volume Principal Component") + theme_classic() + 
      scale_color_manual("",values = c(color, "grey70")) + 
      geom_abline(slope = (-w[1,1]/w[2,1]), intercept = 
                    (limit1/w[2,1]), linetype = "dashed", color = "#d29914") +
      geom_abline(slope = (-w[1,1]/w[2,1]), intercept = 
                    (limit2/w[2,1]), linetype = "dashed", color = "#d29914") +  geom_point(size = I(7.5)) +
      geom_text(aes(label = Yr), color = "white") +
      theme(legend.position = "top") # c(0.10, .90))
  }, height = 625)
  output$indseason <- renderPlot({
    player = selPlayer()
    year = selYear()
    yrdf = finaldf %>% filter(Player == player, Yr == year)
    if ((dim(yrdf)[1]) == 0){
      ggplot() + ggtitle("Select a Player and then input a Year in which that player was active (since 1952).") + theme_minimal()
    } else{
      sta = names(yrdf)[6:21]
      val = (yrdf)[6:21] %>% t()
      st = cbind.data.frame(sta, val)
      st = st %>% filter(sta != "SPBtPFR")
      for (i in 1:nrow(st)){
        if (st$val[i] == 100){
          st$col[i] = "League-Leading"
        } else if (st$val[i] >= 50){
          st$col[i] = "Above Average"
        } else{
          st$col[i] = "Below Average"
        }
      }
      st$col = factor(st$col, levels = c("Below Average", "Above Average", "League-Leading"))
      
      for (i in 1:nrow(st)){if (st$sta[i] %in% c("x3PAVG", "x2PAVG", "xFTAVG", "ASTtTOV", "SPBtPFR", "TS", "FTr")){st$type[i] = "eff"} else{st$type[i] = "vol"}}
      st$sta[1] = "3P ADD"
      st$sta[2] = "2P ADD"
      st$sta[3] = "FT ADD"
      st$sta[9] = "AST/TOV"
      st$sta[14] = "TS%"
      st = st %>% arrange(type, val)
      st$sta = factor(st$sta, levels = st$sta)
      
      if (all(st$col %in% c("Above Average", "League-Leading"))){
        colors = c(MixColor("white", "#ff9900", 0.5), "#ff9900")
      } else{
        colors = c("#9B2335",MixColor("white", "#ff9900", 0.5),"#ff9900")
      }
      
      p = st %>% ggplot(aes(x = sta, y = val)) + geom_hline(yintercept = 50, linetype = "dashed") + geom_bar(stat = "identity", width = I(1/2), alpha = I(3/4), aes(fill = as.factor(col)), color = "black") + coord_flip() + scale_y_continuous("Adjusted Percentile Rank") + scale_x_discrete("") + scale_fill_manual("",values = colors) + theme_classic() + theme(legend.position = "top", panel.background = element_rect(fill = "grey95")) + geom_vline(xintercept = 6.5) + geom_hline(yintercept = 102) +
        annotate("text", x = 11.5, y = 106, label = "Volume", angle = 270) + #MixColor("white", "#9B2335", .8)
        annotate("text", x = 3.5, y = 106, label = "Efficiency", angle = 270)
      p
      
    }
  })
  output$leaderboard <- renderFormattable({
    user_input = lbChoice()
    Year = lbYear()
    player = selPlayer()
    
    if ((Year %in% 1952:(format(Sys.Date(), "%Y")))==F){
      d = data.frame(c("Select a year and statistic to view the leaderboard."))
      names(d) = " "
      formattable(d)
      
    } else{
      choice = opch$choice[which(opch$lboptions == user_input)]
      if (user_input == "3P ADD" | user_input == "2P ADD" | user_input == "FT ADD"){d = finaldf %>% filter(Yr == Year) %>% select(Player, choice)} else{d = finaldf %>% filter(Yr == Year) %>% mutate(VolT = PTS + TRB + AST + STL + BLK + PER + WS + OWS + DWS) %>% filter(VolT > 360) %>% select(Player, choice)}
      lb = data.frame(Player = d[,1][rev(order(d[,2]))], apr = round(d[,2][rev(order(d[,2]))], 2))
      lb$rk = 1:nrow(lb)
      lb = lb %>% select(rk, everything())
      
      if (player %in% lb$Player){
        if (player %in% lb$Player[1:10]){
          lb = lb %>% head(10)
        } else{
          lb1 = lb %>% head(10)
          lb2 = lb %>% filter(Player == player)
          lb = rbind.data.frame(lb1, lb2)
        }
      } else{lb = lb %>% head(10)}
      
      loc = which(lb$Player == player)
      names(lb)[c(1,3)] = c("Rk.", " ")
      player_name_bold <- formatter("span", style = x ~ style("font-weight" = ifelse(x == player, "bold", NA)))
      if (is_empty(loc)){formattable(lb, align = c("r", "c", "r"))} else{formattable(lb, align = c("r", "c", "r"), list(Player = player_name_bold))}
    }
  })
}

shinyApp(ui, server)
