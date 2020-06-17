user_player = "Abdul Jeelani"
htmlinput = get_htmlnba(user_player)

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
                    FG = FG/G,
                    FGA = FGA/G,
                    FT = FT/G,
                    FTA = FTA/G, 
                    PTS = PTS/G,
                    PF = PF/G,
                    AST = AST/G,
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
  dat_ = dat_[,-c(ncol(dat_)-2)]
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
