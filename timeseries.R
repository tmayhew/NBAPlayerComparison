library(rvest)
library(tidyverse)
library(formattable)
library(shiny)
library(shinydashboard)
library(DescTools)
options(stringsAsFactors = F)

cdf = read.csv("cdf.csv")[,-1]
colnames = read.csv("newdata/TScolnames.csv")[,-1]
TSoptions = read.csv("TSoptions.csv")[,-1]

get_gamelog = function(player, year1, year2){
  year1 = as.numeric(year1)
  year2 = as.numeric(year2)
  ayear1 = year1 + 1
  s = strsplit(as.character(cdf$actual_link[which(cdf$names == player)]), "")[[1]]
  links = c()
  for (i in ayear1:year2){links = c(links,paste0(paste(s[1:(length(s)-5)], collapse = ""), "/gamelog/", as.character(i), collapse = ""))}
  dl = NULL
  for (j in 1:length(links)){
    if (read_html(links[j]) %>% html_table(fill = T) %>% is_empty()){
      d = NULL
    } else{
      d_ = read_html(links[j]) %>% html_table(fill = T)
      d = d_[[8]]
      d = d[-which(d$Date == "Date"),]
      if (any(is.na(as.numeric(d$PTS)))){d = d[-which(is.na(as.numeric(d$PTS))),]} else{d = d}
      colnames(d)[which(colnames(d) == "")] = c("at", "Result")
      colnames(d)[which(colnames(d) == "FG%")] = "FG."
      colnames(d)[which(colnames(d) == "FT%")] = "FT."
      colnames(d)[which(colnames(d) == "3P")] = "X3P"
      colnames(d)[which(colnames(d) == "3PA")] = "X3PA"
      colnames(d)[which(colnames(d) == "3P%")] = "X3P."
      colnames(d)[which(colnames(d) == "+/-")] = "Plus.Minus"
      
      if (any((colnames %in% colnames(d) == F))){
        missing = colnames[which((colnames %in% colnames(d) == F))]
        for (k in 1:length(missing)){
          d = data.frame(d, n = 0)
        }
        length(missing)
        colnames(d)[(ncol(d)-length(missing) + 1):ncol(d)] = missing
        d = d %>% select(all_of(colnames))
      } else{
        d = d %>% select(all_of(colnames))
      }
      for (i in 1:nrow(d)){for (j in 9:ncol(d)){if (d[i,j] == ""){d[i,j] = 0} else{d[i,j]}}}
    }
    dl = rbind.data.frame(dl, d)
  }
  dl = data.frame(Player = player, dl) %>% mutate(G = 1:nrow(dl))
  return(dl)
}

player1 = "Draymond Green"
start1 = year1 = "2015"
end1 = year2 = "2016"

player2 = "Klay Thompson"
start2 = "2015"
end2 = "2016"

get_GLcomp = function(player1, p1s, p1e, player2, p2s, p2e){
  GL1 = get_gamelog(player1, p1s, p1e)
  GL1$Player = paste0(GL1$Player[1], " (", p1s, "-", p1e, ")", collapse = "")
  GL2 = get_gamelog(player2, p2s, p2e)
  GL2$Player = paste0(GL2$Player[1], " (", p2s, "-", p2e, ")", collapse = "")
  return(rbind.data.frame(GL1, GL2))
}
GL = get_GLcomp(player1, start1, end1, player2, start2, end2)

######################################################################################

user_sel = "3PT Percentage"
roll_num = 20
sel = TSoptions$actual[which(TSoptions$useroption == user_sel)]

if (sel %in% c("FG.", "X3P.", "FT.")){
  if (sel == "FG."){
    toplot = GL[,which(names(GL) %in% c("Player", "G", "Date", "FG", "FGA"))]
    toplot[,4] = as.numeric(toplot[,4])
    toplot[,5] = as.numeric(toplot[,5])
    toplot[,6] = 0
    
    player1df = toplot %>% filter(grepl(player1, Player))
    player2df = toplot %>% filter(grepl(player2, Player))
    
    for (i in 1:nrow(player1df)){if (i < roll_num){player1df[,6][i] = sum(player1df$FG[1:i])/sum(player1df$FGA[1:i])} else{player1df[,6][i] = sum(player1df$FG[(i-(roll_num-1)):i])/sum(player1df$FGA[(i-(roll_num-1)):i])}}
    player1df = player1df[(roll_num):nrow(player1df),]
    for (i in 1:nrow(player2df)){if (i < roll_num){player2df[,6][i] = sum(player2df$FG[1:i])/sum(player2df$FGA[1:i])} else{player2df[,6][i] = sum(player2df$FG[(i-(roll_num-1)):i])/sum(player2df$FGA[(i-(roll_num-1)):i])}}
    player2df = player2df[(roll_num):nrow(player2df),]
    toplot = rbind.data.frame(player1df, player2df)
    colnames(toplot)[6] = "V5"
  } else if (sel == "X3P."){
    toplot = GL[,which(names(GL) %in% c("Player", "G", "Date", "X3P", "X3PA"))]
    toplot[,4] = as.numeric(toplot[,4])
    toplot[,5] = as.numeric(toplot[,5])
    toplot[,6] = 0
    
    player1df = toplot %>% filter(grepl(player1, Player))
    player2df = toplot %>% filter(grepl(player2, Player))
    
    for (i in 1:nrow(player1df)){if (i < roll_num){player1df[,6][i] = sum(player1df$X3P[1:i])/sum(player1df$X3PA[1:i])} else{player1df[,6][i] = sum(player1df$X3P[(i-(roll_num-1)):i])/sum(player1df$X3PA[(i-(roll_num-1)):i])}}
    player1df = player1df[(roll_num):nrow(player1df),]
    for (i in 1:nrow(player2df)){if (i < roll_num){player2df[,6][i] = sum(player2df$X3P[1:i])/sum(player2df$X3PA[1:i])} else{player2df[,6][i] = sum(player2df$X3P[(i-(roll_num-1)):i])/sum(player2df$X3PA[(i-(roll_num-1)):i])}}
    player2df = player2df[(roll_num):nrow(player2df),]
    toplot = rbind.data.frame(player1df, player2df)
    colnames(toplot)[6] = "V5"
  } else{
    toplot = GL[,which(names(GL) %in% c("Player", "G", "Date", "FT", "FTA"))]
    toplot[,4] = as.numeric(toplot[,4])
    toplot[,5] = as.numeric(toplot[,5])
    toplot[,6] = 0
    
    player1df = toplot %>% filter(grepl(player1, Player))
    player2df = toplot %>% filter(grepl(player2, Player))
    
    for (i in 1:nrow(player1df)){if (i < roll_num){player1df[,6][i] = sum(player1df$FT[1:i])/sum(player1df$FTA[1:i])} else{player1df[,6][i] = sum(player1df$FT[(i-(roll_num-1)):i])/sum(player1df$FTA[(i-(roll_num-1)):i])}}
    player1df = player1df[(roll_num):nrow(player1df),]
    for (i in 1:nrow(player2df)){if (i < roll_num){player2df[,6][i] = sum(player2df$FT[1:i])/sum(player2df$FTA[1:i])} else{player2df[,6][i] = sum(player2df$FT[(i-(roll_num-1)):i])/sum(player2df$FTA[(i-(roll_num-1)):i])}}
    player2df = player2df[(roll_num):nrow(player2df),]
    toplot = rbind.data.frame(player1df, player2df)
    colnames(toplot)[6] = "V5"
  }
} else{
  toplot = GL[,which(names(GL) %in% c("Player", "G", "Date", sel))]
  toplot[,sel] = as.numeric(toplot[,sel])
  toplot[,5] = 0
  player1df = toplot %>% filter(grepl(player1, Player))
  player2df = toplot %>% filter(grepl(player2, Player))
  for (i in 1:length(player1df[,sel])){if (i < roll_num){player1df[,5][i] = mean(player1df[,sel][1:i])} else{player1df[,5][i] = mean(player1df[,sel][(i-(roll_num-1)):i])}}
  player1df = player1df[(roll_num):nrow(player1df),]
  for (i in 1:length(player2df[,sel])){if (i < roll_num){player2df[,5][i] = mean(player2df[,sel][1:i])} else{player2df[,5][i] = mean(player2df[,sel][(i-(roll_num-1)):i])}}
  player2df = player2df[(roll_num):nrow(player2df),]
  toplot = rbind.data.frame(player1df, player2df)
}

if (player1 != player2){toplot$Player = factor(toplot$Player, levels = c(toplot$Player[1], toplot$Player[nrow(toplot)]))} else{toplot$Player = toplot$Player}
tmtab = table(GL$Player, GL$Tm)
if (colnames(tmtab) %>% length() == 1){
  playerTeam = colnames(tmtab)
} else{
  tmtab = tmtab[which(rownames(table(GL$Player, GL$Tm)) == GL$Player[1]),]
  playerTeam = names(which(tmtab == max(tmtab)))
}

tmhex = read.csv('newdata/teamabbreviations.csv')[,-1] # team hex colors
if (playerTeam %in% tmhex$abb){color = tmhex$hex[grep(playerTeam, tmhex$abb)]} else{color = "black"}

toplot %>% ggplot(aes(x = G, y = V5)) + geom_line(aes(color = as.factor(Player))) + geom_point(aes(color = as.factor(Player)), data = toplot[which(toplot$V5 == max(toplot$V5))[1],]) + theme_bw() + scale_y_continuous(name = user_sel) + ggtitle(paste(toplot$Player[1], "vs.", toplot$Player[nrow(toplot)]), subtitle = paste0(roll_num, "-game rolling average")) + geom_text(data = toplot[which(toplot$V5 == max(toplot$V5))[1],], aes(label = Date), position = position_nudge(x = ifelse(which(toplot$V5 == max(toplot$V5))[1] > median(nrow(toplot)), -nrow(toplot)/18, nrow(toplot)/18))) + scale_color_manual("", values = c(color, "grey70")) + theme(legend.position = "top") + scale_x_continuous("Game")


