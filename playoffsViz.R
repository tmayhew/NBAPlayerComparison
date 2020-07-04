library(tidyverse)
library(rvest)

playoffsfinaldf = read.csv("playoffsfinaldf.csv")[,-1]
playoffsfinaldf %>% head(20)
input$player1 = "Klay Thompson"
input$player2 = "Draymond Green"

player1SC = scrape_nbadf(htmlinput = get_htmlnba(input$player1), input$player1)

sel = "totPlayoffShare"
players = c(input$player1, input$player2)
compdf = playoffsfinaldf %>% filter(Player %in% players)

if (input$player1 != input$player2){compdf$Player = factor(compdf$Player, levels = c(input$player2, input$player1))} else{compdf$Player = compdf$Player}
compdf = compdf %>% arrange(Player, Yr)
for (i in 1:nrow(compdf)){
  if (i == 1){
    compdf$Yr.C[i] = 1
  } else{
    if (compdf$Player[i] == compdf$Player[i-1]){
      compdf$Yr.C[i] = compdf$Yr.C[i-1] + 1
    } else{
      compdf$Yr.C[i] = 1
    }
  }
}
ind = which(names(compdf) == sel)

player1d = player1SC
p1ord = player1d %>% arrange(desc(PER))
team = p1ord %>% select(Tm) %>% head(5) %>% group_by(Tm) %>% summarize(n = n(),.groups = 'drop') %>% arrange(desc(n))
playerTeam = ifelse(team$n[1] > 0.50*(min(nrow(player1d), 5)), team$Tm[1], "None")
if (playerTeam %in% tmhex$abb){color = tmhex$hex[grep(playerTeam, tmhex$abb)]} else{color = "black"}

for (i in 1:nrow(compdf)){
  sp = strsplit(as.character(compdf$Yr[i]),"")[[1]]
  l2 = paste(sp[3:4], collapse = "")
  compdf$Season[i] = paste0("'",l2)
}

compdf = compdf %>% arrange(Yr.C)
for (i in 1:nrow(compdf)){
  if (i %% 2 == 1){
    if (is.na(compdf$totPlayoffShare[i+1])){
      compdf$Season[i] = compdf$Season[i]
    } else{
      if (abs(compdf$totPlayoffShare[i] - compdf$totPlayoffShare[i+1]) < 0.01 & compdf$Yr.C[i] == compdf$Yr.C[i+1]){
        compdf$Season[i] = ""
      } else{
        compdf$Season[i] = compdf$Season[i]
      }
    }
  } else{
    compdf$Season[i] = compdf$Season[i]
  }
}

ggplot(compdf, aes(x = compdf$Yr.C, y = compdf[,ind])) +
    geom_line(aes(color = compdf$Player), linetype = "dashed") + 
    geom_point(aes(color = compdf$Player), size = I(7.5)) +
    scale_x_continuous("Year of Career", breaks = seq(min(compdf$Yr),max(compdf$Yr),1)) + scale_y_continuous(paste(sel)) +
    theme_bw() + scale_color_manual("",values = c("grey70", color)) + geom_text(aes(label = Season), color = "white")
