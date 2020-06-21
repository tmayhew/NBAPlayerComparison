player1 = "Zion Williamson"
player1d = scrape_nbadf(get_htmlnba(player1), player1) %>% mutate(GmSc = 100*PER + 100*WS + 2*PTS + (0.4*FG) - (0.7*FGA) - (0.4*(FTA - FT)) + TRB + (0.7*AST) - (0.4*PF)) %>% arrange(desc(GmSc))
team = player1d %>% select(Tm) %>% head(5) %>% group_by(Tm) %>% summarize(n = n(),.groups = 'drop') %>% arrange(desc(n))
playerTeam = ifelse(team$n[1] > 0.50*(min(nrow(player1d), 5)), team$Tm[1], "None")
tmhex = read.csv('newdata/teamabbreviations.csv')[,-1]
if (playerTeam %in% tmhex$abb){color = tmhex$hex[grep(playerTeam, tmhex$abb)]} else{color = "black"}
data.frame(tmhex, val = 1) %>% ggplot(aes(x = abb, y = val)) + geom_bar(stat = "identity", aes(fill = as.factor(abb))) + scale_fill_manual(values = tmhex$hex)

