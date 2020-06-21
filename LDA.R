# LDA
finaldf = read.csv("finaldf.csv")[,-1]
x1bar = finaldf %>% filter(allstar == 1) %>% summarise(
  x1 = mean(Eff),
  x2 = mean(Vol)
) %>% as.matrix() %>% t()
x2bar = finaldf %>% filter(allstar == 0) %>% summarise(
  x1 = mean(Eff),
  x2 = mean(Vol)
) %>% as.matrix() %>% t()
S1 = finaldf %>% filter(allstar == 1) %>% select(Eff, Vol) %>% cov()
S2 = finaldf %>% filter(allstar == 0) %>% select(Eff, Vol) %>% cov()
Sp = ((finaldf %>% filter(allstar == 1) %>% nrow() - 1)*S1 + (finaldf %>% filter(allstar == 0) %>% nrow() - 1)*S2)/(finaldf %>% filter(allstar == 0) %>% nrow() + finaldf %>% filter(allstar == 1) %>% nrow() - 2)
w = solve(Sp)%*%(x1bar - x2bar)
limit = (0.50)*t(w)%*%(x1bar+x2bar) + 6.25

#finaldf %>% ggplot(aes(x = Eff, y = Vol, color = as.factor(allstar))) + geom_hline(yintercept = 0, linetype = "dashed", alpha = I(.55)) + geom_vline(xintercept = 0, linetype = "dashed", alpha = I(.55)) + geom_point(alpha = I(1/2)) + scale_x_continuous("Efficiency") + scale_y_continuous("Volume") + theme_bw() + scale_color_manual(values = c("grey85", "grey15")) +
  #geom_abline(slope = (-w[1,1]/w[2,1]), intercept = (limit/w[2,1]), linetype = "dashed", color = "red2") +
  #theme(legend.position = "none") + ggtitle("Seasons Comparison", "The Efficiency/Volume Frontier") + annotate(geom="text", x=4.5, y=2.0, label="Linear Discriminant Classification Boundary", color="red",angle = 2.5)

finaldf = finaldf %>% mutate(value = 0.223775*Eff + 1.200985*Vol,limit = limit, as.pred = ifelse(value > limit, 1, 0))
pred1 = finaldf %>% filter(as.pred == 1)
pred0 = finaldf %>% filter(as.pred == 0)
paste("Players above the LDA line have probability", sum(pred1$allstar)/nrow(pred1), "of making an all-star team")
paste("Players below the LDA line have probability", sum(pred0$allstar)/nrow(pred0), "of making an all-star team")



# Select for Plot

user_names = c(player1, player2)
toplot = finaldf %>% filter(Player %in% user_names)
for (i in 1:nrow(toplot)){
  sp = strsplit(as.character(toplot$i[i]),"")[[1]]
  l2 = paste(sp[3:4], collapse = "")
  toplot$Yr[i] = paste0("'",l2)
}
toplot$Player = as.factor(toplot$Player)
toplot %>% 
  ggplot(aes(x = Eff, y = Vol, color = Player)) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = I(.55)) + 
  geom_vline(xintercept = 0, linetype = "dashed", alpha = I(.55)) + 
  scale_x_continuous("Efficiency") + scale_y_continuous("Volume") + theme_bw() + 
  scale_color_manual(values = c("black", "grey65")) + 
  geom_abline(slope = (-w[1,1]/w[2,1]), intercept = 
                (limit/w[2,1]), linetype = "dashed", color = "#d29914") + geom_point(size = I(7)) +
  geom_text(aes(label = Yr), color = "white") + annotate(geom="text", x=0.45, y=2.25, label="All-Star Line", color="#d29914",angle = 3.5, size = 6) + ggtitle("Seasons Comparison","The Efficiency/Volume Frontier") +
  theme(legend.position = c(0.10, .90))
  


