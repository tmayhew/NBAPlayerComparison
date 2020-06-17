# Scraping All Stars by Year
library(rvest)
library(tidyverse)
options(stringsAsFactors = FALSE)
#year = 1951-2020
'for (i in 1951:1997){
  year = i
  html = read_html(paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html"))
  whole_body = html %>% html_nodes(xpath = '//comment()') %>% html_text() %>% paste(collapse = "") %>% read_html()
  nm = whole_body %>% html_node("div#div_all_star_game_rosters") %>% html_nodes("p") %>% html_text()
  for (i in 1:length(nm)){
    f = strsplit(nm[i], " ")[[1]][1:(length(strsplit(nm[i], " ")[[1]])-1)]
    first = paste(f, collapse = " ")
    spfirst = strsplit(first, '')[[1]]
    firstname = paste(spfirst[2:length(spfirst)], collapse = "")
    
    last = strsplit(nm[i], " ")[[1]][length(strsplit(nm[i], " ")[[1]])]
    sp = strsplit(last, "")[[1]]
    letters = c()
    for (j in 1:length(sp)){
      if (grepl("[0-9]", sp[j]) | sp[j] == "(" | sp[j] == ")" | sp[j] == "\u00A0" | sp[j] == "*"){
        letters = letters
      } else{
        letters = c(letters, sp[j])
      }
    }
    lastname = paste(letters[1:(length(letters))], collapse = "")
    nm[i] = paste(firstname, lastname, collapse = " ")
  }
  
  nm = iconv(nm, from="UTF-8",to="ASCII//TRANSLIT")
  nm1 = data.frame(Yr = rep(year, length(nm)), Player = nm)
  nm1
  write.csv(nm1, paste0("aslists/as", year, ".csv"))
}'

allstar = NULL
for (i in 1951:2020){
  allstar = rbind.data.frame(allstar, read.csv(paste0("aslists/as", i, ".csv")))
}
allstarcomplete = allstar %>% transmute(
  Yr,
  Player, 
  allstar = 1
) 
allstarcomplete %>% glimpse()
allstarcomplete %>% summary()
write.csv(allstarcomplete, 'aslists/aacomplete.csv')




