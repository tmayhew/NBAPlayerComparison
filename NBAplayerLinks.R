library(rvest)
library(tidyverse)
library(formattable)
library(stringi)
options(stringsAsFactors = F)
finaldf = read.csv("finaldf.csv")[,-1]

letters = str_to_lower(LETTERS)
letters_vec = letters;cdf = NULL
for (i in 1:length(letters_vec)){
  link = paste0("https://www.basketball-reference.com/players/", letters_vec[i], "/")
  html = read_html(link)
  t = html_table(html)[[1]]
  lastyear = t$To
  left <- html %>% html_nodes("th.left")
  names = left %>% html_nodes("a") %>% html_text()
  links = left %>% html_nodes("a") %>% html_attrs()
  links = unlist(links)
  df = cbind.data.frame(names, links, lastyear)
  df = df %>% filter(lastyear >= 1952)
  if (dim(df)[1] == 0){
    cdf = cdf
  } else{
    cdf = rbind.data.frame(cdf, df)
  }
}

for (i in 1:nrow(cdf)){
  if (i == 1){
    cdf$n[i] = 1
  } else{
    if (cdf$names[i] == cdf$names[i-1]){
      if (cdf$names[i] == cdf$names[i-2]){
        cdf$n[i] = 3
      } else{
        cdf$n[i] = 2
      }
    } else{
      cdf$n[i] = 1
    }
  }
}
for (i in 1:nrow(cdf)){
  if (cdf$n[i] == 2){
    cdf$names[i] = paste(cdf$names[i], "[II/Jr.]", collapse = "")
  } else if (cdf$n[i] == 3){
    cdf$names[i] = paste(cdf$names[i], "[III]", collapse = "")
  }
}

cdf = cdf %>% mutate(actual_link = paste0("https://basketball-reference.com", links))
cdf$names = iconv(cdf$names, from = "UTF-8", to="ASCII//TRANSLIT")
cdf$names[which(grepl("Aleksandar Dordevic", cdf$names))] = "Aleksandar Djordjevic"
cdf$names[which(grepl("Vitor Faverani", cdf$names))] = "Vitor Luiz Faverani"
cdf$names[which(grepl("Gheorghe Mure", cdf$names))] = "Gheorghe Muresan"
cdf = cdf %>% select(names, actual_link)
makeJr = function(playerName){
  if ((playerName %in% finaldf$Player)==F){
    if (grepl("II/Jr.", playerName) == F){
      trueName = playerName
    } else{
      sp = strsplit(playerName, "")[[1]]
      name = paste(sp[1:(length(sp)-9)], collapse = "")
      opII = ifelse((paste(name, "II", collapse = "")) %in% finaldf$Player, TRUE,FALSE)
      opJr = ifelse((paste(name, "Jr.", collapse = "")) %in% finaldf$Player, TRUE,FALSE)
      opIII = ifelse((paste(name, "III", collapse = "")) %in% finaldf$Player, TRUE,FALSE)
      trueName = ifelse(opII, paste(name, "II", collapse = ""), ifelse(opIII, paste(name, "III", collapse = ""), ifelse(opJr, paste(name, "Jr.", collapse = ""), playerName)))
    }
  } else{
    trueName = playerName
  }
  return(trueName)
}
for (i in 1:length(cdf$names)){cdf$names[i] = makeJr(cdf$names[i])}
DJMbenga = data.frame(names = "D.J. Mbenga", actual_link = "https://www.basketball-reference.com/players/m/mbengdj01.html")
cdf = rbind.data.frame(cdf, DJMbenga)

write.csv(cdf, "cdf.csv")
