library(rvest)
library(tidyverse)
library(formattable)
library(stringi)
options(stringsAsFactors = F)

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


cdf = cdf %>% mutate(actual_link = paste0("https://basketball-reference.com", links))
cdf$names = iconv(cdf$names, from="UTF-8",to="ASCII//TRANSLIT")
write.csv(cdf, "cdf.csv")
