library(rvest)
library(tidyverse)
library(formattable)
library(stringi)
options(stringsAsFactors = F)

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

cdf$names[which(grepl("Dan Anderson",cdf$names))][1] = "Dan Anderson (1)"
cdf$names[which(grepl("Dan Anderson",cdf$names))][2] = "Dan Anderson"
cdf$names[which(grepl("Bill Bradley",cdf$names))][1] = "Bill Bradley"
cdf$names[which(grepl("Bill Bradley",cdf$names))][2] = "Bill Bradley (2)"
cdf$names[which(grepl("Dee Brown \\[II/Jr.\\]",cdf$names))] = "Dee Brown (2)"
cdf$names[which(grepl("Roger Brown",cdf$names))][1] = "Roger Brown (1)"
cdf$names[which(grepl("Roger Brown",cdf$names))][2] = "Roger Brown"
cdf$names[which(grepl("Mark Davis \\[II/Jr.\\]",cdf$names))] = "Mark Davis (2)"
cdf$names[which(grepl("Mike Davis \\[II/Jr.\\]",cdf$names))] = "Mike Davis (2)"
cdf$names[which(grepl("Mike Dunleavy \\[II/Jr.\\]",cdf$names))] = "Mike Dunleavy Jr."
cdf$names[which(grepl("Patrick Ewing \\[II/Jr.\\]",cdf$names))] = "Patrick Ewing Jr."
cdf$names[which(grepl("Cedric Henderson \\[II/Jr.\\]",cdf$names))] = "Cedric Henderson (2)"
cdf$names[which(grepl("Gerald Henderson \\[II/Jr.\\]",cdf$names))] = "Gerald Henderson (2)"
cdf$names[which(grepl("Luke Jackson \\[II/Jr.\\]",cdf$names))] = "Luke Jackson (2)"
cdf$names[which(grepl("Tony Jackson \\[II/Jr.\\]",cdf$names))] = "Tony Jackson (2)"
cdf$names[which(grepl("Mike James \\[II/Jr.\\]",cdf$names))] = "Mike James (2)"
cdf$names[which(grepl("Chris Johnson \\[II/Jr.\\]",cdf$names))] = "Chris Johnson (2)"
cdf$names[which(grepl("Eddie Johnson \\[II/Jr.\\]",cdf$names))] = "Eddie Johnson (2)"
cdf$names[which(grepl("George Johnson \\[II/Jr.\\]",cdf$names))] = "George Johnson (2)"
cdf$names[which(grepl("George Johnson \\[III\\]",cdf$names))] = "George Johnson (3)"
cdf$names[which(grepl("Ken Johnson \\[II/Jr.\\]",cdf$names))] = "Ken Johnson (2)"
cdf$names[which(grepl("Larry Johnson \\[II/Jr.\\]",cdf$names))] = "Larry Johnson (2)"
cdf$names[which(grepl("Bobby Jones \\[II/Jr.\\]",cdf$names))] = "Bobby Jones (2)"
cdf$names[which(grepl("Charles Jones \\[II/Jr.\\]",cdf$names))] = "Charles Jones (2)"
cdf$names[which(grepl("Charles Jones \\[III\\]",cdf$names))] = "Charles Jones (3)"
cdf$names[which(grepl("Mark Jones \\[II/Jr.\\]",cdf$names))] = "Mark Jones (2)"
cdf$names[which(grepl("George King \\[II/Jr.\\]",cdf$names))] = "George King (2)"
cdf$names[which(grepl("David Lee",cdf$names))][1] = "David Lee (1)"
cdf$names[which(grepl("David Lee",cdf$names))][2] = "David Lee"
cdf$names[which(grepl("Tony Mitchell \\[II/Jr.\\]",cdf$names))] = "Tony Mitchell (2)"
cdf$names[which(grepl("Jim Paxson \\[II/Jr.\\]",cdf$names))] = "Jim Paxson (2)"
cdf$names[which(grepl("Walker Russell \\[II/Jr.\\]",cdf$names))] = "Walker Russell (2)"
cdf$names[which(grepl("Charles Smith \\[II/Jr.\\]",cdf$names))] = "Charles Smith (2)"
cdf$names[which(grepl("Charles Smith \\[III\\]",cdf$names))] = "Charles Smith (3)"
cdf$names[which(grepl("Chris Smith \\[II/Jr.\\]",cdf$names))] = "Chris Smith (2)"
cdf$names[which(grepl("Greg Smith \\[II/Jr.\\]",cdf$names))] = "Greg Smith (2)"
cdf$names[which(grepl("Michael Smith \\[II/Jr.\\]",cdf$names))] = "Michael Smith (2)"
cdf$names[which(grepl("Sam Smith",cdf$names))][1] = "Sam Smith (1)"
cdf$names[which(grepl("Sam Smith",cdf$names))][2] = "Sam Smith"
cdf$names[which(grepl("Jack Turner \\[II/Jr.\\]",cdf$names))] = "Jack Turner (2)"
cdf$names[which(grepl("David Vaughn",cdf$names))][1] = "David Vaughn (1)"
cdf$names[which(grepl("David Vaughn",cdf$names))][2] = "David Vaughn"
cdf$names[which(grepl("Marcus Williams \\[II/Jr.\\]",cdf$names))] = "Marcus Williams (2)"
cdf$names[which(grepl("Reggie Williams \\[II/Jr.\\]",cdf$names))] = "Reggie Williams (2)"
cdf$names[which(grepl("Sam Williams \\[II/Jr.\\]",cdf$names))] = "Sam Williams (2)"
cdf$names[which(grepl("Bobby Wilson",cdf$names))][1] = "Bobby Wilson (1)"
cdf$names[which(grepl("Bobby Wilson",cdf$names))][2] = "Bobby Wilson"
cdf$names[which(grepl("Chris Wright \\[II/Jr.\\]",cdf$names))] = "Chris Wright (2)"

#cdf$names[which(grepl("\\[",cdf$names))]
#cdf$names[which(grepl("\\[",cdf$names))] %>% length()
#fin %>% filter(grepl("Chris Wright",Player)) %>% select(Player, Yr, everything())

write.csv(cdf, "cdf.csv")









