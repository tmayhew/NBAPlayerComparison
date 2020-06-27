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



