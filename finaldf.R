library(tidyverse)
# percentiles
for (j in 6:ncol(finaldf)){
  finaldf[,j] = 100*((finaldf[,j] + abs(min(finaldf[,j])))/max(finaldf[,j]))
}
