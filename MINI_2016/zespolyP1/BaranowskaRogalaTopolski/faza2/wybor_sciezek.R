#library("dplyr")
library("stringi")

#-----------------------# FUNKCJE POMOCNICZE do obrobki danych #-----------------------#
#funkcja tworzy puste ramke danych ze sciezkami
  create_emptyPaths <- function(all_exhibits){
    emptyPaths <- data.frame(from = "---", to = "---")
    for(i in 1:length(all_exhibits)){
      paths <- data.frame(from = c("---", rep(all_exhibits[i], 58)), to = c(all_exhibits[i], all_exhibits[-i], "---"))
      emptyPaths <- rbind(emptyPaths, paths)
    }
    emptyPaths <- emptyPaths[-1,]
    hours <- c(8:18, "after 19")
    emptyPaths[,hours] <- 0
    emptyPaths
  }

#funkcja wybiera sciezki dla zwiedzajacego (usuwam powroty do tego samego eksponatu)
visitor_path <- function(data = data, i){  
  tmp <- data[data$visitor_id == visitors[i],]
  from <- c("---", as.character(tmp$exhibit))
  to <- c(as.character(tmp$exhibit), "---")
  hour <- c(stri_sub(tmp$begin_time[1], from = 1, to = 2), stri_sub(tmp$end_time, from = 1, to = 2)) 
  new_path <- data.frame(from = from, to = to, hour = hour)[from != to,]
  new_path
}

#do zbiorczej ramki danych dodaje sciezki jednego odwiedzajacego (zwraca uaktualniona wersje)
add_path <- function(new_path, paths){
  for(j in 1:nrow(new_path)){
    from = as.character(new_path[j, "from"])
    to = as.character(new_path[j, "to"])
    hour = as.character(new_path[j, "hour"])
    path_row <- which(paths$from == from & paths$to == to)
    if(hour %in% as.character(8:18)){
      paths[path_row, hour] <- paths[path_row, hour] + 1 
    }else{
      paths[path_row, "after 19"] <- paths[path_row, "after 19"] + 1      
    }
  }
  paths
}

#test: sum(paths[,-(1:2)]) == nrow(new_path) OK

###do zbiorczej ramki danych dopisuje pierwszy i ostatni eksponat
# add_first_last <- function(new_path){
#   first = as.character(new_path[1,1])
#   first_row <- which(first_last$exhibit == first)
#   first_last[first_row, 2] <<- first_last[first_row, 2] + 1
#   
#   last <- as.character(tail(new_path[,2], n=1))
#   last_row <- which(first_last$exhibit == last)
#   first_last[last_row, 3] <<- first_last[last_row, 3] + 1
# }


#-----------------------# GLOWNA PETLA obrobka danych #-----------------------#
setwd("/home/zosia/studia/RandBigData/Projekt_I/ScrappingTextMining/sciezki_dane2013")
allExhibits <- unique(stri_sub(list.files("../Raporty"), from = 11, to = -8))
emptyPaths <- create_emptyPaths(allExhibits)
filenames <- list.files()

for(filename in filenames){
    data <- read.table(filename, sep = ",", header = TRUE)[,-1]

    visitors <- unique(data$visitor_id)
    paths <- emptyPaths
      
    for(visitor_nr in 1:length(visitors)){    
        new_path <- visitor_path(data, visitor_nr)
        paths <- add_path(new_path, paths)
    }

    write.csv(paths, file= paste0("../popularneSciezki/", filename))
} 