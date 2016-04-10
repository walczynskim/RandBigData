#-----------------------# FUNKCJE POMOCNICZE do analizy wynikow #-----------------------#

#Funkcja wybiera najpopularniejsze poczatki i konce tras
mostPopularStart <- function(first_last, n = 3){
  order_first <- order(-first_last$times_first)
  mp <- first_last[order_first,][1:n,1:2]
  mp <- mp[mp$times_first > 0,]
  #sum <- sum(first_last$times_first)
  #frequencies <- mp$times_first/sum
  as.character(mp$exhibit)
}

mostPopularStop <- function(first_last, n = 3){
  order_last <- order(-first_last$times_last)
  mp <- first_last[order_last,][1:n,c(1,3)]
  mp <- mp[mp$times_last > 0,]
  #sum <- sum(first_last$times_last)
  #frequencies <- mp$times_last/sum
  as.character(mp$exhibit)
}

#Funkcja wskazuje kolejny eksponat/ eksponaty po punkcie start
nextExhibit <- function(start, paths, returnFrequency = FALSE){
  options <- paths[paths$from == start[1],]
  mp <- options[options$times == max(options$times),]
  if(returnFrequency){
    return(list(name = as.character(mp$to), frequency = mp$times[1]/sum(options$times)))
  }
  as.character(mp$to)
}

#Funkcja tworzy najbardziej prawdopodobna sciezke z punktu start
createPath <- function(start, stop_points, paths){
  visited <- character(0)
  while(!(start %in% visited)){
    visited <- cbind(visited,start)
    if(start %in% stop_points) break()
    start <- nextExhibit(start, paths)
    start <- start[sample(1:length(start)) == 1]
  }
  as.vector(visited)
}

#-----------------------# ANALIZA SCIEZEK #-----------------------#
start <- mostPopularStart(first_last, n = 5)
stop_points <- mostPopularStop(first_last, n = 1)

popular_paths <- list()
for(i in 1:length(start)){
  popular_paths[[start[i]]] <- createPath(start[i], stop_point, paths)
}

#WYNIK: popular_paths