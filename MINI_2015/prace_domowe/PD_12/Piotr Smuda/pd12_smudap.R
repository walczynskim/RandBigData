load("movies10IMDB.rda", verbose=FALSE)
library(parallel)

moviesExtractWords <- function(po_czym){
      sapply(movies10IMDB[,po_czym], function(x){
            x <- as.character(x)
            tolower(
                  ifelse(po_czym=='Title',
                         strsplit(x," "),
                         strsplit(x,","))[[1]])
      })
}

moviesIntersect <- function(x,y){
      p <- length(intersect(x,y))/length(union(x,y))
      return(p)
}

doSimilarityMatrix <- function(po_czym, n, FUN){
      slowaRozdzielone <- moviesExtractWords(po_czym)
      slowaRozdzielone <- tail(slowaRozdzielone,n)
      start.time <- Sys.time()
      ile <- detectCores()
      cl <- makeCluster(ile)
      clusterExport(cl, c("slowaRozdzielone","FUN"), envir=environment())
      X <- parSapply(cl, slowaRozdzielone, function(x){
            sapply(slowaRozdzielone, function(y){
                  if(all(is.na(x),is.na(y))==FALSE)
                        FUN(x,y) else
                              NA
            })
      })
      stopCluster(cl)
      stop.time <- Sys.time()

      rownames(X) <- colnames(X) <- 1:n
      print(stop.time-start.time)
      return(X)
}

smudap_genres_pd12<-doSimilarityMatrix("Genres",2000,moviesIntersect)
save(smudap_genres_pd12,file=("smudap_genres_pd12.rda"))
smudap_genres_pd12[1:10,1:10]

