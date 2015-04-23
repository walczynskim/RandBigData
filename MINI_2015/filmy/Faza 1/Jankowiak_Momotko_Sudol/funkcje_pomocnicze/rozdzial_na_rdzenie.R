library(parallel)
library(stringi)
library(XML)
library(rvest)

# tworze funkcje pomocnicza
filmy_klaster <- function(i){
   print(i)
   link = stri_sub(linki[i],2,(length(linki[i])-3))
   wszystko(link)
}

ile <- detectCores()
cl <- makeCluster(ile)
clusterExport(cl, c("wszystko","actors","create","create2","create3","director",
                    "get_names","harvest_people","htmls_movie","summary",
                    "wydobadz","wyjmij", "linki"))


clusterEvalQ(cl,library(rvest))
clusterEvalQ(cl,library(stringi))
clusterEvalQ(cl,library(XML))

info <- parSapply(cl, 39900:40000, filmy_klaster)
stopCluster(cl)
