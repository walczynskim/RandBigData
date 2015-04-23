### Przyklad wywolania ###
# oczywiscie nie nalezy tego puszczac w takiej postaci tylko w czesciach
# aby komputer sie nie zawiesil
library(stringi)
library(rvest)
library(XML)
library(parallel)
setwd('D:/szkola/R i Big Data/filmy')
linki = readLines('linki_filmy.txt')

filmy_klaster <- function(i){
  link = stri_sub(linki[i],2,(length(linki[i])-3))
  suppressWarnings(wszystko(link))
}

detectCores()#sprawdzenie liczby rdzeni
l=length(linki)

cl <- makeCluster(2)
clusterExport(cl, c("wszystko","actors","create","create2","create3","director",
                    "get_names","harvest_people","htmls_movie","summary",
                    "wydobadz","wyjmij","sprawdz" "linki"))

clusterEvalQ(cl,library(rvest))
clusterEvalQ(cl,library(stringi))
clusterEvalQ(cl,library(XML))
parSapply(cl, 1:l, filmy_klaster)
stopCluster(cl)

