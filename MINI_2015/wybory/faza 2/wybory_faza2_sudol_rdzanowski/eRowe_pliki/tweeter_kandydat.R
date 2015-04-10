############## (SENSOWNE?) WCZYTANIE TWEETOW O KANDYDATACH

library("tm")
library("stringi")
library("twitteR")
library("streamR")
library("ROAuth")

# sciezka dostepu do katalogu, w ktorym skatalogowano kandydatow
dir = "C:\\Users\\Mort\\Desktop\\projekt_r\\twitter_osoby"
# ustawiam katalog roboczy na dir
setwd(dir = dir)

# wyciagam z katalogu roboczego to co sie w nim znajduje
# zakladamy, ze w tym katalogu roboczym znajduja sie foldery 
# zawierajace foldery (nazwiska kandydatow), zas w kazdym z 
# tych folderow znajduja sie foldery o nazwie daty pobrania, 
# z kolei te foldery zawieraja pliki txt (tweety)
tekst <- character()
o_kim <- character()
data <- character()

foldery_nazwiska <- dir()
for(i in 1:length(foldery_nazwiska)){
  foldery_daty <- dir(foldery_nazwiska[i])
  for(j in 1:length(foldery_daty)){
    pliki <- dir(stri_paste(foldery_nazwiska[i], foldery_daty[j], sep = "\\"))
    if(length(pliki) > 0){
      for(k in 1: length(pliki)){
        nazwa <- stri_paste(foldery_nazwiska[i], foldery_daty[j], pliki[k], sep = "\\")
        tek <- readLines(con = nazwa)
        #czyszczenie
        tek <- unique(tek)
        tekst <- c(tekst, tek)
        o_kim <- c(o_kim, rep(foldery_nazwiska[i], length(tek)))
        data <- c(data, rep(foldery_daty[j], length(tek)))
      }
    }
  }
}


dane_tweet_kandydat <- data.frame(tekst = tekst, 
                                  portal = rep("tweeter_kandydat", length(tekst)), 
                                  data = data, 
                                  waga = rep(1, length(tekst)),
                                  o_kim = o_kim, 
                                  stringsAsFactors = FALSE)

dane_tweet_kandydat <- dane_tweet_kandydat[!duplicated(dane_tweet_kandydat[,c('tekst')]),]


write.csv(dane_tweet_kandydat, "C:\\Users\\Mort\\Desktop\\projekt_r\\eRowe_pliki\\raport_projekt\\dane_tweet_kandydat.csv")
