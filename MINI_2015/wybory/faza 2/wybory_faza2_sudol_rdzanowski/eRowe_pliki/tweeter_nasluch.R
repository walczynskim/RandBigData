############## (SENSOWNE?) WCZYTANIE TWEETOW Z JSONOW

library("tm")
library("stringi")
library("twitteR")
library("streamR")
library("ROAuth")

# sciezka dostepu do katalogu, w ktorym skatalogowano tweety
dir = "C:\\Users\\Mort\\Desktop\\projekt_r\\twitter"
# ustawiam katalog roboczy na dir
setwd(dir = dir)

# wyciagam z katalogu roboczego to co sie w nim znajduje
# zakladamy, ze w tym katalogu roboczym znajduja sie foldery 
# zawierajace pliki json, nazwy tych folderow to daty
foldery <- dir()
tweet <- data.frame()

for(i in 1:length(foldery)){
  # wyciagam nazwy plikow z katalogow znajdujacych sie w roboczym
  pliki <- dir(foldery[i])
  if(length(pliki) > 0){
    for(j in 1:length(pliki)){
      tweet2 <- parseTweets(stri_paste(foldery[i], "\\", pliki[j], sep = ""), 
                            simplify = FALSE, verbose = TRUE)[c('text', 'retweet_count', 'favourites_count')]
      tweet2 <- cbind(tweet2, data = rep(foldery[i], nrow(tweet2)))
      tweet <- rbind(tweet, tweet2)
    }
  }
}

# zmienna tweet to ramka danych zawierajaca potrzebne info


###################   CZYSZCZENIE
#usuwam duplikujace sie tweety
tweet <- tweet[!duplicated(tweet[,c('text')]),]


dane_tweet_nasluch <- data.frame(tekst = tweet$text, 
                                 portal = rep("tweeter_nasluch", nrow(tweet)), 
                                 data = tweet$data, waga = rep(1, nrow(tweet)), 
                                 retweet_count = tweet$retweet_count, 
                                 favourites_count = tweet$favourites_count, 
                                 stringsAsFactors = FALSE)


write.csv(dane_tweet_nasluch, "C:\\Users\\Mort\\Desktop\\projekt_r\\eRowe_pliki\\raport_projekt\\dane_tweet_nasluch.csv")



