## Ponizsza funkcja zwraca liste ramek danych zawierajacych oficjalne 
## tweety kandydatow na urzad Prezydenta RP: Andrzeja Dudy, Bronislawa 
## Komorowskiego, Magdaleny Ogorek, Adama Jarubasa, Janusza Palikota oraz 
## Janusza Korwin-Mikke. Ponadto, dla kazdej ramki danych funkcja ta tworzy 
## plik tekstowy z tweetami.

## Argument wejsciowy: data, od ktorej zbierane sa tweety
## Zwracana wartosc: nazwana lista ramek danych

oficjalneTweety <- function(data, katalog){
   
   library(twitteR)
   library(stringi)
   
   consumerKey <- "mruTEgk5DpvU0XM8dk3aPRhVx"
   consumerSecret <- "B2NOHpA7uVrap95LOwTssStx8HfWUgSDbtTo0OJhQrXQEmi1oT"
   access_token <- "51761035-QqJMM7EYxwwV5QnGAelnEq6HVg6RQrUYOFMyw9pho"
   access_secret <- "FteRrg5TjcjyW37qMfLBeXaDsFeYQ7AUFgWFmHS1cJqO5"
   
   setup_twitter_oauth(consumerKey, consumerSecret, access_token, 
                       access_secret)
   
   setwd(katalog)
   
   listaTweetow <- list(6)
   
   oficjalneProfile <- c("AndrzejDuda2015", "Komorowski", "ogorekmagda", 
                         "JarubasAdam", "Palikot_Janusz", "JkmMikke")
   
   data_od <- as.POSIXct(data)
   num_od <- unclass(data_od)
   
   for(i in seq_along(oficjalneProfile)){
      tweety <- userTimeline(oficjalneProfile[i], n = 200)
      df_tweety <- twListToDF(tweety)
      
      daty <- df_tweety$created
      num_daty <- unclass(daty)
      
      df_tweety_od <- df_tweety[num_daty > num_od, ]
      
      # kazda ramka danych zawiera tweety, oficjalna nazwe profilu kandydata 
      # na Twitterze oraz czas powstania tweeta
      listaTweetow[[i]] <- data.frame(tekst = df_tweety_od$text, 
                                      kandydat = df_tweety_od$screenName, 
                                      data = df_tweety_od$created)
      
      # kazda ramka danych zapisywana jest jako plik tekstowy
      write.table(listaTweetow[[i]], 
                  file = stri_paste("Kandydat", i, ".txt"))
   }
   
   structure(listaTweetow, names = c("Andrzej Duda", "Bronisław Komorowski", 
                                     "Magdalena Ogórek", "Adam Jarubas", 
                                     "Janusz Palikot", 
                                     "Janusz Korwin-Mikke"))
}


data1 <- "2015-03-10"
katalog1 <- "C:/Dane/Pawel_2/PW/R_Big_Data/tweety"
oficjalneTweety(data1, katalog2)

data2 <- "2015-03-12"
katalog2 <- "C:/Dane/Pawel_2/PW/R_Big_Data/tweety"
oficjalneTweety(data2, katalog2)

data3 <- "2015-03-05"
katalog3 <- "C:/Dane/Pawel_2/PW/R_Big_Data/tweety"
oficjalneTweety(data3, katalog3)




