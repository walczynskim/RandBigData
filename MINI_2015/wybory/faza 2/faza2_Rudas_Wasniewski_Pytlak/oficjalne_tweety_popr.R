## Ponizsza funkcja zwraca liste ramek danych zawierajacych oficjalne 
## tweety kandydatow na urzad Prezydenta RP: Andrzeja Dudy, Bronislawa 
## Komorowskiego, Magdaleny Ogorek, Adama Jarubasa, Janusza Palikota oraz 
## Janusza Korwin-Mikke. Ponadto, dla kazdej ramki danych funkcja ta tworzy 
## plik tekstowy z tweetami.

## Argument wejsciowy: data, od ktorej zbierane sa tweety
## Zwracana wartosc: nazwana lista ramek danych

oficjalneTweety <- function(katalog, data, data_pob, liczba){
   
   library(twitteR)
   library(stringi)
   
   consumerKey <- "mruTEgk5DpvU0XM8dk3aPRhVx"
   consumerSecret <- "B2NOHpA7uVrap95LOwTssStx8HfWUgSDbtTo0OJhQrXQEmi1oT"
   access_token <- "51761035-QqJMM7EYxwwV5QnGAelnEq6HVg6RQrUYOFMyw9pho"
   access_secret <- "FteRrg5TjcjyW37qMfLBeXaDsFeYQ7AUFgWFmHS1cJqO5"
   
   setup_twitter_oauth(consumerKey, consumerSecret, access_token, 
                       access_secret)
   
   setwd(katalog)
   
   listaTweetow <- list()
   
   kandydaci <- c("Andrzej_Duda", "Bronislaw_Komorowski", 
                  "Magdalena_Ogorek", "Adam_Jarubas", "Janusz_Palikot", 
                  "Janusz_Korwin-Mikke", "Pawel_Kukiz")

   oficjalneProfile <- c("AndrzejDuda2015", "Komorowski", "ogorekmagda", 
                         "JarubasAdam", "Palikot_Janusz", "JkmMikke", 
                         "PrezydentKukiz")

   if(file.exists(stri_paste(katalog, "/", data_pob))){
      
      wczytajDate <- readLines(stri_paste(katalog, "/", data_pob))
      data_od <- as.POSIXct(wczytajDate)
      num_od <- unclass(data_od)
      
      for(i in seq_along(oficjalneProfile)){
         tweety <- userTimeline(oficjalneProfile[i], n = liczba)
         df_tweety <- twListToDF(tweety)
         
         daty <- df_tweety$created
         num_daty <- unclass(daty)
         
         df_tweety_od <- df_tweety[num_daty > num_od, ]
         
         df_tweety_od$text <- stri_replace_all_regex(df_tweety_od$text, 
                                                     "[[:punct:]]", "")
         
         #df_tweety_od$text <- enc2utf8(df_tweety_od_text)
         
         # kazda ramka danych zawiera tweety, oficjalna nazwe profilu kandydata 
         # na Twitterze oraz czas powstania tweeta
         listaTweetow[[i]] <- data.frame(tekst = df_tweety_od$text, 
                                         kandydat = df_tweety_od$screenName, 
                                         data = df_tweety_od$created)
         
         # kazda ramka danych zapisywana jest jako plik tekstowy, nowe dane sa
         # nadpisywane
         write.table(listaTweetow[[i]], 
                     file = stri_paste(kandydaci[i], ".txt"), append = TRUE, 
                     row.names = FALSE, col.names = FALSE)
            
      }
      
      writeLines(as.character(Sys.time()), stri_paste(katalog, "/", data_pob))
      
      structure(listaTweetow, names = c("Andrzej Duda", "Bronislaw Komorowski", 
                                        "Magdalena Ogorek", "Adam Jarubas", 
                                        "Janusz Palikot", 
                                        "Janusz Korwin-Mikke", "Pawel Kukiz"))
      
   } else {
      
      for(i in seq_along(oficjalneProfile)){
         
         tweety <- userTimeline(oficjalneProfile[i], n = liczba)
         df_tweety <- twListToDF(tweety)
         
         daty <- df_tweety$created
         num_daty <- unclass(daty)
         num_od <- unclass(as.POSIXct(data))
         df_tweety_od <- df_tweety[num_daty >= num_od, ]
         
         df_tweety_od$text <- stri_replace_all_regex(df_tweety_od$text, 
                                                     "[[:punct:]]", "")
         
         #df_tweety_od$text <- enc2utf8(df_tweety_od$text)
         
         # kazda ramka danych zawiera tweety, oficjalna nazwe profilu kandydata 
         # na Twitterze oraz czas powstania tweeta
         listaTweetow[[i]] <- data.frame(tekst = df_tweety_od$text, 
                                         kandydat = df_tweety_od$screenName, 
                                         data = df_tweety_od$created)
         
         # ramka danych zapisywana jest jako plik tekstowy
         write.table(listaTweetow[[i]], 
                     file = stri_paste(kandydaci[i], ".txt"), append = TRUE, 
                     row.names = FALSE)
         
      }
      
      writeLines(as.character(Sys.time()), stri_paste(katalog, "/", data_pob))
      
      structure(listaTweetow, names = c("Andrzej Duda", "Bronislaw Komorowski", 
                                        "Magdalena Ogorek", "Adam Jarubas", 
                                        "Janusz Palikot", 
                                        "Janusz Korwin-Mikke", "Pawel Kukiz"))
   }
}


######################################################################
katalog2 <- "C:/Dane/Pawel_2/PW/R_Big_Data/Projekt1/tweety"
#data1 <- stri_paste(katalog4, "/data_poboru.txt")
data2 <- "2015-03-30"
data_pob2 <- "data_poboru.txt"
liczba2 <- 200
oficjalneTweety(katalog2, data2, data_pob2, liczba2)
   
katalog1 <- "C:/Dane/Pawel_2/PW/R_Big_Data/Projekt1/tweety"
#data1 <- stri_paste(katalog4, "/data_poboru.txt")
data1 <- "2015-02-07"
data_pob1 <- "data_poboru.txt"
liczba1 <- 900
oficjalneTweety(katalog1, data1, data_pob1, liczba1)

#katalog2 <- "C:/Dane/Pawel/R_Big_Data/Wybory/tweety"
#data1 <- stri_paste(katalog4, "/data_poboru.txt")
#data2 <- "2015-03-29"
#data_pob2 <- "data_poboru.txt"
#liczba2 <- 30
#oficjalneTweety(katalog2, data2, data_pob2, liczba2)





