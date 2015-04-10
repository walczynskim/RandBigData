
iloscOficTweetow <- function(katalog, data, data_pob, liczba){
   
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
         
         #kazda ramka danych zawiera tweety, oficjalna nazwe profilu kandydata 
         #na Twitterze oraz czas powstania tweeta
         listaTweetow[[i]] <- data.frame(tekst = df_tweety_od$text, 
                                         kandydat = df_tweety_od$screenName, 
                                         data = df_tweety_od$created)
         
         #kazda ramka danych zapisywana jest jako plik tekstowy, nowe dane sa
         #nadpisywane
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
         
         listaTweetow[[i]] <- data.frame(tekst = df_tweety_od$text, 
                                         kandydat = df_tweety_od$screenName, 
                                         data = df_tweety_od$created)
         
         #ramka danych zapisywana jest jako plik tekstowy
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

   ilosc <- list()
   
   for(i in seq_along(kandydaci)){
      nazwisko <- read.table(stri_paste(katalog, "/", kandydaci[i], ".txt"), 
                             header = TRUE, sep = "", row.names = NULL)
      daty <- nazwisko$kandydat
      teksty <- nazwisko$row.names 
      rozdziel <- split(teksty, daty)
      ilosc[[i]] <- sapply(rozdziel, length)
   
      write.table(ilosc[[i]], file = stri_paste("ilosc", "-", kandydaci[i], 
                                                ".txt"))
   }
   
   structure(ilosc, 
             names = c("Andrzej Duda", "Bronislaw Komorowski", 
                       "Magdalena Ogorek", "Adam Jarubas", "Janusz Palikot", 
                       "Janusz Korwin-Mikke", "Pawel Kukiz"))
}

#######################################################################
katalog1 <- "C:/Dane/Pawel_2/PW/R_Big_Data/Projekt1/tweety/oficjalne_tweety"
data1 <- "2015-03-30"
data_pob1 <- "data_poboru.txt"
liczba1 <- 200
iloscOficTweetow(katalog1, data1, data_pob1, liczba1)




