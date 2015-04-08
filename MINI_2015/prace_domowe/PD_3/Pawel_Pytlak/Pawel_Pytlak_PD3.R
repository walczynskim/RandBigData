## Poprawiony kod 

library(streamR)
library(ROAuth)
library(twitteR)
library(stringi)

consumerKey <- "mruTEgk5DpvU0XM8dk3aPRhVx"
consumerSecret <- "B2NOHpA7uVrap95LOwTssStx8HfWUgSDbtTo0OJhQrXQEmi1oT"
access_token   <- "51761035-QqJMM7EYxwwV5QnGAelnEq6HVg6RQrUYOFMyw9pho"
access_secret  <- "FteRrg5TjcjyW37qMfLBeXaDsFeYQ7AUFgWFmHS1cJqO5"

#dokonujemy autoryzacji naszej aplikacji
setup_twitter_oauth(consumerKey, consumerSecret, access_token, access_secret)

## funkcja umozliwiajaca zapis tweetow w pliku tekstowym, w ktorym kazdy wiersz jest oddzielnym tweetem  
kandydat_tweet <- function(od_kiedy = as.POSIXct("2000-01-01"), f){
  
  #tworzymy wektor nazw oficjalnych profili kandydatow
  profile <- c("Komorowski", "AndrzejDuda", "ogorekmagda", "korwinmikke", 
               "Palikot_Janusz", "JarubasAdam", "PrezydentKukiz", "AnnaGrodzka", 
               "M_Kowalski1", "WandaNowicka")
  
  #tworzymy wektor nazwisk kandydatow
  nazwiska <- c("Komorowski", "Duda", "Ogorek", "Korwin", "Palikot", "Jarubas", 
                "Kukiz", "Grodzka", "Kowalski", "Nowicka")
  
  for(i in seq_along(profile)){  #robimy petle po wszystkich nazwach
    
    #tworzymy ramke danych zlozona z tweetow z okresu rownego jednej dobie
    ut <- userTimeline(profile[i], n = 100)
    ut <- twListToDF(ut)  
    ut <- ut[which(ut$created >= od_kiedy & ut$created <= od_kiedy+3600*24), ] 
    
    #z tresci poszczegolnych tweetow usuwamy nowe linie/tabulacje/powroty karetki/cudzyslowia 
    ut$text <- stri_paste('"', 
                          stri_trim_both(
                            stri_replace_all_regex(
                              ut$text, "(\\n)|(\\t)|(\\r)|(\")"," ")),'"')
    
    ut$created <- as.character(ut$created)
    n <- nrow(ut)
    
    #jesli tweety wystepuja, tworzymy nowa kolumne z nazwiskiem kandydata 
    if(n > 0){
      ut <- cbind(ut, data.frame(kandydat = nazwiska[i]))
      ut$kandydat <- as.character(ut$kandydat) 
      
      #zapisujemy kazdy wiersz ramki danych jako jeden napis 
      for(j in seq_along(ut$text)){
        writeLines(stri_flatten(paste(ut[j, ]), collapse = ";"), f)
      }
    }
  }
}


#funkcja tworzaca plik .csv z tweetami i umozliwiajaca dopisywanie nowych tweetow z ostatniej doby
twitter_kandydaci <- function(od_kiedy = as.POSIXct(as.character(Sys.Date()-1))){
  
  data <- as.character(Sys.Date())
  
  fname <- paste0(getwd(), "\\", "tweety_kandydatow", "_", data, ".csv")
  
  #jesli plik jeszcze nie istnieje, tworzymy go, a w pierwszym wierszu umieszczamy  
  #(w formie jednego napisu) nazwy kolumn rozpatrywanej wczesniej ramki danych; 
  #w przeciwnym wypadku, umozliwiamy jedynie dopisywanie nowych tweetow
  if (!file.exists(fname)){
    f <- file(fname, open = "a")
    writeLines(stri_paste('\"text\"', '\"favorited\"', '\"favoriteCount\"', 
                          '\"replyToSN\"', '\"created\"', '\"truncated\"', 
                          '\"replyToSID\"', '\"id\"', '\"replyToUID\"', 
                          '\"statusSource\"', '\"screenName\"', 
                          '\"retweetCount\"', '\"isRetweet\"', '\"retweeted\"', 
                          '\"longitude\"', '\"latitude\"', '\"lastname\"', 
                          sep = ";"), f)
  } else f <- file(fname, open = "a")
  
  kandydat_tweet(od_kiedy, f)
  
  close(f)
  return(invisible(NULL))
}

#twitter_kandydaci()


