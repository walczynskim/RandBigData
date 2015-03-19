## Ponizsza funkcja sluzy pobieraniu z Twittera wiadomosci zawierajacych slowa
## kluczowe zwiazane z nazwiskami glownych kandydatow na urzad Prezydenta RP: 
## Andrzeja Dudy, Bronislawa Komorowskiego, Magdaleny Ogorek, Adama Jarubasa, 
## Janusza Palikota oraz Janusza Korwin-Mikke.

## Argumenty funkcji:
## 
## czas - okres (liczony w sekundach), w jakim dokonujemy nasluchu tweetow
## katalog - sciezka do folderu, w ktorym przechowujemy plik z tweetami
## nazwa - nazwa pliku, w ktorym trzymac bedziemy interesujace nas tweety

## Wartosc zwracana: ramka danych


pobieranieTweetow <- function(czas, katalog, nazwa){
   
   # ladujemy potrzebne pakiety
   library(streamR)
   library(ROAuth)
   library(stringi)
   
   # aby miec mozliwosc korzystania z aplikacji do pobierania tweetow, musimy
   # uzyskac na to zezwolenie
   requestURL <- "https://api.twitter.com/oauth/request_token"
   accessURL <- "https://api.twitter.com/oauth/access_token"
   authURL <- "https://api.twitter.com/oauth/authorize"
   consumerKey <- "mruTEgk5DpvU0XM8dk3aPRhVx"
   consumerSecret <- "B2NOHpA7uVrap95LOwTssStx8HfWUgSDbtTo0OJhQrXQEmi1oT"
   oauthKey <- "51761035-QqJMM7EYxwwV5QnGAelnEq6HVg6RQrUYOFMyw9pho"
   oauthSecret <- "FteRrg5TjcjyW37qMfLBeXaDsFeYQ7AUFgWFmHS1cJqO5"
   
   paczka <- OAuthFactory$new(consumerKey = consumerKey, 
                              consumerSecret = consumerSecret, 
                              oauthKey = oauthKey, 
                              oauthSecret = oauthSecret, 
                              requestURL = requestURL, 
                              accessURL = accessURL, authURL = authURL)   
   
   paczka$handshake(cainfo = system.file("CurlSSL", "cacert.pem", 
                                         package = "RCurl"))
   
   # ustawiamy katalog biezacy 
   setwd(katalog)
   
   # pobieramy interesujace nas wiadomosci, ktore beda przechowywane w pliku w
   # formacie .json; nasluchujemy jedynie tweety napisane w jezyku polskim, 
   # jednakze nie precyzujemy ich lokalizacji
   filtruj <- filterStream(
      file = stri_paste(nazwa, ".json"), 
      track = c("Andrzej Duda", "AndrzejDuda", "AndrzejDuda2015", 
                "Bronisław Komorowski", "prezydentpl", "PBK", 
                "Prezydent Komorowski", "Magdalena Ogórek", "Magda Ogórek", 
                "ogorekmagda", "Adam Jarubas", "JarubasAdam", 
                "Janusz Palikot", "Palikot Janusz", "Palikot2015", 
                "Janusz Korwin-Mikke", "JkmMikke", "Duda", "Komorowski", 
                "Ogórek", "Ogorek", "Jarubas", "Palikot", "Korwin-Mikke", 
                "Korwin Mikke", "Dudy", "Dudzie", "Dudę", "Dude", "Dudą", 
                "Komorowskiego", "Komorowskiemu", "Komorowskiego", 
                "Komorowskim", "Jarubasa", "Jarubasowi", "Jarubasem", 
                "Jarubasie", "Palikota", "Palikotowi", "Palikotem", 
                "Palikocie", "wyboryprezydenckie", "prezydentKomorowski", 
                "Bronislaw Komorowski", "Korwin", "Korwina", "Korwinowi", 
                "Korwinem", "Korwinie", "Korwina-Mikke", "Korwinowi-Mikke", 
                "Korwinem-Mikke", "Korwinie-Mikke"), language = "pl", 
      timeout = czas, oauth = paczka)
   
   # przeksztalcamy plik w formacie .json na ramke danych
   parsuj <- parseTweets(stri_paste(katalog, "/", nazwa, ".json"), 
                         simplify = FALSE, verbose = TRUE)
   
   # niektore sposrod pobranych tweetow nie dotyczylo kandydatow na prezydenta, 
   # a np. pilkarzy; w tym celu postanowiono zlokalizowac podejrzane tweety, 
   # poprzez wpisanie pewnych slow kluczowych zwiazanych z polska liga pilki
   # noznej
   wykryjInne <- lapply(parsuj$text, function(x) 
      stri_detect_regex(x, 
                        c("Ondrej Duda", "Ondreja Dudy", "Ondrejowi Dudzie", 
                          "Ondreja Dudę", "Ondreja Dude", "Ondrejem Dudą", 
                          "Ondrejem Duda", "Ondreju Dudzie", 
                          "Marcin Komorowski", "Marcina Komorowskiego", 
                          "Marcinowi Komorowskiemu", "Marcinem Komorowskim", 
                          "Marcinie Komorowskim", "LegiaWarszawa", "Legia", 
                          "Legii", "Legię", "Legie", "Legią", 
                          "Legia Warszawa", "mecz", "meczu", "meczowi", 
                          "meczem", "obrona", "obrony", "obronie", "obronę", 
                          "obrone", "obroną", "KoltonRoman", "piłka", 
                          "pilka", "piłki", "pilki", "piłce", "pilce", 
                          "piłkę", "pilke", "piłką", "Ekstraklasa", 
                          "Ekstraklasy", "Ekstraklasie", "Ekstraklasę", 
                          "Ekstraklase", "Ekstraklasą", "bramka", "bramki", 
                          "bramce", "bramkę", "bramką", "gol", "gola", 
                          "golem", "golu", "Żyro", "uzupełnienia", 
                          "napastnik", "napastnika", "napastnikowi", 
                          "napastnikiem", "napastniku")))
   
   # ustalamy indeksy tych tweetow, ktore z duzym prawdopobienstwem dotycza
   # wyborow prezydenckich
   prawidlowe <- unlist(lapply(wykryjInne, function(y) all(y == FALSE)))
   tweetyWybory <- parsuj$text[prawidlowe == TRUE]
   ktore <- which(parsuj$text %in% tweetyWybory)
   
   # zmieniamy format daty na bardziej przyjazny
   moje_locale <- Sys.setlocale("LC_TIME")
   Sys.setlocale("LC_TIME", "English")
   zmiana_daty <- strptime(parsuj$created_at, "%a %b %d %H:%M:%S %z %Y")
   Sys.setlocale("LC_TIME", moje_locale)
   
   # tworzymy ramke danych zawierajaca tresci tweetow, jak rowniez podstawowe
   # informacje na temat autora, czasu powstania wiadomosci, lokalizacji oraz 
   # liczby uzytkownikow obserwujacych danego autora
   data.frame(tekst = tweetyWybory, 
              autor = parsuj$screen_name[ktore], 
              data = zmiana_daty[ktore], 
              lokalizacja = parsuj$location[ktore], 
              obserwujacy = parsuj$followers_count[ktore], 
              stringsAsFactors = FALSE)
}

czas <- 3*60
#katalog <- "C:/Dane/Pawel_2/PW/R_Big_Data/tweety"
katalog <- "C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\tweety"
nazwa <- "17_03_proba1"
pobieranieTweetow(czas, katalog, nazwa)

