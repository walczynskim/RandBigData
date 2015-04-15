## Ponizsza funkcja sluzy analizie wydzwieku filtrowanych na biezaco tweetow, 
## zawierajacych slowa kluczowe zwiazane z nazwiskami glownych kandydatow na 
## urzad Prezydenta RP: Andrzeja Dudy, Bronislawa Komorowskiego, Magdaleny 
## Ogorek, Adama Jarubasa, Janusza Palikota, Janusza Korwin-Mikke oraz Pawła 
## Kukiza.

## Argumenty funkcji:
## 
## czas - okres (liczony w sekundach), w jakim dokonujemy nasluchu tweetow
## katalog - sciezka do folderu, w ktorym przechowujemy plik z tweetami
## nazwa - nazwa pliku, w ktorym trzymac bedziemy interesujace nas tweety

## Wartosc zwracana: wektor zawierajacy srednia wartosc wydzwieku dla 
##                   poszczegolnych kandydatow


wydzwiekTweetow <- function(czas, katalog, nazwa){
   
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
                "Korwinem-Mikke", "Korwinie-Mikke", "Paweł Kukiz", 
                "PrezydentKukiz", "Kukiz", "Kukiza", "Kukizowi", "Kukizem", 
                "Kukizie"), 
      language = "pl", 
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
                          "meczem", "KoltonRoman", "piłka", "pilka", 
                          "piłki", "pilki", "piłce", "pilce", "piłkę", 
                          "pilke", "piłką", "Ekstraklasa", "Ekstraklasy", 
                          "Ekstraklasie", "Ekstraklasę", "Ekstraklase", 
                          "Ekstraklasą", "bramka", "bramki", "bramce", 
                          "bramkę", "bramką", "gol", "gola", "golem", 
                          "golu", "Żyro", "Zyro", "uzupełnienia", 
                          "napastnik", "napastnika", "napastnikowi", 
                          "napastnikiem", "napastniku", "Dossa Junior", 
                          "Kuciak", "Kuciaka", "Kuciakowi", "Kuciakiem", 
                          "Kuciaku", "Jałocha", "Jalocha", "Jałochy", 
                          "Jalochy", "Jałosze", "Jalosze", "Jałochę", 
                          "Jaloche", "Jałochą", "Jalocha", "Lewczuk", 
                          "Lewczuka", "Lewczukowi", "Lewczukiem", 
                          "Lewczuku", "Wieteska", "Wieteski", "Wietesce", 
                          "Wieteskę", "Wieteske", "Wieteską", "Astiz", 
                          "Astiza", "Astizowi", "Astizem", "Astizie", 
                          "Brzyski", "Brzyskiego", "Brzyskiemu", "Brzyskim", 
                          "Broź", "Brozia", "Broziowi", "Broziem", "Broziu", 
                          "Marques", "Marquesa", "Marquesowi", "Marquesem", 
                          "Marquesie", "Szwoch", "Szwocha", "Szwochowi", 
                          "Szwochem", "Szwochu", "Vrdoljak", "Vrdoljaka", 
                          "Vrdoljakowi", "Vrdoljakiem", "Vrdoljaku", 
                          "Pinto", "Bartczak", "Bartczaka", "Bartczakowi", 
                          "Bartczakiem", "Bartczaku", "Saganowski", 
                          "Saganowskiego", "Saganowskiemu", "Saganowskim", 
                          "Ryczkowski", "Ryczkowskiego", "Ryczkowskiemu", 
                          "Ryczkowskim", "Orlando Sa", "Henning Berg", 
                          "Jodłowiec", "Jodlowiec", "Jodłowca", "Jodlowca", 
                          "Jodłowcu", "Jodlowcu", "Jodłowcowi", 
                          "Jodlowcowi", "Jodłowcem", "Jodlowcem")))
   
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
   ramka <- data.frame(tekst = tweetyWybory, 
                       autor = parsuj$screen_name[ktore], 
                       data = zmiana_daty[ktore], 
                       lokalizacja = parsuj$location[ktore], 
                       obserwujacy = parsuj$followers_count[ktore], 
                       stringsAsFactors = FALSE)

   slownik <- read.table(
      "C:/Dane/Pawel_2/PW/R_Big_Data/Projekt1/slownikWydzwieku01.csv", 
      fileEncoding = "utf-8")

   teksty <- ramka$tekst
   male <- stri_trans_tolower(teksty)
   bez_n <- stri_replace_all_regex(male, "\n", " ") 
   nie_adresy <- stri_replace_all_regex(bez_n, 
                                        "http(s?)\\S* | http(s?)\\S*", "")
   bez_rt <- stri_replace_all_regex(nie_adresy, "rt ", "")
   slowa <- stri_extract_words(bez_rt)
   
   duda <- c("andrzej duda", "andrzejduda", "andrzejduda2015", "duda", "dudy", 
             "dudzie", "dudę", "dude", "dudą")
   
   komorowski <- c("bronisław komorowski", "prezydentpl", "pbk", 
                   "prezydent komorowski", "komorowski", "komorowskiego", 
                   "komorowskiemu", "komorowskiego", "komorowskim", 
                   "prezydentkomorowski", "bronislaw komorowski")
   
   ogorek <- c("magdalena ogórek", "magda ogórek", "ogorekmagda", "ogórek", 
               "ogorek")
   
   jarubas <- c("adam jarubas", "jarubasadam", "jarubas", "jarubasa", 
                "jarubasowi", "jarubasem", "jarubasie")
   
   palikot <- c("janusz palikot", "palikot janusz", "palikot2015", "palikot", 
                "palikota", "palikotowi", "palikotem", "palikocie")
   
   jkm <- c("janusz korwin-mikke", "jkmmikke", "korwin-mikke", "korwin mikke", 
            "korwin", "korwina", "korwinowi", "korwinem", "korwinie", 
            "korwina-mikke", "korwinowi-mikke", "korwinem-mikke", 
            "korwinie-mikke")
   
   kukiz <- c("paweł kukiz", "prezydentkukiz", "kukiz", "kukiza", "kukizowi", 
              "kukizem", "kukizie")
   
   kandydaci <- list(duda, komorowski, ogorek, jarubas, palikot, jkm, kukiz)
   
   sredni_wydzwiek <- numeric(7)
   
   #analize sentymentu danego kandydata przeprowadzamy na tweetach, 
   #w ktorych nazwisko kandydata pojawilo sie przynajmniej jeden raz
   for(i in seq_along(kandydaci)){
      wykryj <- lapply(bez_rt, function(x) stri_detect_regex(x, 
                                                             kandydaci[[i]]))
      prawda <- sapply(wykryj, function(y) any(y == TRUE))
      jest <- bez_rt[prawda == TRUE]
      
      slowa <- stri_extract_words(jest)
      n <- length(slowa)
      wydzwieki <- numeric(n)
      
      for(j in seq_along(slowa)){
         m <- length(slowa[[j]])
         wydzwiek <- numeric(m)
         for(k in seq_along(slowa[[j]])){
            indeks <- which(slownik[, 1] == slowa[[j]][k])
            if(length(indeks) > 0){
               wydzwiek[k] <- slownik[indeks, 5]
            } else {
               wydzwiek[k] <- 0
            }
         }
         wydzwieki[j] <- sum(wydzwiek)
      }
      sredni_wydzwiek[i] <- mean(wydzwieki)
   }
   
   list(Sys.time(), structure(sredni_wydzwiek, 
                              names = c("duda", "komorowski", "ogorek", 
                                        "jarubas", "palikot", "jkm", 
                                        "kukiz")))
}

czas <- 10
katalog <- "C:/Dane/Pawel_2/PW/R_Big_Data/Projekt1/tweety"
nazwa <- "wydzwiek_08_04"
wydzwiekTweetow(czas, katalog, nazwa)

