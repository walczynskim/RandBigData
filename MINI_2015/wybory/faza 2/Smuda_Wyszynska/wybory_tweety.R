library("streamR")
library("ROAuth")

##KRÓTKA INSTRUKCJA CO ZROBIĆ PRZED ODPALENIEM SKRYPTU

#1. Na początku trzeba stworzyć aplikację API na twitterze
#2. Potem tworzy się jakąś zmienną środowiskową np. paczka
#  przy pomocy funkcji kodu

#requestURL<-"https://api.twitter.com/oauth/request_token"
#accessURL<-"https://api.twitter.com/oauth/access_token"
#authURL<-"https://api.twitter.com/oauth/authorize"
#consumerKey<-"XXXXXXXXXXXXXXXXXXXXXXXXXX"
#consumerSecret<-"XXXXXXXXXXXXXXXXXXXXXXXXXX"
#oauthKey<-"XXXXXXXXXXXXXXXXXXXXXXXXXX"
#oauthSecret<-"XXXXXXXXXXXXXXXXXXXXXXXXXX"

#tw_oauth<-OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,
#      oauthKey=oauthKey,oauthSecret=oauthSecret,requestURL=requestURL,
#      accessURL=accessURL,authURL=authURL)

#3. Autoryzujemy manualnie poprzez:
# tw_oauth$handshake(cainfo=system.file("CurlSSL","cacert.pem",package="RCurl"))

#4. Następnie zapisuje się zmienną do pliku przy pomocy funkcji save(tw_oauth,file)
#5. Teraz można wczytać plik

load("D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/tw_oauth")

# By włączyć skrypt wciskamy CTRL+SHIFT+S

#ścieżka dla folderu z projektem - trzeba zmienić tylko tu ścieżkę
sciezka<-"D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/"

#wczytujemy jeszcze wczesniej dane nam potrzebne
slownik_tw<-readLines(paste0(sciezka,"slownik_tw.txt"))

#ścieżka dla folderu gdzie będą zapisywane dane
sciezka<-paste0(sciezka,"Twitter/")
dir.create(sciezka,showWarnings=FALSE)
sciezka<-paste0(sciezka,"tweety/")
dir.create(sciezka,showWarnings=FALSE)

#ustalamy folder working directory
setwd(sciezka)

#zapisujemy do pliku tweety
filterStream(file=paste0("tweety-",Sys.Date(),".json"),
   track=slownik_tw,
   timeout=2*60*60,oauth=tw_oauth,language="pl")

#parsedTweets<-parseTweets(paste0("tweety-",Sys.Date(),".json"),simplify=FALSE,verbose=TRUE)
#parsedTweets[,"text"]
