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

#paczka<-OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,
#      oauthKey=oauthKey,oauthSecret=oauthSecret,requestURL=requestURL,
#      accessURL=accessURL,authURL=authURL)

#3. Następnie zapisuje się ją do pliku przy pomocy funkcji save(fb_oauth,file)
#4. Teraz można wczytać plik

load("D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/tw_oauth")

#5. Następnie pora na autoryzację manualną

paczka$handshake(cainfo=system.file("CurlSSL","cacert.pem",package="RCurl"))

# By włączyć skrypt wciskamy CTRL+SHIFT+S

#ścieżka dla folderu gdzie będą zapisywane dane
sciezka<-"D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/Twitter/"
dir.create(sciezka,showWarnings=FALSE)
sciezka<-paste0(sciezka,"tweety/")
dir.create(sciezka,showWarnings=FALSE)

#ustalamy folder working directory
setwd(sciezka)

#zapisujemy do pliku tweety
filterStream(file=paste0("tweety-",Sys.Date(),".json"),
   track=c("Braun","Duda","Dudy","Dudę","Dude","Dudą","Dudzie","Grodzka","Grodzką","Grodzkiej",
      "Jankowski","Jarubas","Jędrzejewski","Jedrzejewski","Korab-Karpowicz","Komorowski","Korwin","JKM",
      "Kowalski","Kukiz","Łaska","Laska","Majdański","Majdanski","Marzec","Morawiecki","Nowak","Nowicka",
      "Nowickiej","Nowicką","Ogórek","Ogorek","Palikot","Piątek","Piatek","Słomka","Slomka","Tanajno",
      "Tanajnym","Tanajnego","Tanajnemu","Wilk","Zydorczak","wybory","prezydent","kandydat"),
   timeout=2*60*60,oauth=paczka,language="pl")

#parsedTweets<-parseTweets(paste0("tweety-",Sys.Date(),".json"),simplify=FALSE,verbose=TRUE)
#parsedTweets[,"text"]
