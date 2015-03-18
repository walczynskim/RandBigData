# API dla strumienia, nasluchuje czy okreslone tweety sie pojawily
library(streamR)
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "QTyOuPErIAHzqnQ5Zk4vQlixT"
consumerSecret <- "c6jNRexixiNQVCG19vRqiURvZJFqK1PV1zZVMTbOdeD9jgsjwT"
oauthKey <- "rampampamrymcymcym"
oauthSecret <- "zpizdyogienzdupydym"

# proces autoryzacji jest trzykrokowy
paczka <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                           oauthKey = oauthKey, oauthSecret = oauthSecret,
                           requestURL = requestURL, accessURL = accessURL, authURL = authURL)

paczka$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

# zapiszemy dane tymczasowe do katalogu tmp
setwd("../")




# nasluch przez okolo 5 minut
# wszystkie z geotagiem LUB zawierajace slowa klucze
filterStream( file=paste0("R_project1_tweets-",Sys.Date(),".json"), 
              track=c("#Braun","Grzegorz Braun","Braun","Brauna",
                      "#Duda","#AndrzejDuda","Andzrej Duda", "Duda","Dudy","Dudę","Dude","Dudą","Dudzie",
                      "#Grodzka","Anna Grodzka","Grodzka","Grodzkiej","Grodzką", "ostry dewiant",
                      "#Jankowski","Zdzisław Jankowski","Jankowski",
                      "#Jarubas","Adam Jarubas","Jarubas",
                      "#Jędrzejewski","Józef Jędrzejewski","Jędrzejewski","Jedrzejewskiego","Jędrzejewskiemu",
                      "#KorabKarpowicz","Włodzimierz Korab-Karpowicz","Korab-Karpowicz",
                      "#Komorowski","Bronisław Komorowski","Komorowski","Komorowskiego","Komorowskiemu",
                      "#Korwin","#KornwinMikke","Janusz Korwin-Mikke", "#JKM","JKM","KORWIN","#KORWIN",
                      "Marian Kowalski","Ruch Narodowy",
                      "#Kukiz","Paweł Kukiz","Kukiz","Kukiza",
                      "Dariusz Łaska","Łaska",
                      "Majdański",
                      "Marzec",
                      "Morawiecki",
                      "Nowak",
                      "Nowicka","Wanda Nowicka",
                      "#Ogórek","Magda Ogórek","Ogorek",
                      "#Palikot", "Palikot", "Janusz Palikot",
                      "Piątek",
                      "Słomka",
                      "Tanajno",
                      "Jacek Wilk","Wilk",
                      "Zydorczak",
                      "prezydent", "prezydenta",
                      "wybory","wybory prezydenckie",
                      "kandydat","kandydata","kandydaci"
                      ), 
              timeout=4*3600, oauth=paczka, 
              language="pl",
              locations=c(-180,-90,180,90))
              #locations=c(14,49,25,54))


# parsedTwees <- parseTweets("C://Users//grabarze//Documents//komorowski.json", simplify = FALSE, verbose = TRUE)
# head(parsedTwees)[,1]
# sort(table(parsedTwees$country))
# 
# parsedTwees2 <- parseTweets("C://Users//grabarze//Documents//komorowski2.json", simplify = FALSE, verbose = TRUE)
# head(parsedTwees2)[,1]
# sort(table(parsedTwees2$country))
# 
# parsedTwees3 <- parseTweets("C://Users//grabarze//Documents//DUPA2.json", simplify = FALSE, verbose = TRUE)
# head(parsedTwees3)[,1]
# sort(table(parsedTwees3$country))
# 
# tweets_pol <- parsedTwees3[parsedTwees3$country%in%c('Polska'),1]
# 
# tweets_pol22 <- parsedTwees3[,1]
# 
# str(parsedTwees3)
