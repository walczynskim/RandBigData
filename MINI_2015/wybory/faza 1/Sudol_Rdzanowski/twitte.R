################################## twitter #########################
library(twitteR)
library(ROAuth)
library(streamR)
library(stringi)

slownik=readLines('D://szkola//R i Big Data//projekt 1//slownik.txt')
l=length(slownik)

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "XXXXXXXXXXXXXXXXXXXXX"
consumerSecret <- "XXXXXXXXXXXXXXXXXX"
oauthKey <- "XXXXXXXXXXXXXXXxx"
oauthSecret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXX"
paczka <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                           oauthKey = oauthKey, oauthSecret = oauthSecret,
                           requestURL = requestURL, accessURL = accessURL, authURL = authURL)

paczka$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
access_token="XXXXXXXXXXXXXXXXXX"
access_secret="XXXXXXXXXXXXXXXXX"
setup_twitter_oauth(consumerKey, consumerSecret, access_token, access_secret)
data=as.character(Sys.Date())
data_wcz=as.character(Sys.Date()-1)

# Komorowski
tweets <- searchTwitter("komorowski", n=700, since=data_wcz,until=data)
dftweets <- twListToDF(tweets)
godz=as.character(as.numeric(Sys.time()))
nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//twitter//komorowski//',data,"//",godz,'.txt')
writeLines(dftweets$text,nazwa)
#Duda
tweets <- searchTwitter("Andrzej Duda",n=700, since=data_wcz,until=data)
dftweets <- twListToDF(tweets)
godz=as.character(as.numeric(Sys.time()))
nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//twitter//duda//',data,"//",godz,'.txt')
writeLines(dftweets$text,nazwa)
# Ogorek
tweets <- searchTwitter("Ogorek",n=500, since=data_wcz,until=data)
dftweets <- twListToDF(tweets)
godz=as.character(as.numeric(Sys.time()))
nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//twitter//ogorek//',data,"//",godz,'.txt')
writeLines(dftweets$text,nazwa)
#Jarubas
tweets <- searchTwitter("Jarubas", n=500,since=data_wcz,until=data)
dftweets <- twListToDF(tweets)
godz=as.character(as.numeric(Sys.time()))
nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//twitter//jarubas//',data,"//",godz,'.txt')
writeLines(dftweets$text,nazwa)
#Kukiz
tweets <- searchTwitter("Kukiz", n=500,since=data_wcz,until=data)
dftweets <- twListToDF(tweets)
godz=as.character(as.numeric(Sys.time()))
nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//twitter//kukiz//',data,"//",godz,'.txt')
writeLines(dftweets$text,nazwa)


#ogolne nasluchiwanie
godz=as.character(as.numeric(Sys.time()))
nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//twitter//',data,'//',godz)
filterStream( file=nazwa, 
              track=slownik, 
              timeout=90*60, oauth=paczka,language='pl' )



