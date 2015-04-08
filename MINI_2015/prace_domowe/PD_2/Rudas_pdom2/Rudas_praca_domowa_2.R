library(streamR)
library(ROAuth)
library(stringi)
#autoryzacja aplikacji
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "xoB0d3T5U3scq9YctZmc6UnZO"
consumerSecret <- "VbsCSoGAzZqlUeVLYV4Yda0LhHRT6V9F6nfmvjYIDENS1PBNVJ"
oauthKey <- "3093231833-8STP9Uf7DKGH0CwS29uDsXZJY4wovx9DKwoOADH"
oauthSecret <- "W1eN22KdXLaWLsI9tQMN0shiwuo42yDJDs2FA2tq2tX5l"

# proces autoryzacji jest trzykrokowy
paczka <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                           oauthKey = oauthKey, oauthSecret = oauthSecret,
                           requestURL = requestURL, accessURL = accessURL, authURL = authURL)

paczka$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

# zapiszemy dane tymczasowe do katalogu tmp
setwd("C:\\Users\\Krzysztof\\Documents\\Rudas_pdom2")

filterStream( file="muzyka.json", 
              track=c("Elvis Presley","Beatles","Alphaville", "Modern Talking","Metallica","Michael Jackson","Freddie Mercury"), 
              timeout=30*60, oauth=paczka, 
              language="en")
#czyszczenie danych
parsedTwees <- parseTweets("C:\\Users\\Krzysztof\\Documents\\Rudas_pdom2\\muzyka.json", simplify = FALSE, verbose = TRUE)
n<-nrow(parsedTwees)
parsedTwees1 <- stri_replace_all_regex(parsedTwees[,1],"\n"," ")
parsedTwees1<- stri_trans_tolower(parsedTwees1)
# wkres w ilu tweetach wystepuje dany zespol
Presley<-sum(stri_detect_regex(parsedTwees1,"elvis presley"))
Beatles<-sum(stri_detect_regex(parsedTwees1,"beatles"))
Alphaville<-sum(stri_detect_regex(parsedTwees1,"alphaville"))
Modern_talking<-sum(stri_detect_regex(parsedTwees1,"modern talking"))
Metallica<-sum(stri_detect_regex(parsedTwees1,"metallica"))
Jackson<-sum(stri_detect_regex(parsedTwees1,"michael jackson"))
Mercury<-sum(stri_detect_regex(parsedTwees1,"freddie mercury"))
barplot(c(Presley,Beatles,Alphaville,Modern_talking,Metallica,Jackson,Mercury),
        names.arg=c("Pr","Be","Al","Mo","Me","Ja","Fr"))
# wykres ile razy pojawiala sie nazwa zespolu we wszystkich tweetach
Presley1<-length(na.omit(unlist(stri_extract_all_regex(parsedTwees1,"elvis presley"))))
Beatles1<-length(na.omit(unlist(stri_extract_all_regex(parsedTwees1,"beatles"))))
Alphaville1<-length(na.omit(unlist(stri_extract_all_regex(parsedTwees1,"alphaville"))))
Modern_talking1<-length(na.omit(unlist(stri_extract_all_regex(parsedTwees1,"modern talking"))))
Metallica1<-length(na.omit(unlist(stri_extract_all_regex(parsedTwees1,"metallica"))))
Jackson1<-length(na.omit(unlist(stri_extract_all_regex(parsedTwees1,"michael jackson"))))
Mercury1<-length(na.omit(unlist(stri_extract_all_regex(parsedTwees1,"freddie mercury"))))
barplot(c(Presley1,Beatles1,Alphaville1,Modern_talking1,Metallica1,Jackson1,Mercury1),
        names.arg=c("Pr","Be","Al","Mo","Me","Ja","Fr"))

# Wykresy sa do siebie podobne

# rozwazamy pewne pojecia charakterystyczne dla danych zespolow/wykonawcow.
# Chcemy stwierdzic ile procent tweetow dotyczacych tych pojec stanowia tweety, w ktorych
# byli wspominani przypisani do tych pojec wykonawcy/zespoly

Presley2<-0
Beatles2<-0
Alphaville2<-0
Modern_talking2<-0
Metallica2<-0
Jackson2<-0
Mercury2<-0
Pr<-which(stri_detect_regex(parsedTwees1,"elvis presley"))
Be<-which(stri_detect_regex(parsedTwees1,"beatles"))
Al<-which(stri_detect_regex(parsedTwees1,"alphaville"))
M_t<-which(stri_detect_regex(parsedTwees1,"modern talking"))
Me<-which(stri_detect_regex(parsedTwees1,"metallica"))
Ja<-which(stri_detect_regex(parsedTwees1,"michael jackson"))
Fr<-which(stri_detect_regex(parsedTwees1,"freddie mercury"))
JL<-which(stri_detect_regex(parsedTwees1,"john lennon")==TRUE)

for(i in 1:length(JL))
{
   if(any(JL[i]==Pr))
   {
      Presley2=Presley2+1
   }
   if(any(JL[i]==Be))
   {
      Beatles2=Beatles2+1
   }
   if(any(JL[i]==Al))
   {
      Alphaville2=Alphaville2+1
   }
   if(any(JL[i]==M_t))
   {
      Modern_talking2=Modern_talking2+1
   }
   if(any(JL[i]==Me))
   {
      Metallica2=Metallica2+1
   }
   if(any(JL[i]==Ja))
   {
      Jackson2=Jackson2+1
   }
   if(any(JL[i]==Fr))
   {
      Mercury2=Mercury2+1
   }
   
}
(Wektorprocentowlennon<-c(Presley2,Beatles2,Alphaville2,Modern_talking2,Metallica2,Jackson2,
                         Mercury2)/length(JL))
#rzeczywiscie John Lennon byl nierozerwalnie zwiazany z Beatelsami, stad az 94.7% tweetow dotyczacych
#Lennona dotyczy tez Beatelsow

QU<-which(stri_detect_regex(parsedTwees1,"queen")==TRUE)
          Presley3<-0
          Beatles3<-0
          Alphaville3<-0
          Modern_talking3<-0
          Metallica3<-0
          Jackson3<-0
          Mercury3<-0
          
          for(i in 1:length(QU))
          {
             if(any(QU[i]==Pr))
             {
                Presley3=Presley3+1
             }
             if(any(QU[i]==Be))
             {
                Beatles3=Beatles3+1
             }
             if(any(QU[i]==Al))
             {
                Alphaville3=Alphaville3+1
             }
             if(any(QU[i]==M_t))
             {
                Modern_talking3=Modern_talking3+1
             }
             if(any(QU[i]==Me))
             {
                Metallica3=Metallica3+1
             }
             if(any(QU[i]==Ja))
             {
                Jackson3=Jackson3+1
             }
             if(any(QU[i]==Fr))
             {
                Mercury3=Mercury3+1
             }
             
          }
          (Wektorprocentowqueen<-c(Presley3,Beatles3,Alphaville3,Modern_talking3,Metallica3,Jackson3,
                                   Mercury3)/length(QU))
          #Wynik pozornie zaskakujacy. Queen to zespó³ Freddiego Mercurego, a twitty dotyczace Queen i Freddiego
          #stanowia niewielki procent wszystkich tweetow zawierajacych Queen. Jednakze slowo queen moglo sie pojawiac
          # w twittach w innym sensie (np. krolowa brytyjska), a takze zespol ten mogl byc wymieniany wsrod innych zespolow
          # (np. uzytkownik w tweecie wymienia zespoly ktore lubi).
          # Gdybysmy to w tracku do tweetow dali slowo queen, a teraz szukali Freddiego to pocent moglby byc nieco wiekszy.
PP<-which(stri_detect_regex(parsedTwees1,"pop"))
Presley4<-0
Beatles4<-0
Alphaville4<-0
Modern_talking4<-0
Metallica4<-0
Jackson4<-0
Mercury4<-0

for(i in 1:length(PP))
{
   if(any(PP[i]==Pr))
   {
      Presley4=Presley4+1
   }
   if(any(PP[i]==Be))
   {
      Beatles4=Beatles4+1
   }
   if(any(PP[i]==Al))
   {
      Alphaville4=Alphaville4+1
   }
   if(any(PP[i]==M_t))
   {
      Modern_talking4=Modern_talking4+1
   }
   if(any(PP[i]==Me))
   {
      Metallica4=Metallica4+1
   }
   if(any(PP[i]==Ja))
   {
      Jackson4=Jackson4+1
   }
   if(any(PP[i]==Fr))
   {
      Mercury4=Mercury4+1
   }
   
}

(Wektorprocentowpop<-c(Presley4,Beatles4,Alphaville4,Modern_talking4,Metallica4,Jackson4,
                        Mercury4)/length(PP))
# Prawie 65% wszystkich tweetow zawierajacych slowo pop dotyczy takze Michaela Jacksona, krola popu.
# Zastanawiajacy duzy procentowo udzial Beatelsow (prawie 30%)