#############################################################################
###############   WSKAŹNIK WIDOCZNOŚCI KANDYDATÓW - TWITTER   ###############
#############################################################################

#Pilk zawiera definicje funkcji (oraz funkcji pomocniczYCH), które zliczają
#wskaźniki widoczności kandydatów na portalu społecznościowym Twitter
#odnoszące się do tweetów użytkowników
#
#Sposób obliczania wskaźnika liczby wystąpień kandydata w tweetach na Twitterze:
#Dla danego okresu dla każdego tweeta zliczam, jaki kandydat w nim się pojawił,
#sklejam wszystko do ramki danych, a następnie sumuję po kolumnach (kandydaci)
#liczbę wystąpień
#
#Sposób obliczania wskaźnika wydźwięku tweetów:
#
#
#
#Możliwe okresy: tydzień i miesiąc
#
#Możliwe wzory odpowiednio do okresów: "2015-(X|XX)", "2015-XX"

library("stringi")
library("streamR")


#Ścieżka potrzebna jedynie do przykładów, które są pod funkcjami
#sciezka<-"D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/"

#wczytujemy imiona kandydatów i tokeny
kandydaci<-readLines(paste0(sciezka,"slownik_fb_kandydaci.txt"))
tokeny<-readLines(paste0(sciezka,"slownik_google_tokeny.txt"))

#Funkcja wczytująca plik .json do wektora napisów
wsk_tw_wczytaj_json<-function(plik,antyslownik){
   #zamieniamy plik .json na ramkę danych
   sparsowane<-parseTweets(plik,simplify=FALSE,verbose=FALSE)

   #wybieram właściwe tweety (z pominięciem słów z antysłownika, który zawiera
   #słowa kluczowe związane z polską ligą piłki nożnej
   wlasciwe<-lapply(sparsowane$text,function(tweet){
         !stri_detect_regex(tweet,antyslownik)
      })
   wlasciwe<-sapply(wlasciwe, function(wektor){
         all(wektor==TRUE)
      })
   wlasciwe<-sparsowane$text[wlasciwe]
   #zwracam wektor z tweetami
   return(wlasciwe)
}

#Funkcja pomocnicza, która tworzy długi wektor napisów dla tweetów z określonego
#okresu
wsk_tw_pomocnicza<-function(pliki,okres,wzor,antyslownik){
   if(okres=="dzien"){
      #wybieramy pasujący do wzoru plik i go wczytujemy
      ktory<-which(stri_detect_regex(pliki,wzor))
      if(length(ktory)>0) {
         tweety<-wsk_tw_wczytaj_json(pliki[ktory],antyslownik)
      }
      else {
         tweety<-character()
      }
      return(tweety)
   }
   if(okres=="tydzien"){
      #zapamietujemy interesujący nas numer tygodnia
      nr_tyg<-unlist(stri_extract_all_regex(wzor,"(?<=-).+$"))
      #wyciągamy z plików informację, który tydzień opisują
      daty<-unlist(stri_extract_all_regex(pliki,"(?<=tweety-).+(?=.json)"))
      daty<-as.POSIXlt(daty)
      tygodnie<-ceiling((daty$yday+4)/7)
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore<-which(tygodnie==nr_tyg)
      if(length(ktore)>0) { #gdy były pasujące, a jak nie to zwracamy character()
         tweety<-unlist(lapply(pliki[ktore],wsk_tw_wczytaj_json,antyslownik))
      }
      else {
         tweety<-character()
      }
      return(tweety)
   }
   else if(okres=="miesiac"){
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore<-which(stri_detect_regex(pliki,wzor))
      if(length(ktore)>0) { #gdy były pasujące, a jak nie to zwracamy character()
         tweety<-unlist(lapply(pliki[ktore],wsk_tw_wczytaj_json,antyslownik))
      }
      else {
         tweety<-character()
      }
      return(tweety)
   }
}

# # przykłady
#
# #wczytujemy antysłownik dla tweetów
# antyslownik<-readLines(paste0(sciezka,"slownik_tw_anty.txt"))
#
# #zmieniamy scieżkę i sprawdzamy jakie mamy tam pliki
# sciezka_tweety<-paste0(sciezka,"Twitter/tweety/")
# pliki_tweety<-list.files(sciezka_tweety,full.names=TRUE)
#
# wzor<-"2015-04-06"
# okres<-"dzien"
# a<-wsk_tw_pomocnicza(pliki_tweety,okres,wzor,antyslownik)
#
# wzor<-"2015-14"
# okres<-"tydzien"
# b<-wsk_tw_pomocnicza(pliki_tweety,okres,wzor,antyslownik)
#
# wzor<-"2015-03"
# okres<-"miesiac"
# c<-wsk_tw_pomocnicza(pliki_tweety,okres,wzor,antyslownik)

#Wskaźnik dla liczby wystąpień kandydata w tweetach na Twitterze
wsk_tw_liczba_wystapien<-function(okres,wzor){

   #wczytujemy antysłownik dla tweetów
   antyslownik<-readLines(paste0(sciezka,"slownik_tw_anty.txt"))

   #zmieniamy scieżkę i sprawdzamy jakie mamy tam pliki
   sciezka_tweety<-paste0(sciezka,"Twitter/tweety/")
   pliki_tweety<-list.files(sciezka_tweety,full.names=TRUE)

   #wczytujemy wszystkie tweety z danego okresu do pliku
   tweety<-wsk_tw_pomocnicza(pliki_tweety,okres,wzor,antyslownik)

   #sprawdzamy po tokenach, ile razy kandydaci pojawili się w tweetach
   if(length(tweety)>0) { #gdy mamy jakieś tweety
      wskaznik<-lapply(tweety,stri_count_regex,tokeny)
      wskaznik<-do.call(rbind,wskaznik)
      wskaznik<-apply(wskaznik,2,sum)
   }
   else { #gdy nie mamy pasujących plików
         wskaznik<-numeric(length(kandydaci))
   }
   names(wskaznik)<-kandydaci
   return(wskaznik)
}

# przykłady
#
# wzor<-"2015-14"
# okres<-"tydzien"
# ws<-wsk_tw_liczba_wystapien(okres,wzor)
#
# wzor<-"2015-03"
# okres<-"miesiac"
# ws2<-wsk_tw_liczba_wystapien(okres,wzor)



WskaznikAnalizaTweety <- function(okres, wzor){
    slownik <- read.table(paste0(sciezka,"slownikWydzwieku.csv"))
    slownik <- slownik[ ,c(1,5)]
    slownik[ ,1] <- stri_encode(slownik[ ,1], from="UTF8", to="cp1250")
    slownik[ ,2] <- slownik[ ,2] + 3

    #wczytujemy antysłownik dla tweetów
    antyslownik<-readLines(paste0(sciezka,"slownik_tw_anty.txt"))

    sciezka_tweety<-paste0(sciezka,"Twitter/tweety/")
    pliki_tweety<-list.files(sciezka_tweety,full.names=TRUE)

    tweety <- wsk_tw_pomocnicza(pliki_tweety, okres, wzor, antyslownik )
    wskaznik <- numeric(length(tokeny))

    #Dla kazdego kandydata
    for(i in seq_along(tokeny)){
      #Wyjmujemy zdania z jego nazwiskiem
      wykryj <- stri_detect(tweety, regex=tokeny[i])
      if (length(which(wykryj)) == 0){wskaznik[i] = 0;next}
      analiza <- paste(tweety[which(wykryj)], collapse=" ")
      #Zliczamy występujące w nich słowa i robimy ich analizę
      analiza <- stri_extract_all_words(analiza) %>% unlist() %>% table()
      wspolne <- intersect(slownik[ ,1], names(analiza))
      if(length(wspolne)==0){wskaznik[i] = 3;next}

      #Obliczamy wskaźnik
      wskaznik[i] <- sum(analiza[wspolne] * slownik[slownik[ ,1] %in% wspolne,
                                                    2]) / sum(analiza[wspolne])
    }

    names(wskaznik) <- kandydaci
    wskaznik
  }

# #przykład
#  wzor<-"2015-14"
#  okres<-"tydzien"
# WskaznikAnalizaTweety(okres,wzor)
