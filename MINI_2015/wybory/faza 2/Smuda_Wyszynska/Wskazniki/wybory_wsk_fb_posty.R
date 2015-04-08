##############################################################################
############   WSKAŹNIK WIDOCZNOŚCI KANDYDATÓW - FACEBOOK POSTY   ############
##############################################################################

#Pilk zawiera definicje funkcji (oraz funkcji pomocniczej), które zliczają
#wskaźniki widoczności kandydatów na portalu społecznościowym Facebook
#odnoszące się do postów na ich fanpage'ach
#
#Sposób obliczania wskaźnika liczby postów na fanpage'u:
#Dla danego okresu zliczam liczbę postów opublikowanych przez kandydata
#(długość ramki, która jest zwrócona przez funkcję pomocniczą)
#
#Sposób obliczania wskaźnika „średniej ilości” polubień postów na fanpage'u:
#Dla danego okresu zliczam średnią arytmetyczną liczby polubień postów
#opublikowanych przez kandydata (pierwsza kolumna ramki, która jest zwrócona
#przez funkcję pomocniczą)
#
#Sposób obliczania wskaźnika „średniej ilości” komentarzy postów na fanpage'u:
#Dla danego okresu zliczam średnią arytmetyczną liczby komentarzy postów
#opublikowanych przez kandydata (druga kolumna ramki, która jest zwrócona
#przez funkcję pomocniczą)
#
#Sposób obliczania wskaźnika „średniej ilości” udostępnień postów na fanpage'u:
#Dla danego okresu zliczam średnią arytmetyczną liczby udostępnień postów
#opublikowanych przez kandydata (trzecia kolumna ramki, która jest zwrócona
#przez funkcję pomocniczą)
#
#Możliwe okresy: tydzień i miesiąc
#
#Możliwe wzory odpowiednio do okresów: "2015-(X|XX)", "2015-XX"

library("stringi")

#Ścieżka potrzebna jedynie do przykładów, które są pod funkcjami
#sciezka<-"D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/"

#Funkcja pomocnicza, która korzystając z listy plików, okresu i ze wzoru
#zwraca informacje (liczba polubień, komentarzy i udostępnień) o postach
#kandydata w jednej ramce danych z danego okresu
wsk_fb_posty_pomocnicza<-function(lista,okres,wzor){
   if(okres=="tydzien"){
      #zapamietujemy interesujący nas numer tygodnia
      nr_tyg<-unlist(stri_extract_all_regex(wzor,"(?<=-).+$"))
      #wyciągamy z plików informację, który tydzień opisują
      daty<-unlist(stri_extract_all_regex(lista,"(?<=posty-).+(?=.txt)"))
      daty<-as.POSIXlt(daty)
      tygodnie<-ceiling((daty$yday+4)/7)
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore<-which(tygodnie==nr_tyg)
      if(length(ktore)>0) { #gdy były pasujące, a jak nie to zwracamy numeric()
         posty<-lapply(lista[ktore],read.table)
         #sprawdzamy, w które dni były opublikowane posty
         niepuste<-sapply(posty,function(element){
            if(ncol(element)==1) {
               element<-FALSE
            }
            else {
               element<-TRUE
            }})
         #interesują nas pliki z postami
         posty<-posty[niepuste]
         #zapisujemy co całości
         ramka<-do.call(rbind,posty)
         #interesują nas tylko liczby polubień, komentarzy i udostępnień
         ramka<-ramka[,8:10]
      }
      else {
         ramka<-numeric()
      }
      return(ramka)
   }
   else if(okres=="miesiac"){
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore<-which(stri_detect_regex(lista,wzor))
      if(length(ktore)>0) { #gdy były pasujące, a jak nie to zwracamy numeric()
         posty<-lapply(lista[ktore],read.table)
         #sprawdzamy, w które dni były opublikowane posty
         niepuste<-sapply(posty,function(element){
            if(ncol(element)==1) {
               element<-FALSE
            }
            else {
               element<-TRUE
            }})
         #interesują nas pliki z postami
         posty<-posty[niepuste]
         #zapisujemy co całości
         ramka<-do.call(rbind,posty)
         #interesują nas tylko liczby polubień, komentarzy i udostępnień
         ramka<-ramka[,8:10]
      }
      else {
         ramka<-numeric()
      }
      return(ramka)
   }
}

#Wskaźnik dla liczby postów na stronach na facebooku
wsk_fb_liczba_postow<-function(okres,wzor){

   #wczytujemy imiona kandydatów
   kandydaci<-readLines(paste0(sciezka,"slownik_fb_kandydaci.txt"))

   #zmieniamy scieżkę
   sciezka_fb_posty<-paste0(sciezka,"Facebook/posty/")
   sciezki_fb_posty<-paste0(sciezka_fb_posty,kandydaci)
   pliki_fb_posty<-lapply(sciezki_fb_posty,list.files,full.names=TRUE)

   #wywołujemy funkcję pomocniczą
   posty_kandydaci<-lapply(pliki_fb_posty,wsk_fb_posty_pomocnicza,okres,wzor)

   #zliczamy liczby postów dla kandydatów
   wskaznik<-sapply(posty_kandydaci,function(ramka){
      if(!is.null(ramka)) { #jeśli były posty, to zliczamy, wpp. dajemy 0
         liczba_postow<-nrow(ramka)
      }
      else {
         liczba_postow<-0
      }
      })
   names(wskaznik)<-kandydaci
   return(wskaznik)
}

# przykłady
#
# wzor<-"2015-14"
# okres<-"tydzien"
#
# wsk_fb_liczba_postow(okres,wzor)
#
# wzor<-"2015-03"
# okres<-"miesiac"
#
# wsk_fb_liczba_postow(okres,wzor)

#Wskaźnik dla średniej liczby polubień do posta na stronach na facebooku (średnia arytmetyczna)
wsk_fb_liczba_like_post<-function(okres,wzor){

   #wczytujemy imiona kandydatów
   kandydaci<-readLines(paste0(sciezka,"slownik_fb_kandydaci.txt"))

   #zmieniamy scieżkę
   sciezka_fb_posty<-paste0(sciezka,"Facebook/posty/")
   sciezki_fb_posty<-paste0(sciezka_fb_posty,kandydaci)
   pliki_fb_posty<-lapply(sciezki_fb_posty,list.files,full.names=TRUE)

   #wywołujemy funkcję pomocniczą
   posty_kandydaci<-lapply(pliki_fb_posty,wsk_fb_posty_pomocnicza,okres,wzor)

   #zliczamy średnią zdobytych polubień do posta dla kandydatów
   wskaznik<-sapply(posty_kandydaci,function(ramka){
      if(!is.null(ramka)) { #jeśli były posty, to zliczamy, wpp. dajemy 0
         liczba_like_post<-mean(ramka[,1])
         liczba_like_post<-round(liczba_like_post,2)
      }
      else {
         liczba_like_post<-0
      }
      })
   names(wskaznik)<-kandydaci
   return(wskaznik)
}

# przykłady
#
# wzor<-"2015-14"
# okres<-"tydzien"
#
# wsk_fb_liczba_like_post(okres,wzor)
#
# wzor<-"2015-03"
# okres<-"miesiac"
#
# wsk_fb_liczba_like_post(okres,wzor)

#Wskaźnik dla średniej liczby komentarzy do posta na stronach na facebooku (średnia arytmetyczna)
wsk_fb_liczba_komentarz_post<-function(okres,wzor){

   #wczytujemy imiona kandydatów
   kandydaci<-readLines(paste0(sciezka,"slownik_fb_kandydaci.txt"))

   #zmieniamy scieżkę
   sciezka_fb_posty<-paste0(sciezka,"Facebook/posty/")
   sciezki_fb_posty<-paste0(sciezka_fb_posty,kandydaci)
   pliki_fb_posty<-lapply(sciezki_fb_posty,list.files,full.names=TRUE)

   #wywołujemy funkcję pomocniczą
   posty_kandydaci<-lapply(pliki_fb_posty,wsk_fb_posty_pomocnicza,okres,wzor)

   #zliczamy średnią zdobytych polubień do posta dla kandydatów
   wskaznik<-sapply(posty_kandydaci,function(ramka){
      if(!is.null(ramka)) { #jeśli były posty, to zliczamy, wpp. dajemy 0
         liczba_komentarz_post<-mean(ramka[,2])
         liczba_komentarz_post<-round(liczba_komentarz_post,2)
      }
      else {
         liczba_komentarz_post<-0
      }
      })
   names(wskaznik)<-kandydaci
   return(wskaznik)
}

# przykłady
#
# wzor<-"2015-14"
# okres<-"tydzien"
#
# wsk_fb_liczba_komentarz_post(okres,wzor)
#
# wzor<-"2015-03"
# okres<-"miesiac"
#
# wsk_fb_liczba_komentarz_post(okres,wzor)

#Wskaźnik dla średniej liczby udostępniej do posta na stronach na facebooku (średnia arytmetyczna)
wsk_fb_liczba_udostepnienie_post<-function(okres,wzor){

   #wczytujemy imiona kandydatów
   kandydaci<-readLines(paste0(sciezka,"slownik_fb_kandydaci.txt"))

   #zmieniamy scieżkę
   sciezka_fb_posty<-paste0(sciezka,"Facebook/posty/")
   sciezki_fb_posty<-paste0(sciezka_fb_posty,kandydaci)
   pliki_fb_posty<-lapply(sciezki_fb_posty,list.files,full.names=TRUE)

   #wywołujemy funkcję pomocniczą
   posty_kandydaci<-lapply(pliki_fb_posty,wsk_fb_posty_pomocnicza,okres,wzor)

   #zliczamy średnią zdobytych polubień do posta dla kandydatów
   wskaznik<-sapply(posty_kandydaci,function(ramka){
      if(!is.null(ramka)) { #jeśli były posty, to zliczamy, wpp. dajemy 0
         liczba_udostepnienie_post<-mean(ramka[,3])
         liczba_udostepnienie_post<-round(liczba_udostepnienie_post,2)
      }
      else {
         liczba_udostepnienie_post<-0
      }
      })
   names(wskaznik)<-kandydaci
   return(wskaznik)
}

# przykłady
#
# wzor<-"2015-14"
# okres<-"tydzien"
#
# wsk_fb_liczba_udostepnienie_post(okres,wzor)
#
# wzor<-"2015-03"
# okres<-"miesiac"
#
# wsk_fb_liczba_udostepnienie_post(okres,wzor)
