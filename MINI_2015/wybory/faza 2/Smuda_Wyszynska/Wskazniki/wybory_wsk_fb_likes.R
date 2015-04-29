###############################################################################
##########   WSKAŹNIK WIDOCZNOŚCI KANDYDATÓW - FACEBOOK POLUBIENIA   ##########
###############################################################################

#Pilk zawiera definicje funkcji, które zliczają wskaźniki widoczności kandydatów
#na portalu społecznościowym Facebook odnoszące się do polubień ich fanpage'y
#
#Sposób obliczania wskaźnika polubień na fanpage'u:
#Dla danego okresu wybieram maksimum z liczby polubień
#
#Sposób obliczania wskaźnika „średniego przyrostu” polubień na fanpage'u:
#Dla danego okresu zliczam zmiany liczby polubień fanpage'y, a następnie
#zwracam ich średnią arytmetyczną
#
#Możliwe okresy: dzień, tydzień i miesiąc
#
#Możliwe wzory odpowiednio do okresów: "2015-XX-XX", "2015-(X|XX)", "2015-XX"

library("stringi")

#Ścieżka potrzebna jedynie do przykładów, które są pod funkcjami
#sciezka<-"D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/"

#Wskaźnik dla polubień na fanpage'ach na facebooku
wsk_fb_likes<-function(okres,wzor){

   #wczytujemy imiona kandydatów
   kandydaci<-readLines(paste0(sciezka,"slownik_fb_kandydaci.txt"))

   #zmieniamy scieżkę i sprawdzamy jakie mamy tam pliki
   sciezka_fb_likes<-paste0(sciezka,"Facebook/likes/")
   pliki_fb_likes<-list.files(sciezka_fb_likes,full.names=TRUE)

   if(okres=="dzien"){
      #wybieramy pasujący do wzoru plik i go wczytujemy
      ktory<-which(stri_detect_regex(pliki_fb_likes,wzor))
      if(length(ktory)>0) {
         wskaznik<-read.csv2(pliki_fb_likes[ktory])
      }
      else { #gdy nie mamy pasujących plików
         wskaznik<-numeric(length(kandydaci))
      }
      names(wskaznik)<-kandydaci
      #chcemy wektor na wyjściu
      wskaznik<-unlist(wskaznik)
      return(wskaznik)
   }
   else if(okres=="tydzien"){
      #zapamietujemy interesujący nas numer tygodnia
      nr_tyg<-unlist(stri_extract_all_regex(wzor,"(?<=-).+$"))
      #wyciągamy z plików informację, który tydzień opisują
      daty<-unlist(stri_extract_all_regex(pliki_fb_likes,"(?<=likes-).+(?=.txt)"))
      daty<-as.POSIXlt(daty)
      tygodnie<-ceiling((daty$yday+4)/7)
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore<-which(tygodnie==nr_tyg)
      if(length(ktore)>0) {
         #interesuje nas maksymalny pomiar dla danego tygodnia
         wskaznik<-do.call(rbind,lapply(pliki_fb_likes[ktore],read.csv2))
         wskaznik<-apply(wskaznik,2,max)
      }
      else { #gdy nie mamy pasujących plików
         wskaznik<-numeric(length(kandydaci))
      }
      names(wskaznik)<-kandydaci
      return(wskaznik)
   }
   else if(okres=="miesiac"){
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore<-which(stri_detect_regex(pliki_fb_likes,wzor))
      if(length(ktore)>0) {
         #interesuje nas maksymalny pomiar dla danego miesiąca
         wskaznik<-do.call(rbind,lapply(pliki_fb_likes[ktore],read.csv2))
         wskaznik<-apply(wskaznik,2,max)
      }
      else { #gdy nie mamy pasujących plików
         wskaznik<-numeric(length(kandydaci))
      }
      names(wskaznik)<-kandydaci
      return(wskaznik)
   }
}

# przyklady
#
# a<-wsk_fb_likes("miesiac","2015-03")
# a2<-wsk_fb_likes("miesiac","2015-04")
#
# b<-wsk_fb_likes("tydzien","2015-13")
# b2<-wsk_fb_likes("tydzien","2015-14")
#
# c<-wsk_fb_likes("dzien","2015-04-02")
# c2<-wsk_fb_likes("dzien","2015-04-05")

#Wskaźnik dla średniego wzrostu polubień na fanpage'ach na facebooku
#(średnia arytmetyczna przyrostów)

wzor<-"2015-11"
okres<-"tydzien"

wsk_fb_wzrost_likes<-function(okres,wzor){

   #wczytujemy imiona kandydatów
   kandydaci<-readLines(paste0(sciezka,"slownik_fb_kandydaci.txt"))

   #zmieniamy scieżkę i sprawdzamy jakie mamy tam pliki
   sciezka_fb_likes<-paste0(sciezka,"Facebook/likes/")
   pliki_fb_likes<-list.files(sciezka_fb_likes,full.names=TRUE)

   if(okres=="tydzien"){
      #zapamietujemy interesujący nas numer tygodnia
      nr_tyg<-unlist(stri_extract_all_regex(wzor,"(?<=-).+$"))
      #wyciągamy z plików informację, który tydzień opisują
      daty<-unlist(stri_extract_all_regex(pliki_fb_likes,"(?<=likes-).+(?=.txt)"))
      daty<-as.POSIXlt(daty)
      tygodnie<-ceiling((daty$yday+4)/7)
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore<-which(tygodnie==nr_tyg)
      if(length(ktore)>0) {
         #interesuje nas średni przyrost w pomiarach dla danego tygodnia
         wskaznik<-do.call(rbind,lapply(pliki_fb_likes[ktore],read.csv2))
         wskaznik<-apply(wskaznik,2,diff)
         if(is.data.frame(wskaznik)){
            wskaznik<-apply(wskaznik,2,mean)
         }
         wskaznik<-round(wskaznik,2)
      }
      else { #gdy nie mamy pasujących plików
         wskaznik<-numeric(length(kandydaci))
      }
      names(wskaznik)<-kandydaci
      return(wskaznik)
   }
   else if(okres=="miesiac"){
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore<-which(stri_detect_regex(pliki_fb_likes,wzor))
      if(length(ktore)>0) {
         #interesuje nas średni przyrost w pomiarach dla danego miesiąca
         wskaznik<-do.call(rbind,lapply(pliki_fb_likes[ktore],read.csv2))
         wskaznik<-apply(wskaznik,2,diff)
         if(is.data.frame(wskaznik)){
            wskaznik<-apply(wskaznik,2,mean)
         }
         wskaznik<-round(wskaznik,2)
      }
      else { #gdy nie mamy pasujących plików
         wskaznik<-numeric(length(kandydaci))
      }
      names(wskaznik)<-kandydaci
      return(wskaznik)
   }
}

# przyklady
#
# a<-wsk_fb_wzrost_likes("miesiac","2015-03")
# a2<-wsk_fb_wzrost_likes("miesiac","2015-04")
#
# b<-wsk_fb_wzrost_likes("tydzien","2015-13")
# b2<-wsk_fb_wzrost_likes("tydzien","2015-14")
