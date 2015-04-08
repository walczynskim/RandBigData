################################################################################
##############   WSKAŹNIK WIDOCZNOŚCI KANDYDATÓW - GOOGLE   ####################
################################################################################
#Pilk zawiera definicje funkcji (oraz funkcji pomocniczych) która zlicza wskaźniki
#widoczności kandydatów na portalu informacyjnym google'a.
#
#Sposób obliczania wskaźnika „średniego” wyświetlenia na Google :
#Dla danego okresu sumuję ilość wystąpień danego nazwiska i dzielę to
# przez suma wystąpień wszystkich nazwisk
#
#Sposób obliczania wskaźnika „średniego” wyświetlenia na Google :
#Zwracam sumę wyświetleń  danego nazwiska na portalu dla danego okresu czasu
#
library(stringi)
library(dplyr)

#Ścieżka potrzebna jedynie do przykładów, które są pod funkcjami
#sciezka<-"D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/"

#Potrzebne wielkości - nazwy kandydatów i tokeny
kandydaci<-readLines(paste0(sciezka,"slownik_fb_kandydaci.txt"))
tokeny <- readLines(paste0(sciezka,"slownik_google_tokeny.txt"))

AnalizaPlik <- function(sciezka.plik){
  #Funkcja obliczajaca wskaźnik dla jednego czasu odczytu zliczeń z serwisu Google
  wystapienia <- character(0)
  try(wystapienia <- read.table(sciezka.plik,h=TRUE,encoding="UTF-8"), silent=TRUE)

  wskaznik <- numeric(length(kandydaci))
  names(wskaznik) <- kandydaci
  if(length(wystapienia)==0) return(wskaznik)

  #Poprawa struktury tabeli by spelniala wymagania wyjsciowego wektora
  imiona <- names(wystapienia)

  for(i in 1:ncol(wystapienia)){
    ktory <- which(stri_detect_regex(kandydaci, imiona[i]))
    wskaznik[ktory] <- wystapienia[[i]]
  }

  wskaznik

}

WskaznikGoogleNews <- function(okres, wzor, typ="zwykly"){

  #Zmieniamy scieżkę i sprawdzamy jakie mamy tam pliki
  sciezka <- paste0(sciezka, "GoogleNews")
  pliki <- list.files(sciezka, full.names=TRUE)

  ktore <- character(0)
  #Wybieramy pasujące do wzoru foldery
  if (okres == "dzien"){
    ktore <- which(stri_detect_regex(pliki, wzor))
  } else if (okres == "tydzien"){
    #zapamietujemy interesujący nas numer tygodnia
    nr_tyg <- unlist(stri_extract_all_regex(wzor, "(?<=-).+$"))
    #wyciągamy z plików informację, który tydzień opisują
    daty <- unlist(stri_extract_first_regex(
      list.files(sciezka,full.names=FALSE), "(?<=News)[^ ]+"))
    daty <- as.POSIXlt(daty)
    tygodnie <- ceiling((daty$yday + 4) / 7)
    #wybieramy pasujące do wzoru foldery
    ktore <- which(tygodnie == nr_tyg)
  } else if (okres == "miesiac"){
    #wybieramy pasujące do wzoru pliki i je wczytujemy
    ktore <- which(stri_detect_regex(pliki, wzor))
  }

  #Analizujemy interesujące nas pliki
  wskaznik <- numeric(length(kandydaci))
  names(wskaznik) <- kandydaci
  if(length(ktore) == 0){return(wskaznik)}

  for(i in seq_along(ktore)){
    wskaznik <- AnalizaPlik(pliki[ktore[i]]) + wskaznik
  }

  if(typ == "sredni"){
    suma <- sum(wskaznik)
    wskaznik <- wskaznik/suma
  }

  wskaznik

}


# #PRZYKŁADY
# AnalizaPlik(paste0(sciezka,"GoogleNews/GoogleNews2015-03-15 00-16-02.txt"))
# AnalizaPlik(paste0(sciezka,"GoogleNews/GoogleNews2015-03-16 23-46-36.txt"))
# WskaznikGoogleNews("dzien", "2015-03-23")
# WskaznikGoogleNews("tydzien", "2015-12")
# WskaznikGoogleNews("miesiac", "2015-03")
# WskaznikGoogleNews("dzien", "2015-03-23", "sredni")
# WskaznikGoogleNews("tydzien", "2015-12", "sredni")
# WskaznikGoogleNews("miesiac", "2015-04", "sredni")
