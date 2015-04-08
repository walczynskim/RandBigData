################################################################################
####   WSKAŹNIK WIDOCZNOŚCI KANDYDATÓW - PORTALE INTERENTOWE (TYTUŁY)  #########
################################################################################
#Pilk zawiera definicje funkcji (oraz funkcji pomocniczych) która zlicza wskaźnik
#widoczności kandydatów w tytułach artykułów na portalach informacyjnych.
#Po podaniu portalu, okresu dla którego chcemy wskaźnik oraz wzorca okresu
#funkcja zczytuje pliki z danymi dotyczącymi tego okresu i analizuje
#poszczególne tytuły artykułów.
#
#Sposób obliczania wskaźnika:
#Zliczam ilość wystąpień nazwiska kandydata w tytułach
#artykułów i mnożę to przez wagę artykułu

library(stringi)
library(dplyr)

#Ścieżka potrzebna jedynie do przykładów, które są pod funkcjami
#sciezka<-"D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/"

#Potrzebne wielkości - nazwy kandydatów i tokeny
kandydaci <- readLines(paste0(sciezka,"slownik_fb_kandydaci.txt"))
tokeny <- readLines(paste0(sciezka,"slownik_google_tokeny.txt"))

AnalizaTytul <- function(sciezka.artykul){
  #Funkcja obliczajaca wskaźnik dla jednego tytułu

  f <- file(sciezka.artykul, "r")
  text <- readLines(f,n=2)
  close(f)

  slowa <- stri_paste(text, collapse=" ") %>% unlist() %>%
    stri_split_boundaries(
      opts_brkiter=stri_opts_brkiter(type="word")) %>% unlist()

  wskaznik <- numeric(length(tokeny))

  #Dla kazdego kandydata
  for(i in seq_along(tokeny)){
    #Zliczamy ilosc wystapien nazwiska
    wykryj <- stri_detect(slowa, regex=tokeny[i])
    wskaznik[i] <- sum(wykryj)
  }

  names(wskaznik) <- kandydaci
  wskaznik

}

AnalizaTytulowzFolderu <- function(sciezka.folder){
  pliki <- list.files(sciezka.folder, full.names=TRUE)
  n <- length(pliki)

  #Sumuje wskaźniki dla artykułów z odpowiednią dla nich rangą
  for(i in seq_along(pliki)){
    wskaznik.artykul <- AnalizaTytul(pliki[i])
    waga <- stri_extract_last_regex(pliki[i], "[^//]{1,2}(?=[.]txt)") %>%
      unlist() %>% as.integer()
    ranga <- (n-waga+1)/n
    if(i==1){
      wskaznik <- wskaznik.artykul*ranga
    }else{
      wskaznik <- wskaznik + wskaznik.artykul*ranga}
  }

  wskaznik

}

WskaznikTytuly <- function(okres, wzor, serwis){

  #Zmieniamy scieżkę i sprawdzamy jakie mamy tam pliki
  sciezka <- paste0(sciezka, serwis)
  pliki <- list.dirs(sciezka, full.names=TRUE)
  pliki <- pliki[-1]

  ktore <- character(0)
  #Wybieramy pasujące do wzoru foldery
  if (okres == "dzien"){

    ktore <- which(stri_detect_regex(pliki, wzor))

  } else if (okres == "tydzien"){
    #zapamietujemy interesujący nas numer tygodnia
    nr_tyg <- unlist(stri_extract_all_regex(wzor, "(?<=-).+$"))
    #wyciągamy z plików informację, który tydzień opisują
    daty <- unlist(stri_extract_first_regex(list.dirs(sciezka,full.names=FALSE), "[^ ]+"))
    daty <- daty[-1]
    daty <- as.POSIXlt(daty)
    tygodnie <- ceiling((daty$yday + 4) / 7)
    #wybieramy pasujące do wzoru foldery
    ktore <- which(tygodnie == nr_tyg)
  } else if (okres == "miesiac"){
    #wybieramy pasujące do wzoru pliki i je wczytujemy
    ktore <- which(stri_detect_regex(pliki, wzor))
  }

  #Analizujemy interesujące nas foldery
  wskaznik <- numeric(length(kandydaci))
  names(wskaznik) <- kandydaci
  if(length(ktore) == 0){return(wskaznik)}

  for(i in seq_along(ktore)){
    wskaznik <- AnalizaTytulowzFolderu(pliki[ktore[i]]) + wskaznik
  }

  wskaznik

}


# #PRZYKŁADY
# AnalizaTytul(paste0(sciezka,"Interia/2015-03-15 21-23-54/1.txt"))
# AnalizaTytulowzFolderu(paste0(sciezka,"Interia/2015-03-15 21-23-54"))
# WskaznikTytuly("dzien", "2015-03-23", "Interia")
# WskaznikTytuly("tydzien", "2015-11", "Interia")
# WskaznikTytuly("miesiac", "2015-04", "Interia")
# WskaznikTytuly("dzien", "2015-03-23", "WirtualnaPolska")
# WskaznikTytuly("tydzien", "2015-12", "WirtualnaPolska")
# WskaznikTytuly("miesiac", "2015-04", "WirtualnaPolska")
# WskaznikTytuly("dzien", "2015-03-23", "Onet")
# WskaznikTytuly("tydzien", "2015-12", "Onet")
# WskaznikTytuly("miesiac", "2015-04", "Onet")
