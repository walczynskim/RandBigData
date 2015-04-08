################################################################################
######    WSKAŹNIK WIDOCZNOŚCI KANDYDATÓW - PORTALE INTERENTOWE    ##########
################################################################################
#Pilk zawiera definicje funkcji (oraz funkcji pomocniczych) która zlicza wskaźnik
#widoczności kandydatów na portalach informacyjnych.
#Po podaniu portalu, okresu dla którego chcemy wskaźnik oraz wzorca okresu
#funkcja zczytuje pliki z danymi dotyczącymi tego okresu i analizuje
#poszczególne artykuły.
#Słownik:
#Słowa mają oceny w skali:
#   5 - bardzo pozytywny,
#   4 - pozytywny,
#   3 - neutralny,
#   2 - negatywny,
#   1 - bardzo negatywny
#(w myśl - nie ważne jak mówią - ważne by mówili)
#
#Sposób obliczania wskaźnika:
#Dla każdego artykułu oraz każdego kandydata przeprowadzamy analizę tekstową
# zdań, w których występuje nazwisko kandydata. (Gdy zdań nie odnaleziono
# wskaźnik wynosi 0). Sumujemy oceny słow razy ilość ich występowania i dzielimy
#wynik przez ilość ocenionych słow. Dostajemy wskażnik z przedziału [1,5]
#(Gdy brak słów do oceny - wstawiamy wskaźnik neutralny = 3)
#Następnie przemnażamy to przez rangę artykułu (te wyżej na stronie są bardziej
#widoczne) i sumujemy po wszystkich artykułach.
library(stringi)
library(dplyr)

#Ścieżka potrzebna jedynie do przykładów, które są pod funkcjami
#sciezka<-"D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/"

#Potrzebne wielkości - nazwy kandydatów, słownik i tokeny
kandydaci <- readLines(paste0(sciezka,"slownik_fb_kandydaci.txt"))

slownik <- read.table(paste0(sciezka,"slownikWydzwieku.csv"))
slownik <- slownik[ ,c(1,5)]
slownik[ ,1] <- stri_encode(slownik[ ,1], from="UTF8", to="cp1250")
slownik[ ,2] <- slownik[ ,2] + 3

tokeny <- readLines(paste0(sciezka,"slownik_google_tokeny.txt"))

AnalizaArtykul <- function(sciezka.artykul){
  #Funkcja obliczajaca wskaźnik dla jednego artykułu

  f <- file(sciezka.artykul, "r")
  text <- readLines(f)
  close(f)

  zdania <- stri_paste(text, collapse=" ") %>% unlist() %>%
    stri_split_boundaries(
      opts_brkiter=stri_opts_brkiter(type="sentence")) %>% unlist()

  wskaznik <- numeric(length(tokeny))

  #Dla kazdego kandydata
  for(i in seq_along(tokeny)){
    #Wyjmujemy zdania z jego nazwiskiem
    wykryj <- stri_detect(zdania, regex=tokeny[i])
    if (length(which(wykryj)) == 0){wskaznik[i] = 0;next}
    analiza <- paste(zdania[which(wykryj)], collapse=" ")
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

AnalizaArtykulyzFolderu <- function(sciezka.folder){
  pliki <- list.files(sciezka.folder, full.names=TRUE)
  n <- length(pliki)

  #Sumuje wskaźniki dla artykułów z odpowiednią dla nich rangą
  for(i in seq_along(pliki)){
    wskaznik.artykul <- AnalizaArtykul(pliki[i])
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

WskaznikArtykuly <- function(okres, wzor, serwis){

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
    wskaznik <- AnalizaArtykulyzFolderu(pliki[ktore[i]]) + wskaznik
  }

  wskaznik

}


# #PRZYKŁADY
# AnalizaArtykul(paste0(sciezka,"Interia/2015-03-15 21-23-54/1.txt"))
# AnalizaArtykulyzFolderu(paste0(sciezka,"Interia/2015-03-15 21-23-54"))
# WskaznikArtykuly("dzien", "2015-03-23", "Interia")
# WskaznikArtykuly("tydzien", "2015-15", "Interia")
# WskaznikArtykuly("miesiac", "2015-04", "Interia")
# WskaznikArtykuly("dzien", "2015-03-23", "WirtualnaPolska")
# WskaznikArtykuly("tydzien", "2015-12", "WirtualnaPolska")
# WskaznikArtykuly("miesiac", "2015-04", "WirtualnaPolska")
# WskaznikArtykuly("dzien", "2015-03-23", "Onet")
# WskaznikArtykuly("tydzien", "2015-12", "Onet")
# WskaznikArtykuly("miesiac", "2015-04", "Onet")
