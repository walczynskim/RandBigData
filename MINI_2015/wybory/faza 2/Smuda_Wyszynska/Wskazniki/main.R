################################################################################
########  FUNKCJA OBLICZAJĄCA WSZYSTKIE WSKAŹNIKI WIDOCZNOŚCI   ################
################################################################################
#Skrypt korzystający z funkcji pomocniczych oblicza wszystkie wskaźniki
#widoczności kandydatów i zapisuje je do odpowiednich folderów.

#Potrzebne wielkości
sciezka <- "D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/"
poczatek.zbiorki.danych <- "2015-03-14"
koniec.zbiorki.danych <- as.character(Sys.Date())
wskazniki <- c("IloscTweetow", "WydzwiekTweetow", "IloscPolubienFB",
               "SzybkoscPolubienFB", "IloscPostowFB", "SredniaLikePostFB",
               "SredniaKomentarzPostFB", "SredniaSharePostFB", "IloscGoogle",
               "SredniGoogle", "TytulyInteria", "TytulyOnet", "TytulyWP",
               "TytulyPortale", "ArtykulyInteria", "ArtykulyOnet", "ArtykulyWP",
               "ArtykulyPortale")
kandydaci <- readLines(paste0(sciezka,"slownik_fb_kandydaci.txt"))

####################### Funkcje i foldery pomocnicze  ##########################
source(paste0(sciezka,"Wskazniki/wybory_wsk_google.R"))
source(paste0(sciezka,"Wskazniki/wybory_wsk_portale_internetowe.R"))
source(paste0(sciezka,"Wskazniki/wybory_wsk_portale_internetowe_tytuly.R"))
source(paste0(sciezka,"Wskazniki/wybory_wsk_fb_likes.R"))
source(paste0(sciezka,"Wskazniki/wybory_wsk_fb_posty.R"))
source(paste0(sciezka,"Wskazniki/wybory_wsk_tw.R"))
source(paste0(sciezka,"Wskazniki/wybory_wsk_pomocnicze.R"))

suppressWarnings({
k <- try(dir.create(paste0(sciezka,"Wskazniki/Kandydaci")), silent=TRUE)
k <- try(lapply(kandydaci, function(i){
  dir.create(paste0(sciezka, "Wskazniki/Kandydaci/", i))
  dir.create(paste0(sciezka, "Wskazniki/Kandydaci/", i, "/dzien"))
  dir.create(paste0(sciezka, "Wskazniki/Kandydaci/", i, "/tydzien"))
  dir.create(paste0(sciezka, "Wskazniki/Kandydaci/", i, "/miesiac"))
  }), silent=TRUE)
remove(k) })

#######################  Brak powielania analizy   #############################
#Sprawdzam kiedy była robiona ostatnia analiza by jej nie powielać dla
#dni
max <- lapply(kandydaci, function(i){
  pliki <- list.files(paste0(sciezka,"Wskazniki/Kandydaci/",i,"/dzien"),
                      full.names=FALSE)
  if(length(pliki) == 0) return (poczatek.zbiorki.danych)

  daty <- unlist(stri_match_all_regex(pliki, ".+(?=[.]txt)"))
  max(daty)
})
analiza.dni <- min(unlist(max))
#tygodni
analiza.tygodnie <- ceiling((as.POSIXlt(poczatek.zbiorki.danych)$yday + 4) / 7)
max <- lapply(kandydaci, function(i){
  pliki <- list.files(paste0(sciezka,"Wskazniki/Kandydaci/",i,"/tydzien"),
                      full.names=FALSE)
  if(length(pliki) == 0) return (analiza.tygodnie)

  daty <- unlist(stri_match_all_regex(pliki, "(?<=-).+(?=[.]txt)"))
  max(daty)
})
analiza.tygodnie <- paste0(stri_sub(poczatek.zbiorki.danych,1,4),
                           "-", min(unlist(max)))
#miesięcy
analiza.miesiace <- strftime(as.POSIXlt(poczatek.zbiorki.danych), "%m")
max <- lapply(kandydaci, function(i){
  pliki <- list.files(paste0(sciezka,"Wskazniki/Kandydaci/",i,"/miesiac"),
                      full.names=FALSE)
  if(length(pliki) == 0) return (analiza.miesiace)

  daty <- unlist(stri_match_all_regex(pliki, "(?<=-).+(?=[.]txt)"))
  max(daty)
})
analiza.miesiace <- paste0(stri_sub(poczatek.zbiorki.danych,1,4),
                           "-", min(unlist(max)))

########################  Obliczenie wskaznikow   ##############################
Wskazniki <- function(okres, wzor){
  #Funkcja obliczająca i zapisująca zestaw wskaźników dla wszystkich kandydatów

  #IloscTweetow
  IloscTweetow <- wsk_tw_liczba_wystapien(okres, wzor)
  #WydzwiekTweetow
  WydzwiekTweetow <- WskaznikAnalizaTweety(okres, wzor)
  #IloscPolubienFB
  IloscPolubienFB <- wsk_fb_likes(okres, wzor)
  #SzybkoscPolubienFB
  SzybkoscPolubien <- wsk_fb_wzrost_likes(okres, wzor)
  #IloscPostowFB
  IloscPostowFB <- wsk_fb_liczba_postow(okres, wzor)
  #SredniaLikePostFB
  SredniaLikePostFB <- wsk_fb_liczba_like_post(okres, wzor)
  #SredniaKomentarzPostFB
  SredniaKomentarzFB <- wsk_fb_liczba_komentarz_post(okres, wzor)
  #SredniaSharePostFB
  SredniaSharePostFB <- wsk_fb_liczba_udostepnienie_post(okres, wzor)
  #IloscGoogle
  IloscGoogle <- WskaznikGoogleNews(okres, wzor)
  #SredniGoogle
  SredniGoogle <- WskaznikGoogleNews(okres, wzor, typ="sredni")
  #TytulyInteria
  TytulyInteria <- WskaznikTytuly(okres, wzor, "Interia")
  #TytulyOnet
  TytulyOnet <- WskaznikTytuly(okres, wzor, "Onet")
  #TytulyWP
  TytulyWP <- WskaznikTytuly(okres, wzor, "WirtualnaPolska")
  #TytulyPortale
  TytulyPortale <- TytulyInteria + TytulyOnet + TytulyWP
  #ArtykulyInteria
  ArtykulyInteria <- WskaznikArtykuly(okres, wzor, "Interia")
  #ArtykulyOnet
  ArtykulyOnet <- WskaznikArtykuly(okres, wzor, "Onet")
  #ArtykulyWP
  ArtykulyWP <- WskaznikArtykuly(okres, wzor, "WirtualnaPolska")
  #ArtykulyPortale
  ArtykulyPortale <- ArtykulyInteria + ArtykulyOnet + ArtykulyWP

  lapply(kandydaci, function(i){
    wsk <- c(IloscTweetow[i], WydzwiekTweetow[i], IloscPolubienFB[i],
             SzybkoscPolubien[i], IloscPostowFB[i], SredniaLikePostFB[i],
             SredniaKomentarzFB[i], SredniaSharePostFB[i],
             IloscGoogle[i], SredniGoogle[i], TytulyInteria[i], TytulyOnet[i],
             TytulyWP[i], TytulyPortale[i], ArtykulyInteria[i], ArtykulyOnet[i],
             ArtykulyWP[i], ArtykulyPortale[i])
    wsk <- round(wsk,2)

    plik <- paste0(sciezka,"Wskazniki/Kandydaci/",i,"/",okres,"/",wzor,".txt")
    file.create(plik)
    f <- file(plik, "w+")
    writeLines(paste(wskazniki, collapse=";"), f)
    writeLines(paste(wsk, collapse=";"), f)
    close(f)
  })

  invisible(NULL)
}

#Wzorce dla analiz
print(c(analiza.dni, analiza.tygodnie, analiza.miesiace))

#Analiza dla dni
wzorce <- generuj_date("dzien", analiza.dni)
k <- lapply(wzorce, Wskazniki, okres="dzien")
#Anliza dla tygodni
wzorce <- generuj_date("tydzien", analiza.tygodnie)
k <- lapply(wzorce, Wskazniki, okres="tydzien")
#Analiza dla miesięcy
wzorce <- generuj_date("miesiac", analiza.miesiace)
k <- lapply(wzorce, Wskazniki, okres="miesiac")

remove(k)



