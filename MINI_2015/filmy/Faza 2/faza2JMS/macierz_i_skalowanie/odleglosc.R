#' Funkcja zlicza odleglosc (podobienstwo) miedzy jednym a drugim filmem
#'
#' Funkcja \code{odleglosc} zlicza wartosci podobienstw meidzy dwoma zadanymi filmami.
#' Podobienstwo to jest wyznaczane na podstawie wartosci wskaznikow: roku powstania,
#' gatunkow, kraju produkcji, czasu trwania, autorow muzyki, producentow, aktorow, rezysera,
#' ocen, liczby oceniajacych uzytkownikow, liczby recenzji, tresci recenzji, slow kluczowych,
#' tresci opisu, tresci fabuly, liczby oscarow, liczby innych nagrod, mediany wieku aktorow,
#' zroznicowania wieku aktorow, wieku rezysera, kraju pochodzenia rezysera i aktorow.
#' 
#' @usage odleglosc(f_glowny, f_por, filmy)
#' @param f_glowny - film glowny, czyli jednowierszowa ramka danych zawierajaca odpowiednie info
#' @param f_por - film porownywany, czyli jednowierszowa ramka danych zawierajaca odpowiednie info
#' @param filmy - ramka danych z filmami
#'
#' @return
#' wartosc numeryczna z przedzialu [0,1] - podobienstwo miedzy filmami
#' 
#' @example
#' filmy <- read.csv2("filmy24.csv",stringsAsFactors=FALSE)
#' m <- odleglosc(filmy[1,], filmy[2,], filmy)
#'
#'@import stringi
#'
#'@author Emilia Momotko
#'

odleglosc <- function(f_glowny, f_por, filmy){
  
  #rok powstania
  rok_powstania <- rok(f_glowny[1,4],f_por[1,4])
  
  #gatunki
  gatunki <- frakcja(f_glowny[1,7],f_por[1,7])
  
  #kraje
  kraje <- czy_zawiera(f_glowny[1,10],f_por[1,10])
  
  #muzyka
  muzyka <- czy_zawiera(f_glowny[1,20],f_por[1,20])
  
  #producenci
  producent <- czy_zawiera(f_glowny[1,21],f_por[1,21])
  
  #aktorzy - zawieranie
  aktorzy_zaw <- frakcja(f_glowny[1,22],f_por[1,22])
  
  #rezyser - zawieranie
  rezyser <- frakcja(f_glowny[1,23],f_por[1,23])
  
  #oceny, uzytkownicy i recenzje
  oceny <- roznica(f_glowny[1,11],f_por[1,11],0.3)
  uzytkownicy <- roznica(f_glowny[1,12],f_por[1,12],50000)
  recenzje <- roznica(f_glowny[1,13],f_por[1,13],20)
  
  #tresci recenzji:
  tresci_recenzji <- frakcja_powtarzanych_slow(f_glowny[1,29],f_por[1,29])
  
  #klucze - slowa
  key_p <-  stri_split_fixed(f_por[1,15],'@')
  key_g <- stri_split_fixed(f_glowny[1,15],'@')
  key <- frakcja_powtarzanych_slow(key_g,key_p)
  
  #nagrody
  oscar <- czy_to_samo(f_glowny[1,17],f_por[1,17])
  #inne narody:
  nagrody <- czy_to_samo(f_glowny[1,18],f_por[1,18])
  
  #czas trwania
  cz <- czas(as.numeric(f_glowny[1,5]),as.numeric(f_por[1,5]))
  
  #porownanie opisu
  frakcje_opis <- frakcja_powtarzanych_slow(f_glowny[1,8],f_por[1,8])
  
  #porownanie fabuly
  frakcje_fabula <- frakcja_powtarzanych_slow(f_glowny[1,16],f_por[1,16])
  
  #median, rozrzut wieku, pochodzenie aktorow i rezysera
  mediana_aktorzy <- porownaj_mediany(f_glowny[1,24],f_por[1,24])
  rozrzut_aktorzy <- porownaj_rozrzut(f_glowny[1,25],f_por[1,25])
  mediana_rezyser <- porownaj_mediany(f_glowny[1,27],f_por[1,27])
  aktor_kraje <- frakcja(f_glowny[1,26],f_por[1,26])
  rezyser_kraje <- frakcja(f_glowny[1,28],f_por[1,28])
  
  #wektor wskaznikow
  
  waga <- 30
  
  #zwracane podobienstwo
  sum(c(cz, frakcje_opis,frakcje_fabula, mediana_aktorzy,rozrzut_aktorzy,
        mediana_rezyser,aktor_kraje,rezyser_kraje,rok_powstania,4*gatunki,
        kraje,muzyka,producent,aktorzy_zaw,3*rezyser,
        oceny, uzytkownicy, recenzje,4*key,oscar,nagrody,tresci_recenzji),na.rm=TRUE)/waga
    
}

## dla Justyny
# 
# filmy <- read.csv2("filmy24.csv",stringsAsFactors=FALSE)
# 
# m <- tworz_macierz(ile = 100, filmy=filmy)
# 
# save(m ,file = "macierz.RData")
# 
# load("macierz.RData")