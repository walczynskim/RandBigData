#' Funkcja do ramki danych filmy dodaje kolumny zwiazanie z odpowiednimi wskaznikami
#' zwiazanymi z obsada i rezyseria
#'
#' Funkcja \code{dodaj_aktorzy_rezyser} do ramki danych filmy dodaje kolumny zwiazanie z odpowiednimi wskaznikami
#' zwiazanymi z obsada i rezyseria (rozrzut wieku, mediane wieku, kraje pochodzenia)
#'
#' @usage dodaj_aktorzy_rezyser(filmy, aktorzy, rezyserzy)
#' @param filmy ramka danych z filmami
#' @param aktorzy ramka danych z aktorami
#' @param rezyserzy ramka danych z rezyserami
#'
#' @return
#' ramka danych z dodanymi kolumnami
#
#'
#'@author Emilia Momotko
#'
#'@import stringi
#'

dodaj_aktorzy_rezyser <- function(filmy, aktorzy, rezyserzy){

  #tworzymy ramke danych na ktorej bedziemy operowac
  ramka <- cbind(filmy, aktorzy_wiek=0,aktorzy_rozrzut=0,aktorzy_kraje = NA,rezyser_wiek=0,
                 rezyser_kraje=NA)
  n <- nrow(filmy)

  #dodajemy odpowiednie wartosci (wyszukujemy info z tabel aktorzy i rezyserzy),
  #przeksztalcamy dane i wstawiamy do ramki

  for(i in 1:n){

    #wyszukujemy aktorow
    ktorzy <- aktorzy_wyszukaj_wiek_kraj(filmy[i,],aktorzy)

    #mediana
    ramka[i,24] <- median(ktorzy[[1]],na.rm=TRUE)

    #rozstep miedzykwartylowy
    ramka[i,25] <- IQR(ktorzy[[1]],na.rm=TRUE)

    #pochodzenie
    kraje <- ktorzy[[2]][which(ktorzy[[2]]!="NA")]
    kraje <- na.omit(kraje)
    if(length(kraje)>0){
      ramka[i,26] <- stri_flatten(na.omit(ktorzy[[2]]),collapse="@")
    }

    #to damo dla rezyserow
    rez <- rezyser_wyszukaj_wiek_kraj(filmy[i,],rezyserzy)
    kraje <- rez[[2]][which(rez[[2]]!="NA")]
    kraje <- na.omit(kraje)
    ramka[i,27]<-median(rez[[1]],na.rm=TRUE)
    if(length(kraje)>0){
      ramka[i,28] <- stri_flatten(na.omit(rez[[2]]),collapse="@")
    }


  }

  #zwracamy ramke
  ramka

}
