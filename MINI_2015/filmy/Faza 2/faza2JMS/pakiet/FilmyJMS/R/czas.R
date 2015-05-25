#' Funkcja sprawdza czy dwie wartosci liczbowe sa "podobne"
#'
#' Funkcja \code{czas} sprawdza czy dwie wartosci liczbowe sa "podobne" (w domysle
#' chodzi o czas trwania filmu)
#'
#' @usage czas(czas_glowny, czas_por)
#' @param czas_glowny pierwsza wartosc numeryczna, liczba naturalna
#' @param czas_por druga wartosc numeryczna, liczba naturalna
#'
#' @return
#' wartosc numeryczna 1, jesli istnieje istotne podobienstwo miedzy wartosciami, 0 wpp
#'
#'
#'@author Emilia Momotko
#'

czas <- function(czas_glowny, czas_por){

  #czy nie ma brakow danych
  if(is.na(czas_glowny)||is.na(czas_por)){

    return(NA)

  }

  #wyznaczamy czy istnieje istotne podobienstwo
  if(czas_glowny<60){

    if(czas_por<60){
      return(1)
    } else return(0)
  } else if(czas_glowny<90){

    if(czas_por>=60&&czas_por<90){
      return(1)
    } else return(0)

  } else if(czas_glowny<140){

    if(czas_por>=90&&czas_por<140){
      return(1)
    } else return(0)
  } else if(czas_glowny<180){
    if(czas_por>=140&&czas_por<180){
      return(1)
    } else return(0)

  } else if(czas_glowny<240){

    if(czas_por>=180&&czas_por<240){
      return(1)
    } else return(0)

  } else if(czas_glowny>=240&&czas_por>=140) return(1)

  return(0)

}
