#' Funkcja wyznacza wskaznik podobienstwa miedzy dwiema wartosciami rozrzutu
#'
#' Funkcja \code{porownaj_rozrzut} wyznacza wskaznik podobienstwa miedzy dwiema
#' wartosciami rozrzutu (w domysle chodzi o rozrzut wieku badanej grupy osob)
#'
#' @usage porownaj_rozrzut(r1, r2)
#' @param r1 liczba naturalna - rozrzut pierwszej grupy badanych osob
#' @param r2 liczba naturalna - rozrzut drugiej grupy badanych osob
#'
#' @return
#' wartosc numeryczna z przedzialu [0,1] - podobiestwo wartosci rozrzutu w grupie wiekowej
#'
#'
#'@author Emilia Momotko
#'

porownaj_rozrzut <- function(r1, r2){

  (r1+r2-abs(r1-r2))/(r1+r2)

}
