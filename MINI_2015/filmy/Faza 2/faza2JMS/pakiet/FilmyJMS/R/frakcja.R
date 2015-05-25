#' Funkcja wyznacza wskaznik podobienstwa miedzy dwoma wartosciami tekstowymi
#'
#' Funkcja \code{frakcja} wskaznik podobienstwa miedzy dwoma wartosciami tekstowymi
#' przy zalozeniu ze slowa lub wyrazenia z jednego tekstu sa odseparowane 'malpa'
#'
#' @usage frakcja(a1, a2)
#' @param a1 tekst pierwszy, typu character (wyrazenia rozdzielone 'malpa')
#' @param a2 tekst drugi, typu character (wyrazenia rozdzielone 'malpa')
#'
#' @return
#' wartosc numeryczna z przedzialu [0,1] - podobiestwo obu tekstow
#'
#'
#'@author Emilia Momotko
#'
#'@import stringi

frakcja <- function(a1, a2){

  #wydobywam poszczegolne elementy tekstu
  a1 <- unlist(stri_split_fixed(a1,"@"))
  a2 <- unlist(stri_split_fixed(a2,"@"))

  #zliczam wystepowanie tych elementow i wyrzam "NA"
  t1 <- table(a1)
  t1 <- t1[names(t1)!="NA"]
  t2 <- table(a2)
  t2 <- t2[names(t2)!="NA"]

  #biore czesc wspolna, ktora bede zliczac
  p <- intersect(names(t1),names(t2))

  #teraz dla kazdego elementu z tych co sie powtarzaja w obu filmach wybieram
  #minimum z liczby wystapien w filmie - to slowo pojawilo sie tyle razy w jednym i
  #przynajmniej tyle razy w drugim filmie
  #biore razy dwa bo liczy sie to ze wystepuje w obu filmach
  ile <- sapply(p, function(x){

    n1 <- which(names(t1)==x)
    n2 <- which(names(t2)==x)

    2*min(t1[n1],t2[n2])

  })

  #sumuje uzyskane wartosci powtorzen i dziele przez calkowita sume pojawien sie slow
  sum(unlist(ile))/sum(c(t1,t2))
}
